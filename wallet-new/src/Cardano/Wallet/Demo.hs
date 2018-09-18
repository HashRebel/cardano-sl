{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Demo
    (
    -- * NodeId
    NodeId(..)

    -- * Start a cluster of wallet nodes
    , startCluster
    , waitForNode

    -- Temp
    , createDefaultTopology
    ) where

import           Universum hiding (takeWhile)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async)
import           Data.Attoparsec.ByteString.Char8 (IResult (..), parse,
                     skipWhile, string, takeWhile)
import qualified Data.Attoparsec.Internal.Types as Atto.Internal
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import           Data.IP (IP (IPv4))
import           Data.List (elemIndex, stripPrefix)
import           Data.Maybe (fromJust)
import           Options.Applicative (ParseError (ShowHelpText), Parser,
                     ParserHelp (..), ParserInfo, defaultPrefs, execFailure,
                     execParser, info, parserFailure)
import           Options.Applicative.Help.Chunk (Chunk (..))
import           System.Environment (getEnv, getEnvironment, lookupEnv, setEnv,
                     withArgs)
import           System.IO.Temp (withSystemTempDirectory)
import qualified Text.Parsec as Parsec

import           Cardano.Wallet.Action (actionWithWallet)
import           Cardano.Wallet.API.V1.Types (ForceNtpCheck (..))
import           Cardano.Wallet.Client (ClientError (..), ServantError (..),
                     WalletClient (getNodeInfo))
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     walletBackendParamsParser)
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs, NodeArgs,
                     commonNodeArgsParser, nodeArgsParser)
import           Pos.Client.CLI.Params (loggingParams)
import           Pos.Core.NetworkAddress (NetworkAddress, addrParser)
import           Pos.Infra.Network.Yaml (AllStaticallyKnownPeers (..),
                     NodeMetadata (..), NodeRegion (..), NodeRoutes (..))
import           Pos.Launcher (LoggingParams (..), launchNode)
import           Pos.Util.CompileInfo (withCompileInfo)

-- | Type alias on a string to identify a NodeId
newtype NodeId = NodeId String deriving (Eq, Show)


-- | Start a cluster of wallet nodes in different thread with the given NodeIds.
-- Node gets their argument from the ENVironment.
startCluster :: String -> [NodeId] -> IO [Async ()]
startCluster prefix nodes = do
    lock <- newMVar 0
    let indexedNodes = zip nodes (iterate (+1) 0)
    forM indexedNodes $ \(nodeId, i) -> async $ withStateDirectory prefix $ \stateDir -> do
        (nArgs, cArgs, lArgs, wOpts) <-
            oneByOne lock i $ parseNodeArguments prefix stateDir (nodeId, i)
        withCompileInfo $
            launchNode nArgs cArgs lArgs (actionWithWallet wOpts)


-- | Temporary Working / State Directory, use {prefix}STATE_DIR as a directory
-- if it's given, otherwise, create a new system-level temp directory.
withStateDirectory
    :: String
    -> (FilePath -> IO a)
    -> IO a
withStateDirectory prefix cb = do
    stateDir <- lookupEnv (prefix <> "STATE_DIR")
    case stateDir of
        Nothing ->
            withSystemTempDirectory "cardano-sl-wallet:demo" cb

        Just dir ->
            cb dir


-- | Synchronization function between nodes, to make sure they only run
-- the given action one after the other. The given `Word16` being their
-- 'ticket number'.
oneByOne
    :: MVar Word16
    -> Word16
    -> IO a
    -> IO a
oneByOne lock i action = do
    i' <- takeMVar lock
    if i == i' then do
        a <- action
        putMVar lock (i+1)
        return a
    else do
        putMVar lock i'
        oneByOne lock i action


-- | Parse node arguments from the CLI. Node that
parseNodeArguments
    :: String
    -> FilePath
    -> (NodeId, Word16)
    -> IO (NodeArgs, CommonNodeArgs, LoggingParams, NewWalletBackendParams)
parseNodeArguments prefix stateDir (NodeId nodeId, i) = do
    let p = (<>) prefix

    -- Variables below are treated a bit differently because they're different
    -- for each node. We use the initial ENV var as a starting point and then
    -- adjust the variable's value in function of the current node, incrementing
    -- between each step.
    (p "DB_PATH")            ^?~ (stateDir <> "/db/")
    (p "WALLET_DB_PATH")     ^?~ (stateDir <> "/wallet-db/")
    (p "LOG_CONFIG")         ^?~ (stateDir <> "/logs/")
    (p "LISTEN")             ^?~ "127.0.0.1:3000"
    (p "WALLET_ADDRESS")     ^?~ "127.0.0.1:8090"

    withRestorableEnv $ do
        -- Save their current values to restore them later.
        dbPath       <- getEnv             (p "DB_PATH")
        walletDbPath <- getEnv             (p "WALLET_DB_PATH")
        logConfig    <- getEnv             (p "LOG_CONFIG")
        addr         <- unsafeGetNtwrkAddr (p "LISTEN")
        waddr        <- unsafeGetNtwrkAddr (p "WALLET_ADDRESS")

        -- TODO Generate Topology
        -- TODO Generate TLS Certs

        (p "DB_PATH")            ^.~ dbPath <> nodeId
        (p "WALLET_DB_PATH")     ^.~ walletDbPath <> nodeId
        (p "LOG_CONFIG")         ^.~ logConfig <> nodeId <> ".yaml"
        (p "LISTEN")             ^.~ ntwrkAddrToENV (nextNtwrkAddr i addr)
        (p "WALLET_ADDRESS")     ^.~ ntwrkAddrToENV (nextNtwrkAddr i waddr)
        (p "WALLET_DOC_ADDRESS") ^.~ ntwrkAddrToENV (nextNtwrkAddr (i+100) waddr)
        (p "NODE_ID")            ^.~ nodeId
        (p "TOPOLOGY")           ^.~ (stateDir <> "/topology/" <> nodeId <> ".yaml")
        (p "TLSCERT")            ^.~ (stateDir <> "/tls/" <> nodeId <> ".crt")
        (p "TLSKEY")             ^.~ (stateDir <> "/tls/" <> nodeId <> ".key")
        (p "TLSCA")              ^.~ (stateDir <> "/tls/ca.crt")
        (p "REBUILD_DB")         ^?~ "True"
        (p "WALLET_REBUILD_DB")  ^?~ "True"
        (p "CONFIGURATION_FILE") ^?~ "../lib/configuration.yaml"
        (p "CONFIGURATION_KEY")  ^?~ "default"
        (p "SYSTEM_START")       ^?~ "0"

        let cVars = varFromParser commonNodeArgsParser prefix
        let nVars = varFromParser nodeArgsParser prefix
        let wVars = varFromParser walletBackendParamsParser prefix

        cArgs <- execParserEnv cVars prefix (info commonNodeArgsParser mempty)
        nArgs <- execParserEnv nVars prefix (info nodeArgsParser mempty)
        wArgs <- execParserEnv wVars prefix (info walletBackendParamsParser mempty)
        let lArgs = (loggingParams (fromString nodeId) cArgs) { lpConsoleLog = Just False }
        let wOpts = NewWalletBackendParams wArgs

        return (nArgs, cArgs, lArgs, wOpts)


-- | Create a default topology file structure for the given nodes associated
-- with their corresponding network addresses
createDefaultTopology :: [(NodeId, NetworkAddress)] -> Text
createDefaultTopology nodes =
    let
        perms =
            filter ((== (length nodes - 1)) . length) . subsequences

        mkNodeMetadata (nodeId, (addr, port)) routes = NodeMetdata
            { nmType       = "core"
            , nmRegion     = NodeRegion "undefined"
            , nmRoutes     = routes
            , nmSubscribe  = DnsDomains []
            , nmValency    = 1
            , nmFallbacks  = 1
            , nmAddress    = NodeAddrExact (IPv4 $ read addr) (Just port)
            , nmKademlia   = False
            , nmPublicDNS  = False
            , nmMaxSubscrs = 0
            }
    in
        ""


    -- AllStaticallyKnownPeers



-- | Make HttpRequest continuously to wait after the node
waitForNode :: WalletClient IO -> IO ()
waitForNode wc = do
    resp <- getNodeInfo wc NoNtpCheck
    case resp of
        Right _ ->
            return ()

        Left (ClientHttpError ConnectionError{}) ->
            let
                oneSecond = 1000000
            in
                threadDelay oneSecond  >> waitForNode wc

        Left err ->
            fail (show err)


--
-- Manipulate the ENVironment
--

-- | Define a default ENV var if it doesn't exist
(^?~) :: String -> String -> IO ()
var ^?~ value =
    setEnv var =<< (fromMaybe value <$> lookupEnv var)
infix 4 ^?~

-- | Set a variable in the ENV. It overwrites it if it exists
(^.~) :: String -> String -> IO ()
var ^.~ value =
    setEnv var value
infix 4 ^.~

withRestorableEnv :: IO a -> IO a
withRestorableEnv action =  do
    env <- getEnvironment
    a <- action
    foldMap (uncurry setEnv) env
    return a


--
-- NetworkAddress manipulations
--

-- | (unsafe) Parse a NetworkAddress from an ENV var
unsafeGetNtwrkAddr :: String -> IO NetworkAddress
unsafeGetNtwrkAddr =
    getEnv >=> (either (fail . show) return . Parsec.parse addrParser "" . toText)

-- | Get the next NetworkAddress given an index
nextNtwrkAddr :: Word16 -> NetworkAddress -> NetworkAddress
nextNtwrkAddr i (host, port) =
    (host, port + i)

-- | Convert a NetworkAddress to a string suitable for the Environment
ntwrkAddrToENV :: NetworkAddress -> String
ntwrkAddrToENV (host, port) =
    B8.unpack host <> ":" <> show port


--
-- From ENV var to CLI args
--
-- We want to comprehend any ENVironment variables as if they were CLI
-- arguments and flags, such that we can re-use code to parse them and build
-- **Args structure from them.
--
-- The tricky thing here is that ENVironment "flags" don't exist so to speak,
-- so we have to reflect on the opt-applicative@Parser to see whether we expect
-- a var to be an 'Arg' or 'Flag'. If we expect a 'Flag' then the underlying
-- ENV var should be set to 'True' or 'False'
--
-- This also makes sure that we don't forget any CLI args and that's why, we
-- need to setup a full ENVironment before we actually start parsing. If one
-- variable is missing, it will throw a great deal.

data ArgType = Arg | Flag deriving Show

-- | Extract the list of ENV var from a 'Options.Applicative.Parser'
varFromParser :: Parser a -> String -> [(String, ArgType)]
varFromParser parser pfx =
    map (first (pfx <>)) (foldParse [] (helpToByteString help))
  where
    -- Here is the little trick, we leverage the parserFailure which displays
    -- a usage with all possible arguments and flags and from this usage,
    -- we capture all arguments and flags as tuple (String, ArgType)
    (help, _, _) =
        execFailure (parserFailure defaultPrefs (info parser mempty) ShowHelpText mempty) ""

    helpToByteString :: ParserHelp -> ByteString
    helpToByteString =
        B8.pack . show . fromJust . unChunk . helpUsage

    -- | Convert a string argument to its corresponding ENV var
    argToVar :: String -> (String, ArgType)
    argToVar arg = case elemIndex ' ' arg of
        Nothing -> (kToS (drop 2 arg), Flag)
        Just i  -> (kToS (drop 2 (take i arg)), Arg)

    foldParse :: [(String, ArgType)] -> ByteString -> [(String, ArgType)]
    foldParse xs str = case parse (argToVar . B8.unpack <$> capture) str of
        Fail{}      -> xs
        Partial{}   -> xs
        Done rest x -> foldParse (x : xs) rest

    capture :: Atto.Internal.Parser ByteString ByteString
    capture =
        skipWhile (/= '[') *> string "[" *> takeWhile (/= ']') <* string "]"


-- | Run a parser from environment variables rather than command-line arguments
execParserEnv
    :: [(String, ArgType)] -- ^ The restricted environment
    -> String              -- ^ A prefix to remove from all Env variables
    -> ParserInfo a        -- ^ A corresponding CLI
    -> IO a
execParserEnv vars pfx pInfo = do
    args <- (mapMaybe varToArg . mapMaybe filterEnv) <$> getEnvironment
    withArgs args $ execParser pInfo
  where
    -- | Convert an environment variable to its argument, with value. Returns
    -- 'Nothing' when Flags are given and turned off. 'Just arg' otherwise.
    varToArg :: (String, ArgType, String) -> Maybe String
    varToArg = \case
        (key, Flag, "True") -> Just ("--" <> sToK key)
        (_, Flag, _)        -> Nothing
        (key, Arg, val)     -> Just ("--" <> sToK key <> "=" <> val)

    -- | Only consider ENV var that have the given prefix
    filterEnv :: (String, String) -> Maybe (String, ArgType, String)
    filterEnv (k, v) =
        (\(_, t) -> (unsafeStripPrefix k, t, v)) <$> find ((== k) . fst) vars

    -- | Remove a prefix, throw if there's no such prefix
    unsafeStripPrefix :: String -> String
    unsafeStripPrefix =
        fromJust . stripPrefix pfx


--
-- String Utils
--

-- | kebab-case to UPPER_SNAKE_CASE
kToS :: String -> String
kToS = map (Char.toUpper . replaceIf '-' '_')

-- | UPPER_SNAKE_CASE to kebab-case
sToK :: String -> String
sToK = map (Char.toLower . replaceIf '_' '-')

-- | Replace third argument by the second one if it matches the first one.
replaceIf :: Char -> Char -> Char -> Char
replaceIf want to x | x == want = to
replaceIf _ _ x     = x
