{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

-- | Definition of 'BlockchainHelpers' for the main blockchain.
--
-- FIXME rename this module to something to do with verification.

module Pos.Chain.Block.BHelpers
       ( verifyBlockHeader
       , verifyBlock
       , verifyGenesisBlock
       , verifyMainBlock
       , verifyMainBody
       , verifyMainBlockHeader
       , verifyMainConsensusData
       , verifyMainExtraHeaderData
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))

import           Pos.Binary.Class (Bi)
import           Pos.Chain.Block.Blockchain (Blockchain (..), GenericBlock (..),
                     GenericBlockHeader (..), gbExtra)
import           Pos.Chain.Block.Main (MainBody (..), MainExtraHeaderData (..),
                     MainProof)
import           Pos.Chain.Block.Union (Block, BlockHeader (..),
                     BlockSignature (..), GenesisBlockchain, MainBlockchain,
                     MainConsensusData (..), MainToSign (..),
                     mainBlockEBDataProof)
import           Pos.Chain.Delegation (LightDlgIndices (..), checkDlgPayload)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Ssc (checkSscPayload, verifySscPayload)
import           Pos.Chain.Txp (checkTxPayload)
import           Pos.Chain.Update (checkSoftwareVersion, checkUpdatePayload)
import           Pos.Core.Slotting (SlotId (..))
import           Pos.Crypto (ProtocolMagic, ProxySignature (..), SignTag (..),
                     checkSig, hash, isSelfSignedPsk, proxyVerify)
import           Pos.Util.Some (Some (Some))

-- | Verify a BlockHeader in isolation. There is nothing to be done for
-- genesis headers.
verifyBlockHeader
    :: MonadError Text m
    => ProtocolMagic
    -> BlockHeader
    -> m ()
verifyBlockHeader _ (BlockHeaderGenesis _) = pure ()
verifyBlockHeader pm (BlockHeaderMain bhm) = verifyMainBlockHeader pm bhm

-- | Verify a Block in isolation.
verifyBlock
    :: MonadError Text m
    => Genesis.Config
    -> Block
    -> m ()
verifyBlock genesisConfig = either verifyGenesisBlock (verifyMainBlock genesisConfig)

-- | To verify a genesis block we only have to check the body proof.
verifyGenesisBlock
    :: ( MonadError Text m )
    => GenericBlock GenesisBlockchain
    -> m ()
verifyGenesisBlock UnsafeGenericBlock {..} =
    checkBodyProof @GenesisBlockchain _gbBody (_gbhBodyProof _gbHeader)

verifyMainBlock
    :: ( MonadError Text m
       , Bi MainProof
       )
    => Genesis.Config
    -> GenericBlock MainBlockchain
    -> m ()
verifyMainBlock genesisConfig block@UnsafeGenericBlock {..} = do
    let pm = configProtocolMagic genesisConfig
    verifyMainBlockHeader pm _gbHeader
    verifyMainBody pm _gbBody
    -- No need to verify the main extra body data. It's an 'Attributes ()'
    -- which is valid whenever it's well-formed.
    --
    -- Check internal consistency: the body proofs are all correct.
    checkBodyProof @MainBlockchain _gbBody (_gbhBodyProof _gbHeader)
    -- Check that the headers' extra body data hash is correct.
    -- This isn't subsumed by the body proof check.
    unless (hash (block ^. gbExtra) == (block ^. mainBlockEBDataProof)) $
        throwError "Hash of extra body data is not equal to its representation in the header."
    -- Ssc and Dlg consistency checks which require the header, and so can't
    -- be done in 'verifyMainBody'.
    either (throwError . pretty) pure $
        verifySscPayload
            genesisConfig
            (Right (Some _gbHeader))
            (_mbSscPayload _gbBody)

-- | Verify the body of a block. There are no internal consistency checks,
-- it's just a verification of its sub-components (payloads).
verifyMainBody
    :: MonadError Text m
    => ProtocolMagic
    -> MainBody
    -> m ()
verifyMainBody pm MainBody {..} = do
    checkTxPayload _mbTxPayload
    checkSscPayload pm _mbSscPayload
    checkDlgPayload pm _mbDlgPayload
    checkUpdatePayload pm _mbUpdatePayload

-- | Verify a main block header in isolation.
verifyMainBlockHeader
    :: MonadError Text m
    => ProtocolMagic
    -> GenericBlockHeader MainBlockchain
    -> m ()
verifyMainBlockHeader pm UnsafeGenericBlockHeader {..} = do
    -- Previous header hash is always valid.
    -- Body proof is just a bunch of hashes, which is always valid (although
    -- must be checked against the actual body, in verifyMainBlock.
    -- Consensus data and extra header data require validation.
    verifyMainConsensusData _gbhConsensus
    verifyMainExtraHeaderData _gbhExtra
    -- Internal consistency: is the signature in the consensus data really for
    -- this block?
    unless (verifyBlockSignature _mcdSignature) $
        throwError "can't verify signature"

  where

    verifyBlockSignature (BlockSignature sig) =
        checkSig pm SignMainBlock leaderPk signature sig
    verifyBlockSignature (BlockPSignatureLight proxySig) =
        proxyVerify
            pm
            SignMainBlockLight
            proxySig
            (\(LightDlgIndices (epochLow, epochHigh)) ->
                 epochLow <= epochId && epochId <= epochHigh)
            signature
    verifyBlockSignature (BlockPSignatureHeavy proxySig) =
        proxyVerify pm SignMainBlockHeavy proxySig (const True) signature
    signature = MainToSign _gbhPrevBlock _gbhBodyProof slotId difficulty _gbhExtra
    epochId = siEpoch slotId
    MainConsensusData
        { _mcdLeaderKey = leaderPk
        , _mcdSlot = slotId
        , _mcdDifficulty = difficulty
        , ..
        } = _gbhConsensus

-- | Verify the consensus data in isolation.
verifyMainConsensusData
    :: ( MonadError Text m )
    => MainConsensusData
    -> m ()
verifyMainConsensusData MainConsensusData {..} = do
    when (selfSignedProxy _mcdSignature) $
        throwError "can't use self-signed psk to issue the block"
  where
    selfSignedProxy (BlockSignature _)                      = False
    selfSignedProxy (BlockPSignatureLight (psigPsk -> psk)) = isSelfSignedPsk psk
    selfSignedProxy (BlockPSignatureHeavy (psigPsk -> psk)) = isSelfSignedPsk psk

verifyMainExtraHeaderData
    :: ( MonadError Text m )
    => ExtraHeaderData MainBlockchain
    -> m ()
verifyMainExtraHeaderData MainExtraHeaderData {..} = do
    checkSoftwareVersion _mehSoftwareVersion
