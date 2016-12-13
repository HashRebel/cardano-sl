-- File auto generated by purescript-bridge! --
module Pos.Types.Address where

import Crypto.Hash.Blake2s (Blake2s_224)
import PlutusCore.Program (Program)
import Pos.Crypto.Hashing (AbstractHash)
import Pos.Crypto.Signing (PublicKey)
import Prim (Int)

import Data.Generic (class Generic)


data Address =
    PubKeyAddress {
      addrVersion :: Int
    , addrKeyHash :: AbstractHash Blake2s_224 PublicKey
    }
  | ScriptAddress {
      addrVersion :: Int
    , addrScriptHash :: AbstractHash Blake2s_224 Program
    }

derive instance genericAddress :: Generic Address
