-- File auto generated by purescript-bridge! --
module Pos.Types.Types where

import Crypto.Hash.Blake2b (Blake2b_512)
import Pos.Crypto.Hashing (AbstractHash)
import Pos.Types.Address (Address)
import Prim (Array, Int)

import Data.Generic (class Generic)


data Coin =
    Coin {
      getCoin :: Int
    }

derive instance genericCoin :: Generic Coin

data Tx =
    Tx {
      txInputs :: Array TxIn
    , txOutputs :: Array TxOut
    }

derive instance genericTx :: Generic Tx

data TxIn =
    TxIn {
      txInHash :: AbstractHash Blake2b_512 Tx
    , txInIndex :: Int
    }

derive instance genericTxIn :: Generic TxIn

data TxOut =
    TxOut {
      txOutAddress :: Address
    , txOutValue :: Coin
    }

derive instance genericTxOut :: Generic TxOut

