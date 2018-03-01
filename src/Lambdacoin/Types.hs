{-# LANGUAGE DeriveGeneric #-} 

module Lambdacoin.Types where

import Crypto.Hash
import Data.Time.Clock.POSIX
import GHC.Generics


type Account = String

data Transaction = Transaction
  { _from :: Account
  , _to :: Account
  , _amount :: Integer
  } deriving (Eq, Show, Generic)

type HaskoinHash = Digest SHA1
type Difficulty = Integer

data BlockHeader = BlockHeader
  { _miner :: Account
  , _parentHash :: HaskoinHash
  , _nonce :: Integer
  , _time :: POSIXTime
  , _target :: Difficulty
  } deriving (Eq, Show, Generic)

data Blockchain = Genesis POSIXTime
                | Block BlockHeader [Transaction] Blockchain
                deriving (Generic, Show, Eq)

data Msg = QueryBC
         | NewBlock BlockHeader [Transaction] 
         | RespBC Blockchain
         | AnnounceTX Transaction
         deriving (Show, Generic, Eq)
