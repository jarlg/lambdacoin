{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances #-}

module Lambdacoin.Helpers where


import Lambdacoin.Types
import Lambdacoin.Serialization()

import Protolude
import Prelude ((!!))
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Crypto.Hash
import Crypto.Number.Serialize (os2ip)
import Data.Binary (encode)
import Network.Socket
import System.IO (hSetBuffering, hSetBinaryMode, BufferMode(NoBuffering))

-- SETTINGS

miningReward = 10 :: Integer
globalTransactionLimit = 1000 :: Int

genesisDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF :: Difficulty

-- epoch is # of blocks before difficulty is recalculated
epochSize = 16 :: Int-- 2m40s at 10s/block
epochTime = 160 :: NominalDiffTime


-- octet stream to integer primitive; converts the ByteArray to an Integer
-- a lower integer is more difficult, corresponding to the task of finding
-- a nonce giving a hash whose numerical value is lower than the difficulty
-- should only hash header!
difficulty :: BlockHeader -> Difficulty
difficulty bh = undefined

times :: Blockchain -> [POSIXTime]
times = undefined

targets :: Blockchain -> [Difficulty]
targets = undefined


transactions :: Blockchain -> [Transaction]
transactions = undefined

headers :: Blockchain -> [BlockHeader]
headers = undefined


-- want a block to be mined every 10secs!
-- recalculates target difficulty every N=epochSize blocks
target :: Blockchain -> Difficulty
target = undefined

balances :: Blockchain -> M.Map Account Integer
balances = undefined


-- validates txns; not e.g. blockheaders
filterValidTx :: Blockchain -> [Transaction] -> [Transaction]
filterValidTx = undefined

-- total difficulty
work :: Blockchain -> Difficulty
work = undefined

genesis :: IO Blockchain
genesis = Genesis <$> getPOSIXTime


-- NETWORK

createSocketHandle :: HostName -> ServiceName -> IO Handle
createSocketHandle host port = do 
    -- Extract address from first AddrInfo in list
    AddrInfo{ addrAddress = addr, addrFamily = family }:_
      <- getAddrInfo Nothing (Just host) (Just port)

      -- Create a TCP socket connected to server
    s <- socket family Stream 0
    connect s addr

      -- Convert socket to handle
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h NoBuffering  -- THIS IS IMPORTANT
    hSetBinaryMode stdout True  -- Deal w. broken unicode
    return h
