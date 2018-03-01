{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances #-}

module Lambdacoin.Helpers where


import Lambdacoin.Types
import Lambdacoin.Serialization()

import Protolude
import Prelude ((!!))
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Crypto.Hash -- should use cryptonite!
import Crypto.Number.Serialize (os2ip)
import Data.Binary (encode)
import Network.Socket
import System.IO (hSetBuffering, hSetBinaryMode, BufferMode(NoBuffering))


miningReward = 10 :: Integer
globalTransactionLimit = 1000 :: Int

genesisDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF :: Difficulty

-- epoch is # of blocks before difficulty is recalculated
epochSize = 16 :: Int-- 2m40s at 10s/block
epochTime = 160 :: NominalDiffTime


instance Ord Blockchain where
  bc <= bc' = work bc <= work bc'


-- octet stream to integer primitive; converts the ByteArray to an Integer
-- a lower integer is more difficult, corresponding to the task of finding
-- a nonce giving a hash whose numerical value is lower than the difficulty
-- should only hash header!
difficulty :: BlockHeader -> Difficulty
difficulty bh = os2ip (hashlazy (encode bh) :: HaskoinHash)

-- want a block to be mined every 10secs!
-- recalculates target difficulty every N=epochSize blocks
target :: Blockchain -> Difficulty
target (Genesis _) = genesisDifficulty
target (Block header _ parent)
  | curEpochSize >= epochSize = round $ factor * (fromIntegral curTarget)
  | otherwise = curTarget
  where 
    curTarget = _target header 
    dt = _time header - epochStart
    factor = max 0.25 (dt / epochTime)
    epochStart = times parent !! (epochSize - 1)
    curEpochSize = 1 + (length $ takeWhile (== curTarget) (targets parent))

balances :: Blockchain -> M.Map Account Integer
balances bc = M.fromListWith (+) $ rewards ++ credits
  where
    rewards = [ (_miner header, miningReward) | header <- headers bc ]
    credits = foldl transfer [] (transactions bc)
      where
        transfer :: [(Account, Integer)] -> Transaction -> [(Account, Integer)]
        transfer xs (Transaction from to amount) = (to, amount) : (from, -amount) : xs

times :: Blockchain -> [POSIXTime]
times (Genesis t) = [t]
times (Block h _ parent) = _time h : times parent

targets :: Blockchain -> [Difficulty]
targets (Genesis _) = [0]
targets (Block h _ parent) = _target h : targets parent

headers :: Blockchain -> [BlockHeader]
headers (Genesis _) = []
headers (Block h _ parent) = h : headers parent

transactions :: Blockchain -> [Transaction]
transactions (Genesis _) = []
transactions (Block _ txns parent) = txns ++ transactions parent

-- validates txns; not e.g. blockheaders
filterValidTx :: Blockchain -> [Transaction] -> [Transaction]
filterValidTx bc txns =
  let ledger = balances bc -- important instead of inlining?!
      valid txn = case M.lookup (_from txn) ledger of
        Nothing -> False
        Just balance -> balance >= _amount txn
  in filter valid txns

-- total difficulty
work :: Blockchain -> Difficulty
work = sum . map _target . headers 

genesis :: IO Blockchain
genesis = Genesis <$> getPOSIXTime  

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
