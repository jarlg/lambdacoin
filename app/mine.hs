{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, LambdaCase #-}

-- http://www.michaelburge.us/2017/08/17/rolling-your-own-blockchain.html

import qualified Lambdacoin.Helpers as H
import           Lambdacoin.Serialization
import           Lambdacoin.Types

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (race)
import           Control.Concurrent.MVar
import           Crypto.Hash (hashlazy)
import           Data.Binary (encode, decodeOrFail)
import qualified Data.ByteString.Lazy as L
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Protolude hiding (get)
import           System.Environment (getArgs)


  

mine :: MVar [Transaction] -> Account -> Blockchain -> (IO Blockchain)
mine txnsM miner parent = do
  txns <- fmap (take H.globalTransactionLimit . H.filterValidTx parent) $ swapMVar txnsM []
  putStrLn $ "mining transactions: " <> show txns
  loop txns 0
  where
    parentHash = hashlazy (encode parent)
    newTarget = H.target parent
    loop txns nonce = do
      now <- getPOSIXTime
      let header = BlockHeader miner parentHash nonce now newTarget
      if H.difficulty header < newTarget
        then return (Block header txns parent)
        else loop txns (nonce+1)


listenForBlock :: MVar (BlockHeader, [Transaction]) -> Blockchain -> IO Blockchain
listenForBlock blockM bc = do
  (bh, txns) <- takeMVar blockM
  putStrLn $ "received block from " <> _miner bh
  if _parentHash bh == hashlazy (encode bc)
    then return (Block bh txns bc)
    else listenForBlock blockM bc

listen :: Handle -> MVar (BlockHeader, [Transaction]) -> MVar [Transaction] -> IO () 
listen handle blockM txnsM = L.hGetContents handle >>= decodeLoop
  where
    decodeLoop :: L.ByteString -> IO ()
    decodeLoop stream = case decodeOrFail stream of
      Left (rest, _, e) -> do
        putStrLn $ "failed to decode " <> e
        decodeLoop rest
      Right (rest, _, msg) ->
        case msg of
          NewBlock header txns -> do
            putMVar blockM (header, txns)                 
            decodeLoop rest
          AnnounceTX tx -> do
            modifyMVar_ txnsM (\txns -> return (tx:txns))
            decodeLoop rest
          _ -> decodeLoop rest

main :: IO ()
main = getArgs >>= \case
  [name, host, port] -> do
    handle <- H.createSocketHandle host port
    L.hPut handle $ encode QueryBC
    putStrLn "queried BC"
    bc <- L.hGetContents handle >>= awaitBC 
    putStrLn "received BC."
    blockM <- newEmptyMVar
    txnsM <- newMVar [] 
    forkIO $ listen handle blockM txnsM
    miningLoop name handle blockM txnsM bc
  _ -> putStrLn "usage: [name] [host] [port]"
  where
    awaitBC bs = case decodeOrFail bs of
      Left (rest, _, e) -> do
        putStrLn $ "failed to decode " <> e
        awaitBC rest
      Right (rest, _, msg) -> case msg of
        RespBC bc -> do
          putStrLn "got blockchain!"
          return bc
        _ -> awaitBC rest
      
    miningLoop name handle blockM txnsM bc =
      race (mine txnsM name bc) (listenForBlock blockM bc) >>= \case
        Right bc'@(Block header txns _) -> do
          putStrLn ("received block from " <> _miner header)
          miningLoop name handle blockM txnsM bc'
        Left bc'@(Block header txns _) -> do
          putStrLn "mined a block!"
          L.hPut handle . encode $ NewBlock header txns
          miningLoop name handle blockM txnsM bc'
