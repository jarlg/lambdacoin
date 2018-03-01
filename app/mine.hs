{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedStrings #-}

-- http://www.michaelburge.us/2017/08/17/rolling-your-own-blockchain.html

import qualified Lambdacoin.Helpers as H
import           Lambdacoin.Serialization
import           Lambdacoin.Types

import           Control.Concurrent.Async (race)
import           Crypto.Hash (hashlazy)
import           Data.Binary (encode, decodeOrFail)
import qualified Data.ByteString.Lazy as L
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Protolude hiding (get)
import           System.Environment (getArgs)


-- TODO transactions
mine :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mine pendingTx miner parent = do
  txns <- fmap (take H.globalTransactionLimit . H.filterValidTx parent) pendingTx
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


listen :: Handle -> Blockchain -> IO Blockchain
listen handle bc = L.hGetContents handle >>= decodeLoop
  where
    decodeLoop :: L.ByteString -> IO Blockchain
    decodeLoop stream = case decodeOrFail stream of
      Left (rest, _, e) -> do
        putStrLn $ "failed to decode " <> e
        decodeLoop rest
      Right (rest, _, msg) -> either (decodeLoop . const rest) return $ msgHandler msg 

    msgHandler (NewBlock header txns)
      | _parentHash header == hashlazy (encode bc) = Right (Block header txns bc)
    msgHandler _ = Left ()


main :: IO ()
main = getArgs >>= \case
  [name, host, port] -> do
    handle <- H.createSocketHandle host port
    L.hPut handle $ encode QueryBC
    putStrLn ("queried BC" :: Text)
    bc <- L.hGetContents handle >>= awaitBC 
    putStrLn ("received BC." :: Text)
    miningLoop name handle bc
  _ -> putStrLn ("usage: [name] [host] [port]" :: Text)
  where
    awaitBC bs = case decodeOrFail bs of
      Left (rest, _, e) -> do
        putStrLn $ "failed to decode " <> e
        awaitBC rest
      Right (rest, _, msg) -> case msg of
        RespBC bc -> do
          putStrLn ("got blockchain!" :: Text)
          return bc
        _ -> awaitBC rest
      
    miningLoop name handle bc =
      race (mine (return []) name bc) (listen handle bc) >>= \case
        Right bc'@(Block header txns _) -> do
          putStrLn ("received block from " <> _miner header)
          miningLoop name handle bc'
        Left bc'@(Block header txns _) -> do
          putStrLn ("mined a block!" :: Text)
          L.hPut handle . encode $ NewBlock header txns
          miningLoop name handle bc'
