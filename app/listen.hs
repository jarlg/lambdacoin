{-# LANGUAGE NoImplicitPrelude, LambdaCase, DeriveGeneric, OverloadedStrings #-}


import qualified Lambdacoin.Helpers as H
import           Lambdacoin.Serialization
import           Lambdacoin.Types

import           Crypto.Hash (hashlazy)
import           Data.Binary (encode, decodeOrFail)
import qualified Data.ByteString.Lazy as L
import           Protolude hiding (get)
import           System.Environment (getArgs)


server :: Handle -> Blockchain -> IO ()
server handle bc = do
  L.putStr "Listening..."
  stream <- L.hGetContents handle
  decodeLoop stream bc
  where
    decodeLoop :: L.ByteString -> Blockchain -> IO ()
    decodeLoop stream bc = do
      case decodeOrFail stream of
        Left (rest, _, e) -> do
          putStrLn ("decode failed: " <> e)
          decodeLoop rest bc
        Right (rest, _, msg) -> do
          handleMsg msg bc >>= decodeLoop rest

    handleMsg :: Msg -> Blockchain -> IO Blockchain
    handleMsg QueryBC bc = do
      L.hPut handle . encode $ RespBC bc
      return bc
    handleMsg (NewBlock header txns) bc
      | _parentHash header == hashlazy (encode bc) = do
          let bc' = Block header txns bc
          putStrLn $ _miner header <> " found a new block!"
          writeFile "balances.txt" . show $ H.balances bc'
          return bc'
    handleMsg (AnnounceTX (Transaction from to amount)) bc = do
      putStrLn $ from <> " sends " <> show amount <> " to " <> to
      return bc
    handleMsg _ bc = return bc
    
main :: IO ()
main = getArgs >>= \case
  [host, port] -> do
    handle <- H.createSocketHandle host port
    H.genesis >>= server handle
  _            -> L.putStr "usage: mine [host] [port]"
