{-# LANGUAGE LambdaCase #-}

import Lambdacoin.Types
import System.Environment (getArgs)
import Data.Binary
import qualified Lambdacoin.Helpers as H

import qualified Data.ByteString.Lazy as L

main :: IO ()
main = getArgs >>= \case 
  [host, port, from, to, amount] -> do
    handle <- H.createSocketHandle host port
    L.hPut handle . encode . AnnounceTX $ Transaction from to (read amount)
  _ -> putStrLn "usage: [from] [to] [amount]"
    
