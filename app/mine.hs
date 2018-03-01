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


