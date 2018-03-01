{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Lambdacoin.Serialization where


import           Lambdacoin.Types

import           Crypto.Hash
import           Data.Binary
import           Data.ByteArray (convert)
import qualified Data.ByteString as BS
import           Data.Time


instance Binary Transaction
instance Binary BlockHeader
instance Binary Blockchain
instance Binary Msg

instance Binary HaskoinHash where
  get = do
     mDigest <- digestFromByteString <$> (get :: Get BS.ByteString)
     case mDigest of
       Nothing -> fail "Not a valid digest"
       Just digest -> return digest
  put digest = put $ (convert digest :: BS.ByteString)

instance Binary NominalDiffTime where
  get = fmap fromInteger (get :: Get Integer)
  put t = put (round t :: Integer)
