module Haskon.Serialization where


import Haskoin.Types

import Data.Binary
import Data.Binary.Get
import Data.ByteArray (convert)

instance Binary Transaction
instance Binary BlockHeader
instance Binary Blockchain

instance Binary HaskoinHash where
  get = do
     mDigest <- digestFromByteString <$> (get :: Get S.ByteString)
     case mDigest of
       Nothing -> fail "Not a valid digest"
       Just digest -> return digest
  put digest = put $ (convert digest :: S.ByteString)

instance Binary NominalDiffTime where
  get = fmap fromInteger (get :: Get Integer)
  put t = put (round t :: Integer)
