{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Binary.Class.Annotated
  ( Annotated (..)
  , ByteSpan (..)
  , slice
  , decodeAnnotated
  )
  where

import qualified Codec.CBOR.Decoding as D
import           Codec.CBOR.Read (ByteOffset)
import qualified Data.ByteString.Lazy as BSL

import           Cardano.Binary.Class.Core (Bi (..))
import           Cardano.Prelude


slice :: BSL.ByteString -> ByteSpan -> BSL.ByteString
slice bytes (ByteSpan start end) = BSL.take (end - start) $ BSL.drop start $ bytes

data ByteSpan = ByteSpan !ByteOffset !ByteOffset

data Annotated b a = Annotated { unAnnotated :: !b, annotation :: !a }
  deriving (Eq, Show, Functor, Generic)
  deriving anyclass NFData

annotatedDecoder :: D.Decoder s a -> D.Decoder s (Annotated a ByteSpan)
annotatedDecoder d = flip fmap (D.decodeWithByteSpan d) $ \(x, start, end) ->
  Annotated x (ByteSpan start end)

decodeAnnotated :: (Bi a) => D.Decoder s (Annotated a ByteSpan)
decodeAnnotated = annotatedDecoder decode
