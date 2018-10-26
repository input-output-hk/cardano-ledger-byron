{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Rank2Types #-}

module Cardano.Binary.Class.Annotated
  ( Annotated (..)
  , ByteSpan (..)
  , slice
  , decodeAnnotated
  , decodeFullAnnotatedBytes
  )
  where

import qualified Codec.CBOR.Decoding as D
import           Codec.CBOR.Read (ByteOffset)
import qualified Data.ByteString.Lazy as BSL

import           Cardano.Binary.Class.Core (Bi (..), DecoderError)
import           Cardano.Binary.Class.Primitive (decodeFullDecoder)
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

decodeFullAnnotatedBytes
  :: (Functor f)
  => Text
  -> (forall s. D.Decoder s (f ByteSpan))
  -> BSL.ByteString
  -> Either DecoderError (f ByteString)
decodeFullAnnotatedBytes lbl decoder bytes = (fmap.fmap) (BSL.toStrict . slice bytes) $
  decodeFullDecoder lbl decoder bytes
