module Cardano.Binary.Class.Annotated
  ( WithByteSpan (..)
  , WithByteString (..)
  , withByteString
  , decodeWithByteString
  )
  where

import qualified Codec.CBOR.Decoding as D
import           Codec.CBOR.Read (ByteOffset)
import qualified Data.ByteString.Lazy as BSL

import           Cardano.Binary.Class.Core (Bi (..))
import           Cardano.Prelude

slice :: BSL.ByteString -> ByteOffset -> ByteOffset -> BSL.ByteString
slice bytes start end = BSL.take (end - start) $ BSL.drop start $ bytes

newtype WithByteSpan a = WithByteSpan (a, ByteOffset, ByteOffset)

instance Bi a => Bi (WithByteSpan a) where
  encode (WithByteSpan (x,_,_)) = encode x
  decode = WithByteSpan <$> D.decodeWithByteSpan decode

data WithByteString a = WithByteString !BSL.ByteString !a

withByteString :: BSL.ByteString -> WithByteSpan a -> WithByteString a
withByteString bytes (WithByteSpan (x, start, end)) = WithByteString (slice bytes start end) x

decodeWithByteString :: Bi a => BSL.ByteString -> D.Decoder s (WithByteString a)
decodeWithByteString bytes = withByteString bytes <$> decode
