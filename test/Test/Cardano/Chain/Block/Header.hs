{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Block.Header
  ( tests
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Lazy as BSL
import           Hedgehog (Property, (===))
import qualified Hedgehog as H

import           Cardano.Binary.Class (Bi (..), DecoderError,
                     decodeFullAnnotatedBytes, serializeEncoding)
import           Cardano.Chain.Block.Header (Header, HeaderError, decodeAHeader,
                     verifyAHeader)
import           Cardano.Crypto (ProtocolMagic)
import           Test.Cardano.Chain.Block.Gen (genHeader)
import           Test.Cardano.Chain.Slotting.Gen (genSlotCount)
import           Test.Cardano.Crypto.Gen (genProtocolMagic)


tests :: IO Bool
tests = H.checkSequential $ H.Group "Test.Cardano.Chain.Block.Header"
  [ ("validate headers using annotation", validateAnnotatedHeader)
  ]

data Error = ErrorH HeaderError | ErrorD DecoderError
  deriving (Show, Eq)

validateAnnotatedHeader :: Property
validateAnnotatedHeader = H.property $ do
  pm <- H.forAll genProtocolMagic
  sc <- H.forAll genSlotCount
  header <- H.forAll $ genHeader pm sc
  let bytes = (serializeEncoding . encode) header
  recover pm bytes === Right header
  where
  recover :: ProtocolMagic -> BSL.ByteString -> Either Error Header
  recover pm bytes = do
    header <- first ErrorD $ decodeFullAnnotatedBytes "Header" decodeAHeader bytes
    first ErrorH $ verifyAHeader pm header
    pure $ map (const ()) header
