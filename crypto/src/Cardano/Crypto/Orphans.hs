{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Crypto.Orphans
  ()
where

import Cardano.Prelude
import Prelude (error)

import Crypto.Error (CryptoFailable(..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Base64.Type (getByteString64, makeByteString64)
import qualified Data.Text as T

import Cardano.Binary
  ( FromCBOR(..)
  , Size
  , ToCBOR(..)
  , encodeBytes
  , withWordSize
  )
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude.CanonicalExamples.Orphans ()


fromByteStringToBytes :: ByteString -> BA.Bytes
fromByteStringToBytes = BA.convert

fromByteStringToScrubbedBytes :: ByteString -> BA.ScrubbedBytes
fromByteStringToScrubbedBytes = BA.convert

toByteString :: (BA.ByteArrayAccess bin) => bin -> ByteString
toByteString = BA.convert

fromCryptoFailable :: T.Text -> CryptoFailable a -> Either T.Text a
fromCryptoFailable item (CryptoFailed e) =
  Left $ "Cardano.Crypto.Orphan." <> item <> " failed because " <> show e
fromCryptoFailable _ (CryptoPassed r) = return r

instance FromJSON Ed25519.PublicKey where
  parseJSON v = do
    res <-
      Ed25519.publicKey
      .   fromByteStringToBytes
      .   getByteString64
      <$> parseJSON v
    toAesonError $ fromCryptoFailable "parseJSON Ed25519.PublicKey" res

instance ToJSON Ed25519.PublicKey where
  toJSON = toJSON . makeByteString64 . toByteString

instance FromJSON Ed25519.Signature where
  parseJSON v = do
    res <-
      Ed25519.signature
      .   fromByteStringToBytes
      .   getByteString64
      <$> parseJSON v
    toAesonError $ fromCryptoFailable "parseJSON Ed25519.Signature" res

instance ToJSON Ed25519.Signature where
  toJSON = toJSON . makeByteString64 . toByteString

instance ToCBOR Ed25519.PublicKey where
  toCBOR = encodeBytes . toByteString
  encodedSizeExpr _ _ = bsSize Ed25519.secretKeySize

instance FromCBOR Ed25519.PublicKey where
  fromCBOR = do
    res <- Ed25519.publicKey . fromByteStringToBytes <$> fromCBOR
    toCborError $ fromCryptoFailable "fromCBOR Ed25519.PublicKey" res

instance CanonicalExamples Ed25519.PublicKey where
  canonicalExamples = fmap Ed25519.toPublic <$> canonicalExamples

instance CanonicalExamplesSized Ed25519.PublicKey where
  canonicalExamplesSized = fmap Ed25519.toPublic <$> canonicalExamplesSized

instance ToCBOR Ed25519.SecretKey where
  encodedSizeExpr _ _ = bsSize 64
  toCBOR sk = encodeBytes
    $ BS.append (toByteString sk) (toByteString $ Ed25519.toPublic sk)

instance FromCBOR Ed25519.SecretKey where
  fromCBOR = do
    res <-
      Ed25519.secretKey
      .   fromByteStringToScrubbedBytes
      .   BS.take Ed25519.secretKeySize
      <$> fromCBOR
    toCborError $ fromCryptoFailable "fromCBOR Ed25519.SecretKey" res

instance CanonicalExamples Ed25519.SecretKey where
  canonicalExamples = return $ case Ed25519.secretKey bs of
      CryptoFailed err -> error (show err)
      CryptoPassed a   -> [a]
    where
      bs = BS.pack $ replicate Ed25519.secretKeySize 0

instance CanonicalExamplesSized Ed25519.SecretKey where
  canonicalExamplesSized = return $ case Ed25519.secretKey bs of
      CryptoFailed err -> error (show err)
      CryptoPassed a   -> [a]
    where
      bs = BS.pack $ replicate Ed25519.secretKeySize 0


instance ToCBOR Ed25519.Signature where
  encodedSizeExpr _ _ = bsSize 64
  toCBOR = encodeBytes . toByteString

instance FromCBOR Ed25519.Signature where
  fromCBOR = do
    res <- Ed25519.signature . fromByteStringToBytes <$> fromCBOR
    toCborError $ fromCryptoFailable "fromCBOR Ed25519.Signature" res

instance CanonicalExamples Ed25519.Signature where
  canonicalExamples = do
    secret <- canonicalExamples
    public <- canonicalExamples
    bs <- canonicalExamples
    return $ Ed25519.sign <$> secret <*> public <*> (bs :: [BS.ByteString])

instance CanonicalExamplesSized Ed25519.Signature where
  canonicalExamplesSized = do
    secret <- canonicalExamplesSized
    public <- canonicalExamplesSized
    bs <- canonicalExamplesSized
    return $ Ed25519.sign <$> secret <*> public <*> (bs :: [BS.ByteString])

instance CanonicalExamples CC.XPub
instance CanonicalExamplesSized CC.XPub
deriving instance Generic CC.ChainCode
instance CanonicalExamples CC.ChainCode
instance CanonicalExamplesSized CC.ChainCode

-- Can't derive Generic because it's opaque
instance CanonicalExamples CC.XPrv where
  canonicalExamples = return $ case CC.xprv $ BS.replicate 128 0 of
    Left e -> error $ show e
    Right xprv -> [xprv]

-- Can't derive Generic because it's opaque
instance CanonicalExamples CC.XSignature where
  canonicalExamples = return $  case CC.xsignature $ BS.replicate 64 0 of
    Left e -> error $ show e
    Right xSignature -> [xSignature]

instance CanonicalExamplesSized CC.XSignature where
  canonicalExamplesSized = return $  case CC.xsignature $ BS.replicate 64 0 of
    Left e -> error $ show e
    Right xSignature -> [xSignature]


-- Helper for encodedSizeExpr in ToCBOR instances
bsSize :: Int -> Size
bsSize x = fromIntegral (x + withWordSize x)
