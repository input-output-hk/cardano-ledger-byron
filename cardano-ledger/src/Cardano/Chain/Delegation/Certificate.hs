{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Chain.Delegation.Certificate
  (
  -- * Certificate
    Certificate(..)

  -- * Certificate Constructors
  , signCertificate
  , unsafeCertificate

  -- * Certificate Accessor
  , epoch

  -- * Certificate Predicate
  , isValid
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import Data.Coerce (coerce)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical
  (FromJSON(..), Int54, JSValue(..), ToJSON(..), fromJSField, mkObject)

import Cardano.Binary
  ( Annotated(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , FromCBORAnnotated (..)
  , encodeListLen
  , enforceSize
  , serialize'
  , encodePreEncoded
  , serializeEncoding'
  , withSlice'
  )
import Cardano.Chain.Slotting (EpochNumber)
import Cardano.Crypto
  ( ProtocolMagicId
  , SafeSigner
  , SignTag(SignCertificate)
  , Signature
  , VerificationKey(unVerificationKey)
  , safeSign
  , safeToVerification
  , verifySignatureDecoded
  )


--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

-- | Delegation certificate allowing the @delegateVK@ to sign blocks on behalf
--   of @issuerVK@
--
--   Each delegator can publish at most one 'Certificate' per 'EpochNumber', and
--   that 'EpochNumber' must correspond to the current or next 'EpochNumber' at
--   the time of publishing
data Certificate = UnsafeCertificate
  { aEpoch     :: Annotated EpochNumber ByteString
  -- ^ The epoch from which the delegation is valid
  , issuerVK   :: VerificationKey
  -- ^ The issuer of the certificate, who delegates their right to sign blocks
  , delegateVK :: VerificationKey
  -- ^ The delegate, who gains the right to sign blocks
  , signature  :: Signature EpochNumber
  -- ^ The signature that proves the certificate was issued by @issuerVK@
  , serialize  :: ByteString
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass NFData

--------------------------------------------------------------------------------
-- Certificate Constructors
--------------------------------------------------------------------------------

-- | Create a 'Certificate', signing it with the provided safe signer.
signCertificate
  :: ProtocolMagicId
  -> VerificationKey
  -> EpochNumber
  -> SafeSigner
  -> Certificate
signCertificate protocolMagicId delegateVK epochNumber safeSigner =
  unsafeCertificate epochNumber issuerVK delegateVK signature
  where
  issuerVK   = safeToVerification safeSigner
  signature  = coerce sig :: Signature EpochNumber
  sig = safeSign protocolMagicId SignCertificate safeSigner
    $ mconcat [ "00"
              , CC.unXPub (unVerificationKey delegateVK)
              , serialize' epochNumber]

-- | Create a certificate using the provided signature.
unsafeCertificate
  :: EpochNumber
  -> VerificationKey
  -- ^ The issuer of the certificate. See 'UnsafeCertificate'.
  -> VerificationKey
  -- ^ The delegate of the certificate. See 'UnsafeCertificate'.
  -> Signature EpochNumber
  -> Certificate
unsafeCertificate epochNumber issuerVK delegateVK signature =
  let serialize =  serializeEncoding' $ encodeListLen 4
        <> encodePreEncoded epochNumberBytes
        <> toCBOR issuerVK
        <> toCBOR delegateVK
        <> toCBOR signature
      epochNumberBytes = serialize' epochNumber
      aEpoch = Annotated epochNumber epochNumberBytes
  in UnsafeCertificate { aEpoch, issuerVK, delegateVK, signature, serialize }


--------------------------------------------------------------------------------
-- Certificate Accessor
--------------------------------------------------------------------------------

epoch :: Certificate -> EpochNumber
epoch = unAnnotated . aEpoch


--------------------------------------------------------------------------------
-- Certificate Predicate
--------------------------------------------------------------------------------

-- | A 'Certificate' is valid if the 'Signature' is valid
isValid
  :: Annotated ProtocolMagicId ByteString
  -> Certificate
  -> Bool
isValid pm UnsafeCertificate { aEpoch, issuerVK, delegateVK, signature } =
  verifySignatureDecoded
    pm
    SignCertificate
    issuerVK
    (   serialize'
    .   mappend ("00" <> CC.unXPub (unVerificationKey delegateVK))
    <$> aEpoch
    )
    signature


--------------------------------------------------------------------------------
-- Certificate Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Certificate where
  toCBOR = encodePreEncoded . serialize

instance FromCBORAnnotated Certificate where
  fromCBORAnnotated' = withSlice' $
    UnsafeCertificate <$ lift (enforceSize "Delegation.Certificate" 4)
      <*> fromCBORAnnotated'
      <*> lift fromCBOR
      <*> lift fromCBOR
      <*> lift fromCBOR


--------------------------------------------------------------------------------
-- Certificate Formatting
--------------------------------------------------------------------------------

instance B.Buildable Certificate where
  build (UnsafeCertificate e iVK dVK _ _) = bprint
    ( "Delegation.Certificate { w = " . build
    . ", iVK = " . build
    . ", dVK = " . build
    . " }"
    )
    (unAnnotated e)
    iVK
    dVK


--------------------------------------------------------------------------------
-- Certificate Canonical JSON
--------------------------------------------------------------------------------

instance Monad m => ToJSON m Certificate where
  toJSON cert = mkObject
    -- omega is encoded as a number, because in genesis we always set it to 0
    [ ("omega", pure (JSNum . fromIntegral $ epoch cert))
    , ("issuerPk"  , toJSON $ issuerVK cert)
    , ("delegatePk", toJSON $ delegateVK cert)
    , ("cert"      , toJSON $ signature cert)
    ]

instance MonadError SchemaError m => FromJSON m Certificate where
  fromJSON obj =
    unsafeCertificate
      <$> (fromIntegral @Int54 <$> fromJSField obj "omega")
      <*> fromJSField obj "issuerPk"
      <*> fromJSField obj "delegatePk"
      <*> fromJSField obj "cert"
