{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Cardano.Chain.Delegation.Payload
  ( Payload(..)
  , unsafePayload
  )
where

import Cardano.Prelude

import Formatting (bprint, int)
import Formatting.Buildable (Buildable(..))

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan
  , Decoded(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , annotatedDecoder
  , FromCBORAnnotated (..)
  , serialize'
  , encodePreEncoded
  , withSlice'
  )
import qualified Cardano.Chain.Delegation.Certificate as Delegation


-- | The delegation 'Payload' contains a list of delegation 'Certificate's
data Payload = UnsafePayload
  { getPayload       :: ![Delegation.Certificate]
  , serializePayload :: ByteString
  } deriving (Show, Eq, Generic)
    deriving anyclass NFData

unsafePayload :: [Delegation.Certificate] -> Payload
unsafePayload sks = UnsafePayload sks (serialize' sks)

instance Decoded Payload where
  type BaseType Payload = Payload
  recoverBytes = serializePayload

instance Buildable Payload where
  build (UnsafePayload psks _) = bprint
    ("proxy signing keys (" . int . " items): " . listJson . "\n")
    (length psks)
    psks

instance ToCBOR Payload where
  toCBOR = encodePreEncoded . serializePayload

instance FromCBORAnnotated Payload where
  fromCBORAnnotated' = withSlice' $ UnsafePayload <$> fromCBORAnnotated'
