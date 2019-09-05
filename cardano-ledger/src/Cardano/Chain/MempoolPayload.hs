{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.MempoolPayload
  ( MempoolPayload
  , AMempoolPayload (..)
  )
where

import Cardano.Prelude

import Cardano.Binary
  ( DecoderError(..)
  , FromCBOR(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , decodeWord8
  , encodeListLen
  , enforceSize
  )
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.UTxO.TxPayload (TxPayload)
import qualified Cardano.Chain.Update.Payload as Update

-- | A payload which can be submitted into or between mempools via the
-- transaction submission protocol.
type MempoolPayload = AMempoolPayload ()

-- | A payload which can be submitted into or between mempools via the
-- transaction submission protocol.
data AMempoolPayload a
  = MempoolTxPayload !TxPayload
  -- ^ A transaction payload.
  | MempoolDlgPayload !(Delegation.APayload a)
  -- ^ A delegation payload.
  | MempoolUpdatePayload !(Update.APayload a)
  -- ^ An update payload.
  deriving (Eq, Show)

instance ToCBOR MempoolPayload where
  toCBOR (MempoolTxPayload tp) =
    encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR tp
  toCBOR (MempoolDlgPayload dp) =
    encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR dp
  toCBOR (MempoolUpdatePayload up) =
    encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR up

instance FromCBORAnnotated MempoolPayload where
  fromCBORAnnotated' = do
    lift $ enforceSize "MempoolPayload" 2
    lift decodeWord8 >>= \case
      0   -> MempoolTxPayload <$> fromCBORAnnotated'
      1   -> MempoolDlgPayload <$> lift fromCBOR
      2   -> MempoolUpdatePayload <$> lift fromCBOR
      tag -> lift . cborError $ DecoderErrorUnknownTag "MempoolPayload" tag
