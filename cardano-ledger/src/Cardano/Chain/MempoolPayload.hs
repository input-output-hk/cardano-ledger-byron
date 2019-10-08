{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.MempoolPayload
  ( MempoolPayload (..)
  )
where

import Cardano.Prelude

import Cardano.Binary
  ( DecoderError(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , decodeWord8
  , encodeListLen
  , decodeListLen
  , matchSize
  )
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.UTxO.TxPayload (TxPayload)
import qualified Cardano.Chain.Update.Payload as Update

-- | A payload which can be submitted into or between mempools via the
-- transaction submission protocol.
data MempoolPayload
  = MempoolTxPayload !TxPayload
  -- ^ A transaction payload.
  | MempoolDlgPayload !Delegation.Payload
  -- ^ A delegation payload.
  | MempoolUpdatePayload !Update.Payload
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
    len <- lift $ decodeListLen
    lift $ matchSize "MempoolPayload" 2 len
    lift decodeWord8 >>= \case
      0   -> MempoolTxPayload <$> fromCBORAnnotated'
      1   -> MempoolDlgPayload <$> fromCBORAnnotated'
      2   -> MempoolUpdatePayload <$> fromCBORAnnotated'
      tag -> lift $ cborError $ DecoderErrorUnknownTag "MempoolPayload" tag
