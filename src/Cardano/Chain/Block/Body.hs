{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.Block.Body
  ( Body
  , body
  , ABody(..)
  , bodyTxs
  , bodyWitnesses
  )
where

import Cardano.Prelude

import Cardano.Binary
  (ByteSpan, FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Ssc (SscPayload(..))
import Cardano.Chain.Txp.Tx (Tx)
import Cardano.Chain.Txp.TxPayload (ATxPayload, TxPayload, txpTxs, txpWitnesses)
import Cardano.Chain.Txp.TxWitness (TxWitness)
import qualified Cardano.Chain.Update.Payload as Update

-- | 'Body' consists of payloads of all block components
type Body = ABody ()

-- | Constructor for 'Body'
body :: TxPayload -> SscPayload -> Delegation.Payload -> Update.Payload -> Body
body tx ssc dlg upd = ABody tx ssc dlg upd

-- | 'Body' consists of payloads of all block components
data ABody a = ABody
  { bodyTxPayload     :: !(ATxPayload a)
  -- ^ Txp payload
  , bodySscPayload    :: !SscPayload
  -- ^ Ssc payload
  , bodyDlgPayload    :: !(Delegation.APayload a)
  -- ^ Heavyweight delegation payload (no-ttl certificates)
  , bodyUpdatePayload :: !(Update.APayload a)
  -- ^ Additional update information for the update system
  } deriving (Eq, Show, Generic, Functor, NFData)

instance ToCBOR Body where
  toCBOR bc =
    encodeListLen 4
      <> toCBOR (bodyTxPayload bc)
      <> toCBOR (bodySscPayload bc)
      <> toCBOR (bodyDlgPayload bc)
      <> toCBOR (bodyUpdatePayload bc)

instance FromCBOR Body where
  fromCBOR = void <$> fromCBOR @(ABody ByteSpan)

instance FromCBOR (ABody ByteSpan) where
  fromCBOR = do
    enforceSize "Body" 4
    ABody
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

bodyTxs :: Body -> [Tx]
bodyTxs = txpTxs . bodyTxPayload

bodyWitnesses :: Body -> [TxWitness]
bodyWitnesses = txpWitnesses . bodyTxPayload
