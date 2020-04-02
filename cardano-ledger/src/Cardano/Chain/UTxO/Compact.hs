-- | The UTxO is large and is kept in-memory. It is important to use as
-- small a representation as possible to keep overall memory use reasonable.
--
-- This module provides a special compact representation for data types
-- contained within the UTxO.
--
-- The idea here is that the compact representation is optimised only for
-- storage size and does not have to be the same as the representation used
-- when operating on the data. Conversion functions are to be used when
-- inserting and retrieving values from the UTxO.
--

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.UTxO.Compact
  ( CompactTxIn(..)
  , toCompactTxIn
  , fromCompactTxIn

  , CompactTxId
  , toCompactTxId
  , fromCompactTxId

  , CompactTxOut(..)
  , toCompactTxOut
  , fromCompactTxOut
  )
where

import Cardano.Prelude

import Cardano.Crypto.Hashing (hashToBytesShort, unsafeHashFromBytesShort)
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Primitive.PrimArray
                   ( PrimArray(..), indexPrimArray, newPrimArray
                   , writePrimArray, unsafeFreezePrimArray )

import Cardano.Binary (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)
import Cardano.Chain.Common.Compact
  (CompactAddress, fromCompactAddress, toCompactAddress)
import Cardano.Chain.Common.Lovelace (Lovelace)
import Cardano.Chain.UTxO.Tx (TxId, TxIn(..), TxOut(..))

--------------------------------------------------------------------------------
-- Compact TxIn
--------------------------------------------------------------------------------

-- | A compact in-memory representation for a 'TxIn'.
--
-- Convert using 'toCompactTxIn' and 'fromCompactTxIn'.
--
data CompactTxIn = CompactTxInUtxo {-# UNPACK #-} !CompactTxId
                                   {-# UNPACK #-} !Word32
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass (NFData, NoUnexpectedThunks)

instance HeapWords CompactTxIn where
  heapWords _
    -- We have
    --
    -- > data CompactTxIn = CompactTxInUtxo {-# UNPACK #-} !CompactTxId
    -- >                                    {-# UNPACK #-} !Word32
    --
    -- so 'CompactTxInUtxo' requires:
    --
    -- - 1 word for the 'CompactTxInUtxo' object header
    -- - 4 words (on a 64-bit arch) for the unpacked 'CompactTxId'
    -- - 1 word for the unpacked 'Word32'
    --
    -- +---------------------------------------------+
    -- │CompactTxInUtxo│Word#|Word#│Word#│Word#│Word#│
    -- +---------------------------------------------+
    --
    = 6

instance FromCBOR CompactTxIn where
  fromCBOR = do
    enforceSize "CompactTxIn" 2
    CompactTxInUtxo
      <$> fromCBOR
      <*> fromCBOR

instance ToCBOR CompactTxIn where
  toCBOR (CompactTxInUtxo txId txIndex) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR txIndex

toCompactTxIn :: TxIn -> CompactTxIn
toCompactTxIn (TxInUtxo txId txIndex) =
  CompactTxInUtxo (toCompactTxId txId) txIndex

fromCompactTxIn :: CompactTxIn -> TxIn
fromCompactTxIn (CompactTxInUtxo compactTxId txIndex) =
  TxInUtxo (fromCompactTxId compactTxId) txIndex

--------------------------------------------------------------------------------
-- Compact TxId
--------------------------------------------------------------------------------

-- | A compact in-memory representation for a 'TxId'.
--
-- Convert using 'toCompactTxId' and 'fromCompactTxId'.
--
-- Compared to a normal 'TxId', this takes 5 heap words rather than 12.
--
data CompactTxId = CompactTxId {-# UNPACK #-} !Word64
                               {-# UNPACK #-} !Word64
                               {-# UNPACK #-} !Word64
                               {-# UNPACK #-} !Word64
  deriving (Eq, Generic, Ord, Show)
  deriving anyclass (NFData, NoUnexpectedThunks)

instance HeapWords CompactTxId where
  heapWords _
    -- We have
    --
    -- > data CompactTxId = CompactTxId {-# UNPACK #-} !Word64
    -- >                                {-# UNPACK #-} !Word64
    -- >                                {-# UNPACK #-} !Word64
    -- >                                {-# UNPACK #-} !Word64
    --
    -- so 'CompactTxId' requires:
    --
    -- - 1 word for the 'CompactTxId' object header
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64'
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64'
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64'
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64'
    --
    -- +-----------------------------------+
    -- │CompactTxId│Word#│Word#│Word#│Word#│
    -- +-----------------------------------+
    --
    = 5

instance FromCBOR CompactTxId where
  fromCBOR = do
    enforceSize "CompactTxId" 4
    CompactTxId
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance ToCBOR CompactTxId where
  toCBOR (CompactTxId a b c d) =
    encodeListLen 4
      <> toCBOR a
      <> toCBOR b
      <> toCBOR c
      <> toCBOR d

toCompactTxId :: TxId -> CompactTxId
toCompactTxId txid =
    -- This is a little bit cunning. We extract the ByteArray# from the
    -- ShortByteString representation of the TxId Hash, and using the primitive
    -- package we make a PrimArray Word64 which we can then use to read the
    -- four words. So this should mean the cost is just 4 memory reads & writes.
    CompactTxId
      (indexPrimArray arr 0)
      (indexPrimArray arr 1)
      (indexPrimArray arr 2)
      (indexPrimArray arr 3)
  where
    arr :: PrimArray Word64
    arr = toPrimArray (hashToBytesShort txid)

    toPrimArray :: SBS.ShortByteString -> PrimArray Word64
    toPrimArray (SBS.SBS ba) = PrimArray ba

fromCompactTxId :: CompactTxId -> TxId
fromCompactTxId (CompactTxId w0 w1 w2 w3) =
    unsafeHashFromBytesShort
  . fromPrimArray
  $ runST mkByteArray
  where
    mkByteArray :: ST s (PrimArray Word64)
    mkByteArray = do
      arr <- newPrimArray 4
      writePrimArray arr 0 w0
      writePrimArray arr 1 w1
      writePrimArray arr 2 w2
      writePrimArray arr 3 w3
      unsafeFreezePrimArray arr

    fromPrimArray :: PrimArray Word64 -> SBS.ShortByteString
    fromPrimArray (PrimArray ba) = SBS.SBS ba

--------------------------------------------------------------------------------
-- Compact TxOut
--------------------------------------------------------------------------------

-- | A compact in-memory representation for a 'TxOut'.
--
-- Convert using 'toCompactTxOut' and 'fromCompactTxOut'.
--
data CompactTxOut = CompactTxOut                !CompactAddress
                                 {-# UNPACK #-} !Lovelace
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass (NFData, NoUnexpectedThunks)

instance HeapWords CompactTxOut where
  heapWords (CompactTxOut compactAddr _)
    -- We have
    --
    -- > data CompactTxOut = CompactTxOut {-# UNPACK #-} !CompactAddress
    -- >                                  {-# UNPACK #-} !Lovelace
    -- > newtype CompactAddress = CompactAddress ShortByteString
    -- > newtype Lovelace = Lovelace { getLovelace :: Word64 }
    --
    -- so @CompactTxOut {-# UNPACK #-} !CompactAddress {-# UNPACK #-} !Lovelace@
    -- requires:
    --
    -- - 1 word for the 'CompactTxOut' object header
    -- - 1 word for the pointer to the byte array object
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64' ('Lovelace')
    -- - the heap words required by the byte array object
    --
    -- Note that for the sake of uniformity, we use 'heapWordsUnpacked' to
    -- account for the level of indirection removed by the @UNPACK@ pragma.
    --
    -- +----------------------+
    -- │CompactTxOut│ * │Word#│
    -- +--------------+-------+
    --                |
    --                v
    --                +--------------+
    --                │BA#│sz│payload│
    --                +--------------+
    --
    = 3 + heapWordsUnpacked compactAddr

instance FromCBOR CompactTxOut where
  fromCBOR = do
    enforceSize "CompactTxOut" 2
    CompactTxOut
      <$> fromCBOR
      <*> fromCBOR

instance ToCBOR CompactTxOut where
  toCBOR (CompactTxOut compactAddr lovelace) =
    encodeListLen 2
      <> toCBOR compactAddr
      <> toCBOR lovelace

toCompactTxOut :: TxOut -> CompactTxOut
toCompactTxOut (TxOut addr lovelace) =
  CompactTxOut (toCompactAddress addr) lovelace

fromCompactTxOut :: CompactTxOut -> TxOut
fromCompactTxOut (CompactTxOut compactAddr lovelace) =
  TxOut (fromCompactAddress compactAddr) lovelace
