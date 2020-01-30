{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Cardano.Chain.UTxO.TxAux
  ( TxAux
  , ATxAux(..)
  , mkTxAux
  , annotateTxAux
  , fromCborTxAux
  , toCborTxAux
  , taTx
  , taWitness
  , txaF
  )
where

import Cardano.Prelude

import Formatting (Format, bprint, build, later)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan
  , Decoded(..)
  , DecoderError
  , FromCBOR(..)
  , ToCBOR(..)
  , annotationBytes
  , annotatedDecoder
  , fromCBORAnnotated
  , decodeFullDecoder
  , encodeListLen
  , enforceSize
  , serialize
  , unsafeDeserialize
  )
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxWitness (TxWitness)

import qualified Data.ByteString.Lazy.Char8 as LBS


-- | Transaction + auxiliary data
type TxAux = ATxAux ()

mkTxAux :: Tx -> TxWitness -> TxAux
mkTxAux tx tw = ATxAux (Annotated tx ()) (Annotated tw ()) ()

annotateTxAux :: TxAux -> ATxAux ByteString
annotateTxAux ta = annotationBytes bs ta'
  where
    bs  = serialize ta
    ta' = unsafeDeserialize bs

data ATxAux a = ATxAux
  { aTaTx         :: !(Annotated Tx a)
  , aTaWitness    :: !(Annotated TxWitness a)
  , aTaAnnotation :: !a
  } deriving (Generic, Show, Eq, Functor)
    deriving anyclass NFData

instance Decoded (ATxAux ByteString) where
  type BaseType (ATxAux ByteString) = ATxAux ()
  recoverBytes = aTaAnnotation

taTx :: ATxAux a -> Tx
taTx = unAnnotated . aTaTx

taWitness :: ATxAux a -> TxWitness
taWitness = unAnnotated . aTaWitness

-- | Specialized formatter for 'TxAux'
txaF :: Format r (TxAux -> r)
txaF = later $ \ta -> bprint
  (build . "\n" . "witnesses: " . listJsonIndent 4)
  (taTx ta)
  (taWitness ta)

instance B.Buildable TxAux where
  build = bprint txaF

instance ToCBOR TxAux where
  toCBOR ta = encodeListLen 2 <> toCBOR (taTx ta) <> toCBOR (taWitness ta)

  encodedSizeExpr size pxy = 1 + size (taTx <$> pxy) + size (taWitness <$> pxy)

instance FromCBOR TxAux where
  fromCBOR = void <$> fromCBOR @(ATxAux ByteSpan)

instance FromCBOR (ATxAux ByteSpan) where
  fromCBOR = do
    Annotated (tx, witness) byteSpan <- annotatedDecoder $ do
      enforceSize "ATxAux" 2
      tx      <- fromCBORAnnotated
      witness <- fromCBORAnnotated
      pure (tx, witness)
    pure $ ATxAux tx witness byteSpan

fromCborTxAux :: ByteString ->  Either DecoderError (ATxAux ByteString)
fromCborTxAux bs =
    fmap (annotationBytes lbs)
      $ decodeFullDecoder "Cardano.Chain.UTxO.TxAux.fromCborTxAux" fromCBOR lbs
  where
    lbs = LBS.fromStrict bs

toCborTxAux :: ATxAux ByteString -> ByteString
toCborTxAux = aTaAnnotation -- The ByteString anotation is the CBOR encoded version.
