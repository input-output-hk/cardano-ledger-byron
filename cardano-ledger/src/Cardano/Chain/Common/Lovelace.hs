{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- This is for 'mkKnownLovelace''s @n <= 45000000000000000@ constraint, which is
-- considered redundant. TODO: investigate this.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Chain.Common.Lovelace
  (
  -- * Lovelace
    Lovelace

    -- Only export the error case that is still possible:
  , LovelaceError(LovelaceUnderflow)

  -- * Conversions
  , naturalToLovelace
  , lovelaceToNatural
  , lovelaceToInteger

  -- * Arithmetic operations
  , sumLovelace
  , addLovelace
  , subLovelace
  , scaleLovelace
  , scaleLovelaceRational
  , divLovelace
  , modLovelace

  -- * Formatting
  , lovelaceF
  )
where

import Cardano.Prelude

import Data.Data (Data)
import Data.Monoid (Monoid(..))
import Formatting (Format, bprint, build, int)
import qualified Formatting.Buildable as B
import qualified Text.JSON.Canonical as Canonical
  (FromJSON(..), ReportSchemaErrors, ToJSON(..))

import Cardano.Binary
  ( DecoderError(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , decodeListLen
  , decodeWord8
  , encodeListLen
  , matchSize
  )


-- | Lovelace is the least possible unit of currency
newtype Lovelace = Lovelace { unLovelace :: Natural }
  deriving (Show, Ord, Eq, Generic, Data, NFData, NoUnexpectedThunks)

instance Semigroup Lovelace where
  Lovelace a <> Lovelace b = Lovelace (a + b)

instance Monoid Lovelace where
  mempty = Lovelace 0

instance B.Buildable Lovelace where
  build (Lovelace n) = bprint (int . " lovelace") n

instance ToCBOR Lovelace where
  toCBOR = toCBOR . unLovelace
  encodedSizeExpr size _pxy = encodedSizeExpr size (Proxy :: Proxy Word64)

instance FromCBOR Lovelace where
  fromCBOR = do
    l <- fromCBOR
    return $! Lovelace (fromIntegral (l :: Word64))

instance Monad m => Canonical.ToJSON m Lovelace where
  toJSON = Canonical.toJSON . unLovelace

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m Lovelace where
  fromJSON = fmap (Lovelace . (fromIntegral :: Word64 -> Natural))
           . Canonical.fromJSON

naturalToLovelace :: Natural -> Lovelace
naturalToLovelace = Lovelace

lovelaceToNatural :: Lovelace -> Natural
lovelaceToNatural (Lovelace n) = n

lovelaceToInteger :: Lovelace -> Integer
lovelaceToInteger = toInteger . lovelaceToNatural

data LovelaceError
  = LovelaceOverflow Word64
  | LovelaceTooLarge Integer
  | LovelaceTooSmall Integer
  | LovelaceUnderflow Word64 Word64
  deriving (Data, Eq, Show)

instance B.Buildable LovelaceError where
  build = \case
    LovelaceOverflow c -> bprint
      ("Lovelace value, " . build . ", overflowed")
      c
    LovelaceTooLarge c -> bprint
      ("Lovelace value, " . build . ", exceeds maximum, " . build)
      c
      maxLovelaceVal
    LovelaceTooSmall c -> bprint
      ("Lovelace value, " . build . ", is less than minimum, " . build)
      c
      (Lovelace 0)
    LovelaceUnderflow c c' -> bprint
      ("Lovelace underflow when subtracting " . build . " from " . build)
      c'
      c

instance ToCBOR LovelaceError where
  toCBOR = \case
    LovelaceOverflow c ->
      encodeListLen 2 <> toCBOR @Word8 0 <> toCBOR c
    LovelaceTooLarge c ->
      encodeListLen 2 <> toCBOR @Word8 1 <> toCBOR c
    LovelaceTooSmall c ->
      encodeListLen 2 <> toCBOR @Word8 2 <> toCBOR c
    LovelaceUnderflow c c' ->
      encodeListLen 3 <> toCBOR @Word8 3 <> toCBOR c <> toCBOR c'

instance FromCBOR LovelaceError where
  fromCBOR = do
    len <- decodeListLen
    let checkSize size = matchSize "LovelaceError" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 2 >> LovelaceOverflow <$> fromCBOR
      1 -> checkSize 2 >> LovelaceTooLarge <$> fromCBOR
      2 -> checkSize 2 >> LovelaceTooSmall <$> fromCBOR
      3 -> checkSize 3 >> LovelaceUnderflow <$> fromCBOR <*> fromCBOR
      _ -> cborError $ DecoderErrorUnknownTag "TxValidationError" tag

-- | Maximal possible value of 'Lovelace'
maxLovelaceVal :: Word64
maxLovelaceVal = 45e15

-- | Lovelace formatter which restricts type.
lovelaceF :: Format r (Lovelace -> r)
lovelaceF = build

-- | Compute sum of all lovelace in container. Result is 'Integer' as a
--   protection against possible overflow.
sumLovelace :: Foldable t => t Lovelace -> Lovelace
sumLovelace = fold

-- | Addition of lovelace.
addLovelace :: Lovelace -> Lovelace -> Lovelace
addLovelace (Lovelace a) (Lovelace b) = Lovelace (a + b)

-- | Subtraction of lovelace, returning 'LovelaceError' on underflow
subLovelace :: Lovelace -> Lovelace -> Either LovelaceError Lovelace
subLovelace (Lovelace a) (Lovelace b)
  | a >= b    = Right (Lovelace (a - b))
  | otherwise = Left (LovelaceUnderflow (fromIntegral a) (fromIntegral b))

-- | Scale a 'Lovelace' by an 'Natural' factor.
scaleLovelace :: Lovelace -> Natural -> Lovelace
scaleLovelace (Lovelace a) b = Lovelace (a * b)

-- | Scale a 'Lovelace' by a 'Rational' factor between @0..1@, rounding down.
scaleLovelaceRational :: Lovelace -> Rational -> Lovelace
scaleLovelaceRational (Lovelace a) b =
    Lovelace (a * n `div` d)
  where
    n, d :: Natural
    n = fromInteger (numerator b)
    d = fromInteger (denominator b)

-- | Integer division of a 'Lovelace' by an 'Natural' factor
divLovelace :: Lovelace -> Natural -> Lovelace
divLovelace (Lovelace a) b = Lovelace (a `div` b)

-- | Integer modulus of a 'Lovelace' by an 'Natural' factor
modLovelace :: Lovelace -> Natural -> Lovelace
modLovelace (Lovelace a) b = Lovelace (a `mod` b)

