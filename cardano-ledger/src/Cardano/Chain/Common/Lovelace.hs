{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Cardano.Chain.Common.Lovelace
  (
  -- * Lovelace
    Lovelace

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
  ( FromCBOR(..)
  , ToCBOR(..)
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
subLovelace :: Lovelace -> Lovelace -> Maybe Lovelace
subLovelace (Lovelace a) (Lovelace b)
  | a >= b    = Just $! Lovelace (a - b)
  | otherwise = Nothing

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

