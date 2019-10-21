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

module Cardano.Chain.Common.Lovelace
  (
  -- * Lovelace
    Lovelace

  -- * Constructors
  , mkLovelace
  , maxLovelaceVal

  -- * Formatting
  , lovelaceF

  -- * Conversions
  , lovelaceToInteger
  , integerToLovelace

  -- * Arithmetic operations
  , sumLovelace
  , addLovelace
  , subLovelace
  , scaleLovelace
  , divLovelace
  , modLovelace
  )
where

import Cardano.Prelude

import Data.Data (Data)
import Formatting (Format, bprint, build, int)
import qualified Formatting.Buildable as B
import qualified Text.JSON.Canonical as Canonical
  (FromJSON(..), ReportSchemaErrors, ToJSON(..))

import Cardano.Binary
  ( FromCBOR(..)
  , ToCBOR(..)
  )


-- | Lovelace is the least possible unit of currency
newtype Lovelace = Lovelace { getLovelace :: Integer }
  deriving (Eq, Ord, Show, Generic, Data, NFData, NoUnexpectedThunks)

instance B.Buildable Lovelace where
  build (Lovelace n) = bprint (int . " lovelace") n

instance ToCBOR Lovelace where
  toCBOR = toCBOR . getLovelace --Will encode as big int if needed
  encodedSizeExpr size pxy = size (getLovelace <$> pxy)

instance FromCBOR Lovelace where
  fromCBOR = mkLovelace <$> fromCBOR  --Will fail if encounters big int

instance Monad m => Canonical.ToJSON m Lovelace where
  toJSON = Canonical.toJSON . getLovelace

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m Lovelace where
  fromJSON = fmap Lovelace . Canonical.fromJSON

-- | Maximal possible value of 'Lovelace'
maxLovelaceVal :: Integer
maxLovelaceVal = 45e15

-- | Constructor for 'Lovelace'
mkLovelace :: Word64 -> Lovelace
mkLovelace c = Lovelace (fromIntegral c)
{-# INLINE mkLovelace #-}

-- | Lovelace formatter which restricts type.
lovelaceF :: Format r (Lovelace -> r)
lovelaceF = build

-- | Compute sum of all lovelace in container. Result is 'Integer' as a
--   protection against possible overflow.
sumLovelace :: Foldable t => t Lovelace -> Lovelace
sumLovelace = integerToLovelace . foldl' (\a x -> a + getLovelace x) 0
{-# INLINE sumLovelace #-}

lovelaceToInteger :: Lovelace -> Integer
lovelaceToInteger = getLovelace
{-# INLINE lovelaceToInteger #-}

-- | Addition of lovelace
addLovelace :: Lovelace -> Lovelace -> Lovelace
addLovelace (Lovelace a) (Lovelace b) = Lovelace (a + b)
{-# INLINE addLovelace #-}

-- | Subtraction of lovelace
subLovelace :: Lovelace -> Lovelace -> Lovelace
subLovelace (Lovelace a) (Lovelace b) = Lovelace (a - b)
{-# INLINE subLovelace #-}

-- | Scale a 'Lovelace' by an 'Integral' factor
scaleLovelace :: Integral b => Lovelace -> b -> Lovelace
scaleLovelace (Lovelace a) b = integerToLovelace (a * toInteger b)
{-# INLINE scaleLovelace #-}

-- | Integer division of a 'Lovelace' by an 'Integral' factor
divLovelace :: Integral b => Lovelace -> b -> Lovelace
divLovelace (Lovelace a) b = integerToLovelace (a `div` toInteger b)
{-# INLINE divLovelace #-}

-- | Integer modulus of a 'Lovelace' by an 'Integral' factor
modLovelace :: Integral b => Lovelace -> b -> Lovelace
modLovelace (Lovelace a) b = integerToLovelace (a `mod` toInteger b)
{-# INLINE modLovelace #-}

integerToLovelace :: Integer -> Lovelace
integerToLovelace = Lovelace
