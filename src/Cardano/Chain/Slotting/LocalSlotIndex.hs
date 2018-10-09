{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Chain.Slotting.LocalSlotIndex
       ( LocalSlotIndex (..)

       , mkLocalSlotIndex
       , addLocalSlotIndex

       , localSlotIndexToEnum
       , localSlotIndexFromEnum
       , localSlotIndexSucc
       , localSlotIndexPred

       , localSlotIndexMinBound
       , localSlotIndexMaxBound
       , localSlotIndexMaxBoundExplicit
       , localSlotIndices

       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (throwError))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Ix (Ix)
import           Formatting.Buildable (Buildable)

import           Cardano.Binary.Class (Bi (..))

import           Cardano.Chain.Slotting.SlotCount (SlotCount)

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = UnsafeLocalSlotIndex
  { getSlotIndex :: Word16
  } deriving ( Show
             , Eq
             , Ord
             , Ix
             , Generic
             , Buildable
             , Typeable
             , NFData
             , Num
             , Integral
             , Real
             , Enum
             )

instance Bi LocalSlotIndex where
    encode = encode . getSlotIndex
    decode = UnsafeLocalSlotIndex <$> decode

deriveJSON defaultOptions ''LocalSlotIndex

localSlotIndexToEnum :: SlotCount -> Int -> LocalSlotIndex
localSlotIndexToEnum epochSlots i
    | i >= fromIntegral epochSlots = error
        "toEnum @LocalSlotIndex: greater than maxBound"
    | i < 0 = error "toEnum @LocalSlotIndex: less than minBound"
    | otherwise = UnsafeLocalSlotIndex (fromIntegral i)

localSlotIndexFromEnum :: LocalSlotIndex -> Int
localSlotIndexFromEnum = fromIntegral . getSlotIndex

localSlotIndexSucc :: SlotCount -> LocalSlotIndex -> LocalSlotIndex
localSlotIndexSucc epochSlots =
    localSlotIndexToEnum epochSlots . (+ 1) . localSlotIndexFromEnum

localSlotIndexPred :: SlotCount -> LocalSlotIndex -> LocalSlotIndex
localSlotIndexPred epochSlots =
    localSlotIndexToEnum epochSlots . subtract 1 . localSlotIndexFromEnum

localSlotIndexMinBound :: LocalSlotIndex
localSlotIndexMinBound = UnsafeLocalSlotIndex 0

localSlotIndexMaxBound :: SlotCount -> LocalSlotIndex
localSlotIndexMaxBound epochSlots =
    UnsafeLocalSlotIndex (fromIntegral epochSlots - 1)

localSlotIndexMaxBoundExplicit :: SlotCount -> LocalSlotIndex
localSlotIndexMaxBoundExplicit es = UnsafeLocalSlotIndex (fromIntegral es - 1)

-- | All local slot indices for the given number of slots in epoch, in ascending
-- order.
localSlotIndices :: SlotCount -> [LocalSlotIndex]
localSlotIndices slotsInEpoch = fmap UnsafeLocalSlotIndex [0..upperBound]
  where
    upperBound = fromIntegral slotsInEpoch - 1

mkLocalSlotIndex_ :: SlotCount -> Word16 -> Maybe LocalSlotIndex
mkLocalSlotIndex_ es idx
    | idx < fromIntegral es = Just (UnsafeLocalSlotIndex idx)
    | otherwise = Nothing

mkLocalSlotIndex :: MonadError Text m => SlotCount -> Word16 -> m LocalSlotIndex
mkLocalSlotIndex es idx = case mkLocalSlotIndex_ es idx of
    Just it -> pure it
    Nothing ->
        throwError
            $ "local slot is greater than or equal to the number of slots in epoch: "
            <> show idx

-- | Shift slot index by given amount, and return 'Nothing' if it has
-- overflowed past 'epochSlots'.
addLocalSlotIndex
    :: SlotCount -> SlotCount -> LocalSlotIndex -> Maybe LocalSlotIndex
addLocalSlotIndex epochSlots x (UnsafeLocalSlotIndex i)
    | s < fromIntegral epochSlots = Just (UnsafeLocalSlotIndex (fromIntegral s))
    | otherwise = Nothing
  where
    s :: Word64
    s = fromIntegral x + fromIntegral i
