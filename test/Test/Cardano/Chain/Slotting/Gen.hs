module Test.Cardano.Chain.Slotting.Gen
       ( genEpochIndex
       , genFlatSlotId
       , genLocalSlotIndex
       , genSlotCount
       , genSlotId
       , feedPMEpochSlots
       ) where

import           Cardano.Prelude

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Chain.Slotting (EpochIndex (..), FlatSlotId,
                     LocalSlotIndex (..), SlotCount (..), SlotId (..),
                     localSlotIndexMaxBound, localSlotIndexMinBound)
import           Cardano.Crypto (ProtocolMagic)

import           Test.Cardano.Crypto.Gen (genProtocolMagic)


genEpochIndex :: Gen EpochIndex
genEpochIndex = EpochIndex <$> Gen.word64 Range.constantBounded

genFlatSlotId :: Gen FlatSlotId
genFlatSlotId = Gen.word64 Range.constantBounded

genLocalSlotIndex :: SlotCount -> Gen LocalSlotIndex
genLocalSlotIndex epochSlots = UnsafeLocalSlotIndex
    <$> Gen.word16 (Range.constant lb ub)
  where
    lb = getSlotIndex localSlotIndexMinBound
    ub = getSlotIndex (localSlotIndexMaxBound epochSlots)

genSlotCount :: Gen SlotCount
genSlotCount = SlotCount <$> Gen.word64 Range.constantBounded

genSlotId :: SlotCount -> Gen SlotId
genSlotId epochSlots =
    SlotId <$> genEpochIndex <*> genLocalSlotIndex epochSlots

feedPMEpochSlots :: (ProtocolMagic -> SlotCount -> Gen a) -> Gen a
feedPMEpochSlots genA = do
    pm         <- genProtocolMagic
    epochSlots <- genSlotCount
    genA pm epochSlots
