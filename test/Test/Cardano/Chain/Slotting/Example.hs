module Test.Cardano.Chain.Slotting.Example
       ( exampleEpochIndex
       , exampleSlotId
       ) where

import           Cardano.Chain.Slotting (EpochIndex (..), LocalSlotIndex (..),
                     SlotId (..))

exampleEpochIndex :: EpochIndex
exampleEpochIndex = EpochIndex 14

exampleSlotId :: SlotId
exampleSlotId = SlotId (EpochIndex 11) (UnsafeLocalSlotIndex 47)
