
module Cardano.Spec.Chain.STS.Properties where

import Control.Lens ((^..))
import Data.List.Ordered (isSorted)
import Hedgehog (MonadTest, Property, forAll, property, (===), assert)

import Control.State.Transition.Generator
import Control.State.Transition.Trace

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.Chain

slotsIncrease :: Property
slotsIncrease = property $ forAll trace >>= slotsIncreaseInTrace

slotsIncreaseInTrace :: MonadTest m => Trace CHAIN -> m ()
slotsIncreaseInTrace tr = assert . isSorted $ slots
  where blocks = traceSignals NewestFirst tr
        slots = blocks ^.. traverse . bHeader . bSlot
