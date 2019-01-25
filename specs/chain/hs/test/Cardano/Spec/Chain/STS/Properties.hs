
module Cardano.Spec.Chain.STS.Properties where

import Control.Lens ((^..))
import Hedgehog (MonadTest, Property, forAll, property, (===))

import Control.State.Transition.Generator
import Control.State.Transition.Trace

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.Chain

slotsIncrease :: Property
slotsIncrease = property $ forAll nonTrivialTrace >>= slotsIncreaseInTrace

slotsIncreaseInTrace :: MonadTest m => Trace CHAIN -> m ()
slotsIncreaseInTrace tr = -- blocks ^.. traverse . bHeader . bSlot === []
  blocks === []
  where blocks = traceSignals NewestFirst tr
