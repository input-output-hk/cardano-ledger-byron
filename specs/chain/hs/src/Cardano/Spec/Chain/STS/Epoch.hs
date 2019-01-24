{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Epoch where

import Data.Map.Strict (Map)

import Control.State.Transition

import Ledger.Core

data EPOCH

instance STS EPOCH where
  type Environment EPOCH = Map VKeyGenesis VKey
  type State EPOCH = Epoch
  type Signal EPOCH = Slot
  data PredicateFailure EPOCH = X
    deriving (Eq, Show)
  initialRules = []
  transitionRules =
    [ do
        TRC (_, _, s) <- judgmentContext
        return $! sEpoch s
    ]

-- | Compute the epoch for the given _absolute_ slot
sEpoch :: Slot -> Epoch
sEpoch (Slot s) = Epoch $ s `div` slotsPerEpoch
  where
    -- Hardcoded number of slots per epoch, as per Byron.
    slotsPerEpoch = 21600
