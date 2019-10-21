{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Common.Lovelace
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property, (===), discover, property)

import Cardano.Chain.Common (maxLovelaceVal)

import Test.Options (TSGroup, concatTSGroups)


prop_maxLovelaceUnchanged :: Property
prop_maxLovelaceUnchanged =
  property $ maxLovelaceVal === (45e15 :: Integer)

tests :: TSGroup
tests = concatTSGroups [const $$discover, $$discoverPropArg]

