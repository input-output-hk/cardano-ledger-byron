{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Cardano.Chain.Common.Lovelace
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (discover, forAll, property)

import Cardano.Chain.Common
  ( addLovelace
  , subLovelace
  , naturalToLovelace
  , lovelaceToNatural
  )

import Test.Cardano.Chain.Common.Gen (genLovelace, genCustomLovelace)
import Test.Options (TSGroup, TSProperty, concatTSGroups, withTestsTS)


ts_prop_subLovelace :: TSProperty
ts_prop_subLovelace = withTestsTS 1000 . property $ do
  a <- forAll genLovelace
  b <- forAll $ genCustomLovelace (fromIntegral (lovelaceToNatural a))
  assertIsJust $ subLovelace a b

ts_prop_subLovelaceUnderflow :: TSProperty
ts_prop_subLovelaceUnderflow =
  withTestsTS 1000
    . property
    $ do a <- forAll genLovelace
         assertIsNothing (subLovelace a (addLovelace a (naturalToLovelace 1)))

tests :: TSGroup
tests = concatTSGroups [const $$discover, $$discoverPropArg]

