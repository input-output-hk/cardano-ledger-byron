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

import Data.Data (Constr, toConstr)

import Hedgehog (discover, forAll, property)

import Cardano.Chain.Common
  ( LovelaceError(LovelaceUnderflow)
  , addLovelace
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
  assertIsRight $ subLovelace a b

ts_prop_subLovelaceUnderflow :: TSProperty
ts_prop_subLovelaceUnderflow =
  withTestsTS 1000
    . property
    $ do a <- forAll genLovelace
         assertIsLeftConstr dummyLovelaceUnderflow
           (subLovelace a (addLovelace a (naturalToLovelace 1)))

tests :: TSGroup
tests = concatTSGroups [const $$discover, $$discoverPropArg]


--------------------------------------------------------------------------------
-- Dummy values for constructor comparison in assertIsLeftConstr tests
--------------------------------------------------------------------------------

dummyLovelaceUnderflow :: Constr
dummyLovelaceUnderflow = toConstr $ LovelaceUnderflow 1 1
