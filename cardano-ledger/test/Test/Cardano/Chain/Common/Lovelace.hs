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
import Formatting (build, sformat)

import Hedgehog (Property, (===), discover, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Common
  ( Lovelace
  , LovelaceError(..)
  , addLovelace
  , subLovelace
  , naturalToLovelace
  , lovelaceToNatural
  )

import Test.Cardano.Chain.Common.Gen (genLovelace, genCustomLovelace)
import Test.Options (TSGroup, TSProperty, concatTSGroups, withTestsTS)


-- TODO: This will be removed soon since overflow will no longer be possible.
maxLovelaceVal :: Word64
maxLovelaceVal = 45e15

ts_prop_addLovelace :: TSProperty
ts_prop_addLovelace = withTestsTS 1000 . property $ do
  a <- forAll genLovelace
  let newRange = maxLovelaceVal - fromIntegral (lovelaceToNatural a)
  b <- forAll $ genCustomLovelace newRange
  assertIsRight $ addLovelace a b


prop_maxLovelaceUnchanged :: Property
prop_maxLovelaceUnchanged =
  property $ (fromIntegral maxLovelaceVal :: Integer) === 45e15

ts_prop_subLovelace :: TSProperty
ts_prop_subLovelace = withTestsTS 1000 . property $ do
  a <- forAll genLovelace
  b <- forAll $ genCustomLovelace (fromIntegral (lovelaceToNatural a))
  assertIsRight $ subLovelace a b

ts_prop_subLovelaceUnderflow :: TSProperty
ts_prop_subLovelaceUnderflow =
  withTestsTS 1000
    . property
    $ do
        a <- forAll genLovelace
        case addLovelace a (naturalToLovelace 1) of
          Right added ->
            assertIsLeftConstr dummyLovelaceUnderflow (subLovelace a added)
          Left err -> panic $ sformat
            ("The impossible happened in subLovelaceUnderflow: " . build)
            err

tests :: TSGroup
tests = concatTSGroups [const $$discover, $$discoverPropArg]


--------------------------------------------------------------------------------
-- Dummy values for constructor comparison in assertIsLeftConstr tests
--------------------------------------------------------------------------------

dummyLovelaceTooSmall :: Constr
dummyLovelaceTooSmall = toConstr $ LovelaceTooSmall 1

dummyLovelaceUnderflow :: Constr
dummyLovelaceUnderflow = toConstr $ LovelaceUnderflow 1 1
