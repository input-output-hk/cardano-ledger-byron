{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests whether the block validation implementation matches the formal
-- specification.
module Test.Cardano.Chain.Block.Validation.Spec
  ( tests
  )
where

import Cardano.Prelude hiding (trace)

import Hedgehog
  ( MonadTest
  , Property
  , checkParallel
  , discover
  , evalEither
  , forAll
  , property
  )

import qualified Cardano.Chain.Genesis as Genesis
import Control.State.Transition.Generator
import Control.State.Transition.Trace
import Cardano.Chain.Block as Concrete
import Cardano.Spec.Chain.STS.Rule.Chain
import qualified Cardano.Spec.Chain.STS.Block as Abstract

import Test.Cardano.Chain.Config (readMainetCfg)

tests :: IO Bool
tests = checkParallel $$discover

-- | Every abstract chain that was generated according to the inference rules,
-- after being elaborated must be validated by the concrete block validator.
prop_generatedChainsAreValidated :: Property
prop_generatedChainsAreValidated = property $ do
  config <- readMainetCfg
  forAll trace >>= passConcreteValidation config
  where
    passConcreteValidation :: MonadTest m => Genesis.Config -> Trace CHAIN -> m ()
    passConcreteValidation config tr = do
      initSt <- evalEither $ Concrete.initialChainValidationState config
      let
        res = foldM elaborateAndUpdate initSt $ traceSignals OldestFirst tr
      void $ evalEither res
      where

        elaborateAndUpdate
          :: Concrete.ChainValidationState
          -> Abstract.Block
          -> Either Concrete.ChainValidationError Concrete.ChainValidationState
        elaborateAndUpdate cst ast =
          Concrete.updateChain config cst (elaborate cst ast)

    -- TODO: this will be replaced with the actual elaborate function.
    -- TODO: the elaborate function might need some other parameters, like the
    -- previous block.
    elaborate
      :: Concrete.ChainValidationState
      -> Abstract.Block
      -> Concrete.ABlock ByteString
    elaborate = undefined
