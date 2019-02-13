{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests whether the block validation implementation matches the formal
-- specification.
module Test.Cardano.Chain.Block.Validation.Spec
  ( tests
  , passConcreteValidation
  )
where

import Cardano.Prelude hiding (trace)
import Control.Lens ((^.))

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
import qualified Control.State.Transition as Transition
import Control.State.Transition.Trace
import qualified Control.State.Transition.Trace as Trace
import Cardano.Chain.Block as Concrete
import Cardano.Spec.Chain.STS.Rule.Chain
import qualified Cardano.Spec.Chain.STS.Block as Abstract

import Test.Cardano.Chain.Config (readMainetCfg)
import qualified Test.Cardano.Chain.Block.Elaboration as E

tests :: IO Bool
tests = checkParallel $$discover

-- | Every abstract chain that was generated according to the inference rules,
-- after being elaborated must be validated by the concrete block validator.
prop_generatedChainsAreValidated :: Property
prop_generatedChainsAreValidated = property $ do
  config <- readMainetCfg -- TODO: you might want to generate this genesis
                          -- config from the intial abstract environment (which
                          -- will be contained in the trace)
  forAll trace >>= passConcreteValidation config

passConcreteValidation
  :: MonadTest m
  => Genesis.Config
  -> Trace CHAIN -> m ()
passConcreteValidation config tr = do
  initSt <- evalEither $ Concrete.initialChainValidationState config
  let
    res = foldM elaborateAndUpdate initSt $ Trace.preStatesAndSignals OldestFirst tr
  void $ evalEither res
  where
    elaborateAndUpdate
      :: Concrete.ChainValidationState
      -> (Transition.State CHAIN, Abstract.Block)
      -> Either Concrete.ChainValidationError Concrete.ChainValidationState
    elaborateAndUpdate cst (ast, ab) =
      Concrete.updateChain config cst (E.elaborateBS gh aenv ast cst ab)
      where
        gh = Genesis.configGenesisHash config
        aenv = tr ^. traceEnv
