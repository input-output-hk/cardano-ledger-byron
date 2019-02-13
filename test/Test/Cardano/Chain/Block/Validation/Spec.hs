{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests whether the block validation implementation matches the formal
-- specification.
module Test.Cardano.Chain.Block.Validation.Spec
  ( tests
  , passConcreteValidation
  , passConcreteValidationIO
  , randomTrace
  )
where

import Cardano.Prelude hiding (trace)

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Hedgehog
  ( MonadTest
  , Property
  , checkParallel
  , discover
  , evalEither
  , forAll
  , property
  , failure
  )
import qualified Hedgehog.Gen as Gen

import qualified Cardano.Chain.Genesis as Genesis
import Control.State.Transition.Generator
import qualified Control.State.Transition as Transition
import Control.State.Transition.Trace
import qualified Control.State.Transition.Trace as Trace
import Cardano.Chain.Block as Concrete
import Cardano.Spec.Chain.STS.Rule.Chain
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import qualified Ledger.Delegation as Deleg

import Test.Cardano.Chain.Config (readMainetCfg)
import qualified Test.Cardano.Chain.Block.Elaboration as E

tests :: IO Bool
tests = checkParallel $$discover

-- | Every abstract chain that was generated according to the inference rules,
-- after being elaborated must be validated by the concrete block validator.
prop_generatedChainsAreValidated :: Property
prop_generatedChainsAreValidated = property $ do

  todo "TODO: this is wrong ! We need to generate the config from the abstract environment!"
  -- Furthemore, we might want to weaken the preconditions of the Block and
  -- Delegation validation functions so that they only take the parts of the config they need.

  -- WRONG! config <- readMainetCfg
  forAll trace >>= passConcreteValidation config

passConcreteValidation
  :: MonadTest m
  => Trace CHAIN -> m ()
passConcreteValidation tr = do
  config <- todo "Make a config from the initial environment of the trace"
  initSt <- evalEither $ todo "we have to make an chain initial states ourselves, since initialChainValidationState makes a delegation transition"
    -- WRONG! Concrete.initialChainValidationState config
  let
    res = foldM elaborateAndUpdate initSt $ Trace.preStatesAndSignals OldestFirst tr
  void $ evalEither res
  where
    elaborateAndUpdate
      :: Concrete.ChainValidationState
      -> (Transition.State CHAIN, Abstract.Block)
      -> Either Concrete.ChainValidationError Concrete.ChainValidationState
    elaborateAndUpdate cst (ast, ab) =
      Concrete.updateChain config cst (E.elaborateBS config aenv ast cst ab)
      where
        aenv = tr ^. traceEnv

-- TODO: remove this function if not needed or remove duplication
passConcreteValidationIO
  :: MonadIO m
  => Trace CHAIN -> m ()
passConcreteValidationIO  tr = do
  config <- readMainetCfg
  let initSt =
        either (panic . show) identity $ Concrete.initialChainValidationState config
  let res =
        foldM (elaborateAndUpdate config) initSt $
        Trace.preStatesAndSignals OldestFirst tr
  either (panic . show) (const $ return ()) res
  where
    elaborateAndUpdate
      :: Genesis.Config
      -> Concrete.ChainValidationState
      -> (Transition.State CHAIN, Abstract.Block)
      -> Either Concrete.ChainValidationError Concrete.ChainValidationState
    elaborateAndUpdate config cst (ast, ab) =
      Concrete.updateChain config cst (E.elaborateBS config aenv ast cst ab)
      where
        aenv = tr ^. traceEnv

randomTrace :: IO (Trace CHAIN)
randomTrace = Gen.sample trace

-- TODO: put this in the STS tests.
prop_blockIssuersAreDelegates :: Property
prop_blockIssuersAreDelegates =
  property $ forAll trace >>= blockIssuersAreDelegates
  where
    blockIssuersAreDelegates :: MonadTest m => Trace CHAIN -> m ()
    blockIssuersAreDelegates tr =
       traverse_ checkIssuer $ Trace.preStatesAndSignals OldestFirst tr
       where
         checkIssuer :: MonadTest m => (Transition.State CHAIN, Transition.Signal CHAIN) -> m ()
         checkIssuer (st, bk) =
           case M.keys $ M.filter (== issuer) dm of -- TODO: factor out this repetition
             _:_ -> pure $! ()
             [] -> failure
           where
             issuer = bk ^. Abstract.bHeader . Abstract.bIssuer
             dm = st ^. dis . Deleg.delegationMap
