{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests whether the block validation implementation matches the formal
-- specification.
module Test.Cardano.Chain.Block.Validation.Spec
  ( tests
  , passConcreteValidation
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
xprop_generatedChainsAreValidated :: Property
xprop_generatedChainsAreValidated = property $ do
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
