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
  , assert
  , checkParallel
  , discover
  , forAll
  , property
  )

import qualified Cardano.Chain.Genesis as Genesis
import Control.State.Transition.Generator
import Control.State.Transition.Trace
import Cardano.Chain.Block as Concrete
import Cardano.Spec.Chain.STS.Rule.Chain
import qualified Cardano.Spec.Chain.STS.Block as Abstract

import Test.Cardano.Chain.Common.Utils (readMainetCfg)

tests :: IO Bool
tests = checkParallel $$discover

-- | Every abstract chain that was generated according to the inference rules,
-- after being interpreted must be validated by the concrete block validator.
prop_generatedChainsAreValidated :: Property
prop_generatedChainsAreValidated = property $ do
  config <- readMainetCfg
  forAll trace >>= areValidated config
  where
    areValidated :: MonadTest m => Genesis.Config -> Trace CHAIN -> m ()
    areValidated config tr = do
      let
        res = foldM interpretAndUpdate initSt $ traceSignals OldestFirst tr
      either (panic . show) (const $ pure ()) res
      where

        interpretAndUpdate
          :: Concrete.ChainValidationState
          -> Abstract.Block
          -> Either Concrete.ChainValidationError Concrete.ChainValidationState
        interpretAndUpdate cst ast =
          Concrete.updateChain config cst (interpret cst ast)

        initSt = either (panic . show) identity $
          Concrete.initialChainValidationState config

    -- TODO: this will be replaced with the actual interpreter.
    -- TODO: the interpreter might need some other parameters, like the
    -- previous block.
    interpret
      :: Concrete.ChainValidationState
      -> Abstract.Block
      -> Concrete.ABlock ByteString
    interpret = undefined
