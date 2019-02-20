{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests whether the block validation implementation matches the formal
-- specification.
module Test.Cardano.Chain.Block.Validation.Spec
  ( tests
  , passConcreteValidation
  -- , passConcreteValidationIO
  -- , randomTrace
  )
where

import Cardano.Prelude hiding (trace)
import Data.Time (Day(ModifiedJulianDay), UTCTime(UTCTime))

import Control.Lens ((^.))
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

import qualified Cardano.Crypto.Hashing as H
import qualified Cardano.Chain.Genesis as Genesis
import Control.State.Transition.Generator
import qualified Control.State.Transition as Transition
import Control.State.Transition.Trace
import qualified Control.State.Transition.Trace as Trace
import Cardano.Chain.Block as Concrete
import Cardano.Spec.Chain.STS.Rule.Chain
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import qualified Ledger.Delegation as Deleg
import Ledger.Update (maxBkSz, maxHdrSz)

import Test.Cardano.Chain.Interpreter (elaborateVKeyGenesis)
import Test.Cardano.Crypto.Dummy (dummyProtocolMagic)
import Cardano.Chain.Common
  ( BlockCount(BlockCount)
  , LovelacePortion(LovelacePortion)
  , TxFeePolicy(TxFeePolicyUnknown)
  , mkStakeholderId
  )
import qualified Cardano.Chain.Update as Update
import Test.Cardano.Chain.Config (readMainetCfg)
import qualified Test.Cardano.Chain.Block.Elaboration as E

tests :: IO Bool
tests = checkParallel $$discover

-- | Every abstract chain that was generated according to the inference rules,
-- after being elaborated must be validated by the concrete block validator.
prop_generatedChainsAreValidated :: Property
prop_generatedChainsAreValidated = property $ do

--  todo "TODO: this is wrong ! We need to generate the config from the abstract environment!"
  -- Furthemore, we might want to weaken the preconditions of the Block and
  -- Delegation validation functions so that they only take the parts of the config they need.

  -- WRONG! config <- readMainetCfg
  forAll trace >>= passConcreteValidation

passConcreteValidation
  :: MonadTest m
  => Trace CHAIN -> m ()
passConcreteValidation tr = do
    -- We have to make an chain initial states ourselves, since
    -- initialChainValidationState makes a delegation transition

  let
    initSt = either (panic . show) identity $ initialChainValidationState config
    res = foldM elaborateAndUpdate initSt $ Trace.preStatesAndSignals OldestFirst tr
  void $ evalEither res
  where
    config = abEnvToCfg (tr ^. traceEnv)

    elaborateAndUpdate
      :: Concrete.ChainValidationState
      -> (Transition.State CHAIN, Abstract.Block)
      -> Either Concrete.ChainValidationError Concrete.ChainValidationState
    elaborateAndUpdate cst (ast, ab) =
      Concrete.updateChain config cst (E.elaborateBS config aenv ast cst ab)
      where
        aenv = tr ^. traceEnv

-- TODO: remove this function if not needed or remove duplication
-- passConcreteValidationIO
--   :: MonadIO m
--   => Trace CHAIN -> m ()
-- passConcreteValidationIO  tr = do
--   config <- readMainetCfg
--   let initSt =
--         either (panic . show) identity $ Concrete.initialChainValidationState config
--   let res =
--         foldM (elaborateAndUpdate config) initSt $
--         Trace.preStatesAndSignals OldestFirst tr
--   either (panic . show) (const $ return ()) res
--   where
--     elaborateAndUpdate
--       :: Genesis.Config
--       -> Concrete.ChainValidationState
--       -> (Transition.State CHAIN, Abstract.Block)
--       -> Either Concrete.ChainValidationError Concrete.ChainValidationState
--     elaborateAndUpdate config cst (ast, ab) =
--       Concrete.updateChain config cst (E.elaborateBS config aenv ast cst ab)
--       where
--         aenv = tr ^. traceEnv

-- randomTrace :: IO (Trace CHAIN)
-- randomTrace = Gen.sample trace

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
           case Map.keys $ Map.filter (== issuer) dm of -- TODO: factor out this repetition
             _:_ -> pure $! ()
             [] -> failure
           where
             issuer = bk ^. Abstract.bHeader . Abstract.bIssuer
             dm = st ^. disL . Deleg.delegationMap

--  | Make a config from the initial environment of the trace.
abEnvToCfg
  :: Transition.Environment CHAIN
  -> Genesis.Config
abEnvToCfg (_, vkgs, pps) = Genesis.Config genesisData genesisHash Nothing
  where
    genesisData
      = Genesis.GenesisData
      { Genesis.gdBootStakeholders =
          Genesis.GenesisWStakeholders genesisStakeHolders
      , Genesis.gdHeavyDelegation =
          Genesis.UnsafeGenesisDelegation [] -- We don't need initial heavyweight delegation.
      , Genesis.gdStartTime =
          UTCTime (ModifiedJulianDay 0) 0
      , Genesis.gdNonAvvmBalances =
          Genesis.GenesisNonAvvmBalances []
      , Genesis.gdProtocolParameters =
          gPps
      , Genesis.gdK =
        -- TODO: for now this is a constant. It will come from the environment
        -- once we add the update mechanism (which requires the K parameter).
          BlockCount 10 -- This is configSlotSecurityParam config
      , Genesis.gdProtocolMagic =
          dummyProtocolMagic
      , Genesis.gdAvvmDistr =
          Genesis.GenesisAvvmBalances []
      }

    -- We shouldn't need to use 'coerce' after
    -- https://github.com/input-output-hk/cardano-chain/issues/332 gets
    -- implemented.
    genesisHash = Genesis.GenesisHash $ coerce $ H.hash ("" :: ByteString)

    gPps
      = Update.ProtocolParameters
      { Update.ppScriptVersion = 0
      , Update.ppSlotDuration = 0
      , Update.ppMaxBlockSize = pps ^. maxBkSz
      , Update.ppMaxHeaderSize = pps ^. maxHdrSz
      , Update.ppMaxTxSize = 0
      , Update.ppMaxProposalSize = 0
      , Update.ppMpcThd = LovelacePortion 0
      , Update.ppHeavyDelThd = LovelacePortion 0
      , Update.ppUpdateVoteThd = LovelacePortion 0
      , Update.ppUpdateProposalThd = LovelacePortion 0
      , Update.ppUpdateImplicit = 0
      , Update.ppSoftforkRule =
          Update.SoftforkRule (LovelacePortion 0) (LovelacePortion 0) (LovelacePortion 0)
      , Update.ppTxFeePolicy =
          TxFeePolicyUnknown 0 "Fee policy unspecified at \"Test.Cardano.Chain.Block.Validation.Spec\""
      , Update.ppUnlockStakeEpoch = 0
      }

    genesisStakeHolders
      = Map.fromList
      $ zip (mkStakeholderId . elaborateVKeyGenesis <$> vkgs') [1..]

    vkgs' = Set.toList vkgs

-- | Make a concrete chain validation state from an abstract state.
--
-- TODO: we might not need this if we make a configuration from the genesis environment.
abStToInitSt
  :: Transition.State CHAIN
  -> Concrete.ChainValidationState
abStToInitSt = undefined
