{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Control.State.Transition.Goblin.BreedingPit where

import Control.Monad (join)
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.State.Transition
import Data.Either (lefts)
import Data.Functor.Identity (runIdentity)
import qualified Data.TypeRepMap as TM
import Hedgehog
import qualified Hedgehog.Internal.Gen as IGen
import qualified Hedgehog.Internal.Tree as ITree
import qualified Hedgehog.Range as Range
import Moo.GeneticAlgorithm.Binary
import Test.Goblin

breedStsGoblins
  :: forall s
   . (STS s, Goblin Bool (Signal s))
  => Gen (TRC s) -- ^ Generator for a scenario
  -> PredicateFailure s -- ^ Failure we are looking for
  -> IO (Population Bool)
breedStsGoblins jcGen wantedFailure =
  let
    popsize    = 101
    genomeSize = 64
    maxiters   = 10000

    genSize    = Range.Size 1
    genSeed    = Seed 12345 12345

    -- | Fitness function. This should run the goblins on a set of examples
    -- which we generate atop `initState`.
    fitness :: [Bool] -> Double
    fitness genome =
      maybe 0 ITree.nodeValue
        . runIdentity
        . runMaybeT
        . ITree.runTree
        . IGen.runGenT genSize genSeed
        $ do
            TRC (env, initState, sig) <- jcGen
            newSig                <- evalStateT (tinker sig) gd
            -- Apply the signal to the state (and environment)
            let
              jc      = TRC (env, initState, newSig)
              results = applyRule @s jc <$> transitionRules
            -- Score the result
            return $ scoreResult results
     where
      gd = spawnGoblin genome TM.empty
      scoreResult :: [Either [PredicateFailure s] (State s)] -> Double
      scoreResult result =
        -- Start at 100 points to make objective function positive
        -- 0 points for a rule passing
        -- -1 points for an unwanted predicate failure
        -- 5 points for a desired predicate failure
        let failures = join $ lefts result
        in
          fromIntegral
          $ (5 * length (filter (== wantedFailure) failures))
          - length failures
          + 100

    initialize = getRandomBinaryGenomes popsize genomeSize
    select     = stochasticUniversalSampling popsize
    crossover  = onePointCrossover 0.5
    mutate     = pointMutate 0.01
    evolve     = loop (Generations maxiters `Or` converged)
      $ nextGeneration Maximizing fitness select 0 crossover mutate
      where converged = IfObjective $ \fitvals -> minimum fitvals >= 104.0
  in do
    population <- runGA initialize evolve
    print (bestFirst Maximizing $ population)
    return population
