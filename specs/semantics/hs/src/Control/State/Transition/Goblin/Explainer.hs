{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.State.Transition.Goblin.Explainer where

import Control.State.Transition
import Control.State.Transition.Goblin
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Functor.Identity (runIdentity)
import Data.TreeDiff
import Data.TreeDiff.Class
import qualified Data.TypeRepMap as TM
import Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Gen as IGen
import qualified Hedgehog.Internal.Tree as ITree

explainGoblin
  :: forall s
   . (STS s, Goblin Bool (Signal s), ToExpr (Signal s))
  => Signal s
  -> GoblinData Bool
  -> Maybe (Edit EditExpr)
explainGoblin sig goblin =
  fmap ITree.nodeValue
    . runIdentity
    . runMaybeT
    . ITree.runTree
    . IGen.runGenT genSize genSeed
    $ do
        newSig <- evalStateT (tinker sig) goblin
        return $ ediff sig newSig
 where
  genSize = Range.Size 1
  genSeed = Seed 12345 12345

explainGoblinGen
  :: forall s
   . (STS s, Goblin Bool (Signal s), ToExpr (Signal s))
  => Gen (Signal s)
  -> GoblinData Bool
  -> Maybe (Edit EditExpr)
explainGoblinGen sigGen goblin =
  fmap ITree.nodeValue
    . runIdentity
    . runMaybeT
    . ITree.runTree
    . IGen.runGenT genSize genSeed
    $ do
        sig    <- sigGen
        newSig <- evalStateT (tinker sig) goblin
        return $ ediff sig newSig
 where
  genSize = Range.Size 1
  genSeed = Seed 12345 12345
