{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Control.State.Transition.Goblin where

import Control.Applicative (liftA2)
import Control.Lens
import Control.Monad.State.Strict (StateT)
import Data.Int
import Data.List (splitAt)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Typeable (Typeable)
import Data.TypeRepMap
import qualified Data.TypeRepMap as TM
import GHC.Generics
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Moo.GeneticAlgorithm.Binary (bitsNeeded, decodeBinary)
import Moo.GeneticAlgorithm.Types (Genome)
import Numeric.Natural (Natural)


data GoblinData g = GoblinData
  { -- | Remaining genes, controlling how a goblin operates
    _genes       :: !(Genome g)
    -- | A goblin's bag of tricks contains items of many differnt types. When
    -- tinkering, a goblin (depending on its genome) might look in its bag of
    -- tricks to see whether it has anything of the appropriate type to replace
    -- what it's currently tinkering with (or, depending on the type, do
    -- something different - for example, utilise a monoid instance to add
    -- things together).
  , _bagOfTricks :: !(TypeRepMap [])
  }
makeLenses 'GoblinData

-- | Tinker monad
type TinkerM g = StateT (GoblinData g) Gen

class GeneOps g => Goblin g a where
  -- | Tinker with an item of type 'a'.
  tinker
    :: a
    -> TinkerM g a

  default tinker
    :: (Generic a, GGoblin g (Rep a))
    => a
    -> TinkerM g a
  tinker a = GHC.Generics.to <$> gTinker (GHC.Generics.from a)

  -- | As well as tinkering, goblins can conjure fresh items into existence.
  conjure :: TinkerM g a

  default conjure
    :: (Generic a, GGoblin g (Rep a))
    => TinkerM g a
  conjure = GHC.Generics.to <$> gConjure

-- | Construct a tinker function given a set of possible things to do.
--
--   Each 'toy' is a function taking the original value and one grabbed from the
--   bag of tricks or conjured.
tinkerWithToys
  :: (Goblin g a, Typeable a, GeneOps g) => [a -> a -> a] -> (a -> TinkerM g a)
tinkerWithToys toys =
  let
    defaultToys = [const, flip const]
    allToys     = defaultToys ++ toys
  in \a -> do
    toy <- (allToys !!) <$> transcribeGenesAsInt (length allToys - 1)
    toy a <$> rummageOrConjure

--------------------------------------------------------------------------------
-- Gene operations
--------------------------------------------------------------------------------

-- | Read (and consume) a gene from the genome
transcribeGene :: TinkerM g g
transcribeGene = do
  g <- use genes
  case g of
    [] -> error "Genome has run out! Try increasing the size of the genome."
    (x : xs) -> do
      genes .= xs
      return x

class GeneOps g where
  -- | Choose between two actions based on the value of a gene
  onGene
       -- | When gene is on
    :: TinkerM g a
       -- | When gene is off
    -> TinkerM g a
    -> TinkerM g a

  -- | Transcribe sufficient genes to get an integer in the range [0..n].
  transcribeGenesAsInt
    :: Int
    -> TinkerM g Int

instance GeneOps Bool where

  onGene yes no = do
    tg <- transcribeGene
    if tg then yes else no

  transcribeGenesAsInt n = do
    (gs, xs) <- splitAt (bitsNeeded (0,n)) <$> use genes
    genes .= xs
    return $ decodeBinary (0, n) gs `mod` (n+1)

--------------------------------------------------------------------------------
-- Bag of tricks
--------------------------------------------------------------------------------

-- | Fetch something from the bag of tricks if there's something there.
rummage :: forall a g . Typeable a => TinkerM g (Maybe a)
rummage = do
  bag <- use bagOfTricks
  case TM.lookup bag of
    Nothing -> return Nothing
    Just xs -> Just <$> Gen.element xs

-- | Fetch everything from the bag of tricks.
rummageAll :: forall a g . Typeable a => TinkerM g [a]
rummageAll = do
  bag <- use bagOfTricks
  case TM.lookup bag of
    Nothing -> return []
    Just xs -> return xs

-- | Fetch something from the bag of tricks, or else conjure it up.
rummageOrConjure :: forall a g . (Typeable a, Goblin g a) => TinkerM g a
rummageOrConjure = maybe conjure return =<< rummage

--------------------------------------------------------------------------------
-- Generic goblins
--------------------------------------------------------------------------------

class GGoblin g f where
  gTinker :: f a -> TinkerM g (f a)
  gConjure :: TinkerM g (f a)

-- gRummageOrConjure
--   :: (Generic a, Goblin g a, GGoblin g f, f ~ Rep a)
--   => TinkerM g (f a)
-- gRummageOrConjure = GHC.Generics.from <$> rummageOrConjure

instance GGoblin g V1 where
  gTinker = return
  gConjure = error "Cannot conjure a void type"

instance GGoblin g U1 where
  gTinker = return
  gConjure = return U1

instance Goblin g c => GGoblin g (K1 i c) where
  gTinker x = K1 <$> tinker (unK1 x)
  gConjure = K1 <$> conjure

instance GGoblin g f => GGoblin g (M1 i t f) where
  gTinker (M1 x) = M1 <$> gTinker x
  gConjure = M1 <$> gConjure

-- TODO In the 'tinker' implementations here, we would like to rummageOrConjure
-- rather than just conjuring when we switch to teh other branch
instance (GeneOps g, GGoblin g a, GGoblin g b) => GGoblin g (a :+: b) where
  gTinker (L1 x) = onGene (R1 <$> gConjure) (L1 <$> gTinker x)
  gTinker (R1 x) = onGene (L1 <$> gConjure) (R1 <$> gTinker x)

  gConjure = onGene (L1 <$> gConjure) (R1 <$> gConjure)

instance (GeneOps g, GGoblin g a, GGoblin g b) => GGoblin g (a :*: b) where
  gTinker (a :*: b) = liftA2 (:*:) (onGene (gTinker a) (return a)) (onGene (gTinker b) (return b))
  gConjure = liftA2 (:*:) gConjure gConjure

--------------------------------------------------------------------------------
-- Primitive goblins
--------------------------------------------------------------------------------

instance GeneOps a => Goblin a Bool
instance GeneOps a => Goblin a Char where
  tinker b = onGene rummageOrConjure conjure
  conjure = Gen.unicodeAll

instance GeneOps a => Goblin a Natural where
  tinker = tinkerWithToys [(+), (*)]
  conjure = toEnum . abs <$> conjure

instance GeneOps a => Goblin a Int where
  tinker = tinkerWithToys [(+), (-), (*)]
  conjure = Gen.int (Range.constantFrom 0 (-1000) 1000)

--------------------------------------------------------------------------------
-- Composite goblins
--------------------------------------------------------------------------------

instance (Goblin g a, Goblin g b) => Goblin g (a,b)

instance (Integral a, Goblin g a) => Goblin g (Ratio a) where
  tinker obj = do
    n <- tinker $ numerator obj
    d <- tinker $ denominator obj
    return $ n % d
  conjure = (%) <$> conjure <*> conjure

instance Goblin g a => Goblin g (Maybe a)

-- | Our list goblin behaves slightly differently, since it pulls whole lists of
-- things from the bag of tricks, and is also specialised to do some more
-- messing about with lists.
instance (Eq a, Typeable a, GeneOps g, Goblin g a) => Goblin g [a] where
  tinker obj = do
      toy <- (toys !!) <$> transcribeGenesAsInt (length toys - 1)
      toy obj =<< rummageAll
     where
       -- Toys for lists can use 'TinkerM', because they might be random
       -- toys :: [[a] -> [a] -> TinkerM g [a]]
       toys =
         [ \a _ -> return a
         , \_ b -> return b
         , \a _ -> Gen.shuffle a
         , \a b -> (a ++) <$> Gen.subsequence b
         , \a b -> (a List.\\) <$> Gen.subsequence b
         , \a b -> (++) <$> Gen.subsequence a <*> Gen.subsequence b
         ]

  conjure = Gen.list (Range.constantFrom 5 0 15) conjure

instance (Goblin g a, Ord a, Typeable a) =>  Goblin g (Set.Set a) where
  tinker obj = do
      toy <- (toys !!) <$> transcribeGenesAsInt (length toys - 1)
      toy obj =<< rummageAll
     where
       -- Toys for sets can use 'TinkerM', because they might be random
       -- toys :: [Set.Set a -> [a] -> TinkerM g (Set.Set a)]
       toys =
         [ \a _ -> return a
         , \_ b -> return $ Set.fromList b
         , \a b -> Set.difference a . Set.fromList <$> Gen.subsequence b
         , \a b -> Set.union a . Set.fromList <$> Gen.subsequence b
         , \a b -> return $ a `Set.intersection` Set.fromList b
         ]

  conjure = Gen.set (Range.constantFrom 5 0 15) conjure

instance (Goblin g k, Goblin g v, Ord k, Eq k, Eq v, Typeable k, Typeable v)
  => Goblin g (Map.Map k v) where
    tinker obj = do
        toy <- (toys !!) <$> transcribeGenesAsInt (length toys - 1)
        key <- rummageAll
        val <- rummageAll
        toy obj key val
      where
        -- Toys for sets can use 'TinkerM', because they might be random
        -- toys :: [Map.Map k v -> [k] -> [v] -> TinkerM g (Map.Map k v)]
        toys =
          [ \a _ _ -> return a
          , \a k v -> do
              ks <- Gen.subsequence k
              vs <- Gen.subsequence v
              return $ Map.union a (Map.fromList $ zip ks vs)
          , \a k _ -> Map.withoutKeys a . Set.fromList <$> Gen.subsequence k
          , \a k _ -> Map.restrictKeys a . Set.fromList <$> Gen.subsequence k
          ]

    conjure = Gen.map (Range.constantFrom 5 0 15) conjure

--------------------------------------------------------------------------------
-- Training goblins
--------------------------------------------------------------------------------

 -- | Spawn a goblin from a given genome and a bag of tricks.
spawnGoblin :: Genome g -> TypeRepMap [] -> GoblinData g
spawnGoblin = GoblinData
