{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
module UTxO where

import           Crypto.Hash (Digest, SHA256)
import qualified Data.ByteArray        as BA
import Data.Monoid (Sum(..))

type Hash = Digest SHA256

newtype Coin = Coin Int
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Int)

-- |The address of a transaction output, used to identify the owner.
newtype Addr = Addr Hash
  deriving (Show, Eq, Ord)
