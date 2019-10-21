{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Genesis.NonAvvmBalances
  ( GenesisNonAvvmBalances(..)
  , convertNonAvvmDataToBalances
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(..))
import qualified Data.Map.Strict as M
import Formatting (bprint, build)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary (DecoderError)
import Cardano.Chain.Common
  ( Address
  , Lovelace
  , addLovelace
  , fromCBORTextAddress
  , integerToLovelace
  )


-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
  { unGenesisNonAvvmBalances :: Map Address Lovelace
  } deriving (Show, Eq, NoUnexpectedThunks)

instance B.Buildable GenesisNonAvvmBalances where
  build (GenesisNonAvvmBalances m) =
    bprint ("GenesisNonAvvmBalances: " . mapJson) m

deriving instance Semigroup GenesisNonAvvmBalances
deriving instance Monoid GenesisNonAvvmBalances

instance Monad m => ToJSON m GenesisNonAvvmBalances where
  toJSON = toJSON . unGenesisNonAvvmBalances

instance MonadError SchemaError m => FromJSON m GenesisNonAvvmBalances where
  fromJSON = fmap GenesisNonAvvmBalances . fromJSON

data NonAvvmBalancesError = NonAvvmBalancesDecoderError DecoderError

instance B.Buildable NonAvvmBalancesError where
  build (NonAvvmBalancesDecoderError err) =
     bprint
      ("Failed to decode NonAvvmBalances.\n Error: " . build)
      err

-- | Generate genesis address distribution out of avvm parameters. Txdistr of
--   the utxo is all empty. Redelegate it in calling function.
convertNonAvvmDataToBalances
  :: forall m
   . MonadError NonAvvmBalancesError m
  => Map Text Integer
  -> m GenesisNonAvvmBalances
convertNonAvvmDataToBalances balances = fmap GenesisNonAvvmBalances $ do
  converted <- traverse convert (M.toList balances)
  return $! mkBalances converted
 where
  mkBalances :: [(Address, Lovelace)] -> Map Address Lovelace
  mkBalances = M.fromListWith addLovelace

  convert :: (Text, Integer) -> m (Address, Lovelace)
  convert (txt, i) = do
    addr <- fromCBORTextAddress txt `wrapError` NonAvvmBalancesDecoderError
    let lovelace = integerToLovelace i
    return (addr, lovelace)
