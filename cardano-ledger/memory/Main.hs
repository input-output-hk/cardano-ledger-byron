{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import           Cardano.Prelude

import           System.FilePath       ((</>))
import           System.Mem            (performGC)

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.UTxO    as UTxO
import           Cardano.Crypto        (RequiresNetworkMagic (..),
                                        decodeAbstractHash)

main :: IO ()
main = do
    -- Load the Genesis Config
    !cfg <- fmap (either (panic . show) identity) $ runExceptT $
      Genesis.mkConfigFromFile RequiresNoMagic genesisFile $
      either panic identity $ decodeAbstractHash genesisHash

    -- It is /not/ in normal form
    print =<< (isNormalForm $! cfg)

    sleepSeconds 2

    -- Force it so that it is (hopefully?) in normal form
    !_ <- evaluate $! force $! cfg
    performGC

    -- It is in normal form, at least according to 'isNormalForm'
    print =<< (isNormalForm $! cfg)

    sleepSeconds 2

    -- Just make sure we hold on to the Genesis Config until the end of the
    -- program
    print =<< (isNormalForm $! cfg)
  where
    sleepSeconds :: Int -> IO ()
    sleepSeconds s = threadDelay (s * 1000 * 1000)

    genesisFile :: FilePath
    genesisFile = "cardano-ledger" </> "mainnet-genesis.json"

    genesisHash :: Text
    genesisHash =
        "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"


deriving instance NFData  UTxO.UTxOConfiguration
deriving instance Generic Genesis.GenesisAvvmBalances
deriving instance NFData  Genesis.GenesisAvvmBalances
deriving instance Generic Genesis.GenesisNonAvvmBalances
deriving instance NFData  Genesis.GenesisNonAvvmBalances
deriving instance Generic Genesis.GenesisDelegation
deriving instance NFData  Genesis.GenesisDelegation
deriving instance Generic Genesis.GenesisKeyHashes
deriving instance NFData  Genesis.GenesisKeyHashes
deriving instance Generic Genesis.GenesisData
deriving instance NFData  Genesis.GenesisData
deriving instance Generic Genesis.Config
deriving instance NFData  Genesis.Config
