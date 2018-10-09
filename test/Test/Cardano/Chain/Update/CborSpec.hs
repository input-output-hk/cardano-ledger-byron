{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.Update.CborSpec
       ( spec
       ) where

import           Cardano.Prelude

import           Test.Hspec (Spec, describe)

import           Cardano.Chain.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), SoftforkRule (..),
                     SoftwareVersion (..))

import           Test.Cardano.Binary.Helpers (binaryTest)
import           Test.Cardano.Chain.Update.Arbitrary ()

spec :: Spec
spec = describe "Cbor Bi instances" $ do
    binaryTest @ApplicationName
    binaryTest @BlockVersion
    binaryTest @BlockVersionData
    binaryTest @SoftforkRule
    binaryTest @SoftwareVersion
