{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.Ssc.CborSpec
       ( spec
       ) where

import           Cardano.Prelude

import           Test.Hspec (Spec, describe)

import           Cardano.Chain.Ssc (VssCertificate)

import           Test.Cardano.Binary.Helpers (binaryTest)
import           Test.Cardano.Chain.Ssc.Arbitrary ()

spec :: Spec
spec = describe "Cbor Bi instances" $ binaryTest @VssCertificate
