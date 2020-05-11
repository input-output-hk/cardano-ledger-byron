{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.CBOR
  ( tests
  , CanonicalTestable (..)
  , TypeTerm (..)
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Test.Cardano.Binary.Helpers.CanonicalTestable
  ( CanonicalTestable (..)
  , TypeTerm (..)
  )
import Test.Cardano.Binary.Helpers.GoldenRoundTrip (goldenTestCBORCanonicalAll)
import Hedgehog (Group(..), Property)
import Test.Options (concatGroups)

import Cardano.Chain.Block
import Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Update as Update
import Cardano.Chain.Slotting
import Cardano.Chain.UTxO
import Cardano.Chain.Common

goldenAll :: Property
goldenAll = goldenTestCBORCanonicalAll (Just "term.txt")
    [
      WithCBORSized (Proxy :: Proxy Body) (stdArgs 2)
    , WithCBORSized (Proxy :: Proxy TxPayload) (stdArgs 2)
    , WithCBORSized (Proxy :: Proxy Delegation.Payload) (stdArgs 2)
    , WithCBORSized (Proxy :: Proxy Update.Payload) (stdArgs 2)
    , WithCBORSized (Proxy :: Proxy ProposalBody) (stdArgs 2)
    , WithCBORSized (Proxy :: Proxy ProtocolParametersUpdate) (stdArgs 2)
    , WithCBOR (Proxy :: Proxy EpochNumber)
    , WithCBOR (Proxy :: Proxy SlotNumber)
    , WithCBOR (Proxy :: Proxy SoftforkRule)
    , WithCBOR (Proxy :: Proxy TxFeePolicy)
    , WithCBOR (Proxy :: Proxy LovelacePortion)
    , WithCBOR (Proxy :: Proxy ProtocolVersion)
    , WithCBOR (Proxy :: Proxy Delegation.Certificate)
    , WithCBOR (Proxy :: Proxy TxPayload)
    , WithCBOR (Proxy :: Proxy BlockSignature)
--    , WithCBOR (Proxy :: Proxy Certificate)
    ]
    []

tests :: Group
tests = concatGroups [($$discoverGolden)]
