{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
module Main (main) where

import Cardano.Chain.Common
import Cardano.Chain.UTxO
import Cardano.Crypto.Signing (VerificationKey, SigningKey)
import Cardano.Crypto.Signing.Safe (PassPhrase(..), safeDeterministicKeyGen)

import Criterion.Main

main :: IO ()
main =
  defaultMain [
    bgroup "CompactUTxO" [
      bench "toCompactTxId"      $ whnf toCompactTxId      exampleTxId
    , bench "fromCompactTxId"    $ whnf fromCompactTxId    exampleCompactTxId

    , bench "toCompactTxIn"      $ whnf toCompactTxIn      exampleTxIn
    , bench "fromCompactTxIn"    $ whnf fromCompactTxIn    exampleCompactTxIn

    , bench "toCompactAddress"   $ whnf toCompactAddress   exampleAddress
    , bench "fromCompactAddress" $ whnf fromCompactAddress exampleCompactAddress

    , bench "toCompactTxOut"     $ whnf toCompactTxOut     exampleTxOut
    , bench "fromCompactTxOut"   $ whnf fromCompactTxOut   exampleCompactTxOut
    ]
  ]

--
-- Tx Inputs
--

exampleTxId :: TxId
exampleTxId = "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25"

exampleCompactTxId :: CompactTxId
exampleCompactTxId = toCompactTxId exampleTxId


exampleTxIn :: TxIn
exampleTxIn = TxInUtxo exampleTxId 42

exampleCompactTxIn :: CompactTxIn
exampleCompactTxIn = toCompactTxIn exampleTxIn

--
-- Tx Outputs
--

exampleAddress :: Address
exampleAddress = makeAddress (VerKeyASD exampleVKey) examplAttrs

exampleCompactAddress :: CompactAddress
exampleCompactAddress = toCompactAddress exampleAddress

exampleTxOut :: TxOut
exampleTxOut =
    TxOut {
      txOutAddress = exampleAddress
    , txOutValue   = mkKnownLovelace @42424242
    }
  where

exampleCompactTxOut :: CompactTxOut
exampleCompactTxOut = toCompactTxOut exampleTxOut


exampleVKey :: VerificationKey
exampleSKey :: SigningKey
(exampleVKey, exampleSKey) =
  safeDeterministicKeyGen
    "example salt of at least 32 bytes length"
    (PassPhrase "example secret passphrase")

examplAttrs :: AddrAttributes
examplAttrs =
    AddrAttributes {
      aaNetworkMagic     = NetworkMainOrStage
    , aaVKDerivationPath = Just payload
    }
  where
    payload = HDAddressPayload "a legacy Byron Daedalus HD address payload"
