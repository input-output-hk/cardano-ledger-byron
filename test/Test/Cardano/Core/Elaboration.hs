

module Test.Cardano.Core.Elaboration
  ( elaborateKeyPair
  , vKeyPair
  , elaborateVKey
  , elaborateVKeyGenesis
  )
where

import Cardano.Crypto.Signing (PublicKey, SecretKey)
import Ledger.Core (KeyPair, Owner(Owner), VKey, VKeyGenesis)

elaborateKeyPair :: KeyPair -> (PublicKey, SecretKey)
elaborateKeyPair kp = deterministicKeyGen $ padSeed seed
 where
  Owner o = owner $ sKey kp
  padSeed s =
    let padLength = max 0 (32 - BS.length s) in BS.replicate padLength 0 <> s
  seed = BSL.toStrict . toLazyByteString . integerDec $ fromIntegral o

vKeyPair :: VKey -> KeyPair
vKeyPair (VKey o) = keyPair o

elaborateVKey :: VKey -> PublicKey
elaborateVKey = fst . elaborateKeyPair . vKeyPair

elaborateVKeyGenesis :: VKeyGenesis -> PublicKey
elaborateVKeyGenesis = elaborateVKey . coerce
