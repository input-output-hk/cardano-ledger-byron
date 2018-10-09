module Cardano.Chain.Update.Proof
       ( Proof
       , mkProof
       ) where

import           Cardano.Crypto (Hash, hash)

import           Cardano.Chain.Update.Payload

-- | Proof that body of update message contains 'UpdatePayload'.
type Proof = Hash Payload

mkProof :: Payload -> Proof
mkProof = hash
