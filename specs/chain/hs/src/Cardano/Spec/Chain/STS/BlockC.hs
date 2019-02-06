module Cardano.Spec.Chain.STS.BlockC where

import Ledger.Core
import Ledger.Delegation
import Ledger.Signatures


class BlockHeaderC h where
  -- | Hash of the previous block header, or 'genesisHash' in case of
  -- the first block in a chain.
  prevHash :: h -> Hash
  -- | Absolute slot for which the block was generated.
  bSlot :: h -> Slot
  -- | Block issuer.
  bIssuer :: h -> VKey
  -- | Signature of the block by its issuer.
  bSig :: h -> Sig VKey


class BlockBodyC bb where
  -- | Delegation certificates.
  bDCerts :: bb -> [DCert]


class BlockC b where
  -- | Gets the block header
  bHeader :: BlockHeaderC h => b -> h
  -- | Gets the block body
  bBody :: BlockBodyC bb => b -> bb
