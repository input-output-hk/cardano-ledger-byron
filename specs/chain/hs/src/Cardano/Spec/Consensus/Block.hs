{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Type classes for interfacing with the consensus layer
module Cardano.Spec.Consensus.Block where

import           Control.Lens ((^.))

import qualified Cardano.Spec.Chain.STS.Block as CBM -- Concrete Block Module
import           Ledger.Core
import           Ledger.Delegation
import           Ledger.Signatures


class BlockHeader h where
  -- | Hash of the previous block header, or 'genesisHash' in case of
  -- the first block in a chain.
  bhPrevHash :: h -> Hash
  -- | Header hash
  bhHash :: h -> Hash
  -- | Signature of the block by its issuer.
  bSig :: h -> Sig VKey
  -- | Block issuer.
  bIssuer :: h -> VKey
  -- | Slot for which this block is issued
  bhSlot :: h -> Slot


class BlockBody bb where
  -- | Delegation certificates.
  bCerts :: bb -> [DCert]


class ( BlockHeader (FamBlockHeader b)
      , BlockBody (FamBlockBody b)
      ) => Block b where
  type family FamBlockHeader b :: *
  type family FamBlockBody   b :: *

  -- | Gets the block header
  bHeader :: b -> FamBlockHeader b
  -- | Gets the block body
  bBody   :: b -> FamBlockBody b


instance BlockBody CBM.BlockBody where
  bCerts (CBM.BlockBody bDCerts) = bDCerts

instance BlockHeader CBM.BlockHeader where
  bhPrevHash h = h ^. CBM.prevHHash
   -- TODO: a corresponding field as of Feb 11, 2019 does not exist in
   -- the CBM.BlockHeader type
  bhHash       = undefined
  bSig       h = h ^. CBM.bSig
  bIssuer    h = h ^. CBM.bIssuer
  bhSlot     h = h ^. CBM.bSlot

instance Block CBM.Block where
  type FamBlockHeader CBM.Block = CBM.BlockHeader
  type FamBlockBody   CBM.Block = CBM.BlockBody

  bHeader b = b ^. CBM.bHeader
  bBody   b = b ^. CBM.bBody
