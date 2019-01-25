{-# LANGUAGE TemplateHaskell #-}

module Cardano.Spec.Chain.STS.Block where

import Control.Lens ((^.), makeLenses, view)
import Crypto.Hash (hash, hashlazy)
import Data.ByteString.Lazy.Char8 (pack)
import Numeric.Natural (Natural)

import Cardano.Prelude
  ( HeapWords
  , heapWords
  , heapWords1
  , heapWords2
  , heapWords3
  )

import Control.State.Transition.Generator
import Ledger.Core
import Ledger.Delegation
import Ledger.Signatures

data BlockHeader
  = BlockHeader
  { _prevHHash :: !Hash
    -- ^ Hash of the previous block header, or 'genesisHash' in case of the
    -- first block in a chain.
  , _bSlot :: !Slot
    -- ^ Absolute slot for which the block was generated.
  , _bIssuer :: !VKey
    -- ^ Block issuer.
  , _bSig :: !(Sig VKey)
    -- ^ Signature of the block by its issuer.

    -- TODO: BlockVersion – the block version; see Software and block versions.
    -- Block version can be associated with a set of protocol rules. Rules
    -- associated with _mehBlockVersion from a block are the rules used to
    -- create that block (i.e. the block must adhere to these rules).

    -- TODO: SoftwareVersion – the software version (see the same link); the
    -- version of software that created the block
  } deriving (Eq, Show)

makeLenses ''BlockHeader

data BlockBody
  = BlockBody
  { _bDCerts  :: [DCert]
  -- ^ Delegation certificates.
  } deriving (Eq, Show)

makeLenses ''BlockBody

-- | A block in the chain. The specification only models regular blocks since
-- epoch boundary blocks will be largely ignored in the Byron-Shelley bridge.
data Block
  = Block
  { _bHeader :: BlockHeader
  , _bBody :: BlockBody
  } deriving (Eq, Show)

makeLenses ''Block

-- | Compute the size (in words) that a block takes.
bSize :: Block -> Natural
bSize = fromInteger . toInteger . heapWords

instance HeapWords Block where
  heapWords b = heapWords2 (b ^. bHeader) (b ^. bBody)

instance HeapWords BlockHeader where
  heapWords header
    -- The constant 12 is was taken from:
    --
    -- https://github.com/input-output-hk/cardano-chain/pull/244/files#diff-2955aa8b04471dc586f90cb5f22948beR118
    --
    -- 12 = 8 words of digest + 4 words for hash
    = 12
    + heapWords3 (header ^. bSlot)
                 (header ^. bIssuer)
                 (header ^. bSig)

instance HeapWords BlockBody where
  heapWords body = heapWords1 (body ^. bDCerts)

-- | Compute the size (in words) that a block header.
bHeaderSize :: BlockHeader -> Natural
bHeaderSize = fromInteger . toInteger . heapWords

-- | Computes the hash of a header.
hashHeader :: BlockHeader -> Hash
hashHeader = hashlazy . pack . show
-- TODO: we might want to serialize this properly, without using show...


-- | Compute the epoch for the given _absolute_ slot
sEpoch :: Slot -> Epoch
sEpoch (Slot s) = Epoch $ s `div` slotsPerEpoch
  where
    -- Hardcoded number of slots per epoch, as per Byron.
    slotsPerEpoch = 21600

instance HasSizeInfo Block where
  isTrivial = null . view (bBody . bDCerts)
