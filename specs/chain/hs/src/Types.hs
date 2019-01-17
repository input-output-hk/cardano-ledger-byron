{-# LANGUAGE OverloadedStrings #-}
-- | Defines basic types for working with the ledger and the blockchain
module Types
  ( BlockIx(..)
  , PParams(PParams)
  , maxBlockSize
  , maxHeaderSize
  , Block(..)
  , genesisHash
  )
where

import Data.Set (Set)
import Numeric.Natural
import Crypto.Hash (hash)
import Data.ByteString (ByteString)

import Ledger.Core (VKey, Sig, Slot, VKeyGenesis)
import Ledger.Delegation (DCert)
import Ledger.Signatures (Hash)


newtype BlockIx = MkBlockIx Natural deriving (Eq, Ord, Show)

data PParams = PParams
  { maxBlockSize  :: !Natural
  -- ^ Maximum block size in bytes.
  , maxHeaderSize :: !Natural
  -- ^ Maximum block header size in bytes.
  }

genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = hash ("" :: ByteString)

-- | Block type for two kinds of blocks: a genesis block and a
-- non-genesis block
data Block
  -- | Epoch boundary block
  = EBB {
    prevHash :: Hash
    -- ^ Hash of the previous block. This will be the @genesisHash@
    }
  -- | Regular block
  | RBlock {
      rbHash   :: Hash -- ^ Hash of the predecessor block
    , rbIx     :: BlockIx -- ^ Index of the block
    , rbSigner :: VKey -- ^ Block signer
    , rbCerts  :: [DCert] -- ^ New certificates posted to the blockchain
    , rbSl     :: Slot -- ^ Slot in which the block was issued
    , rbData   :: Hash -- ^ Body of the block
      -- NOTE(md): rbData shouldn't be of type Hash, but some sensible type.
      -- Until that type is pinned down, it is left as a Hash so that calls
      -- to the @verify@ function type-check on the body data of a block
    , rbSig    :: Sig Hash -- ^ Cryptographic signature of the block
    , rbIsEBB  :: Bool -- ^ Indicates if this is an epoch boundary block
    , rbSize   :: Natural -- ^ Size of the block
    , rbHeaderSize :: Natural -- ^ Size of the header of the block
    } deriving (Show)
