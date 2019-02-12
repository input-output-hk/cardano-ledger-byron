{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Spec.Chain.STS.Block where

import Control.Lens ((^.), makeLenses, view)
import Crypto.Hash (hashlazy)
import Data.AbstractSize
import Data.ByteString.Lazy.Char8 (pack)
import Data.Sequence ((<|))
import Data.Typeable (typeOf)
import Numeric.Natural (Natural)
import GHC.Generics (Generic)
import Control.State.Transition.Generator
import Ledger.Core
import Ledger.Delegation
import Ledger.Signatures

-- TODO(md): Define this type by seeing what is needed here in the
-- blockchain LaTeX document
data BHToSign deriving (Eq, Show, Generic)

data BlockProtocolHeader
  = MkBlockProtocolHeader
  {
    -- | Hash of the previous block header, or 'genesisHash' in case of
    -- the first block in a chain.
    _bhPrevHash :: Hash
    -- | Hash of the block.
  , _bhHash     :: Hash
    -- | Block issuer.
  , _bhIssuer   :: VKey
    -- | Part of the block header which must be signed.
  , _bhToSign   :: BHToSign
    -- | Signature of the block by its issuer.
  , _bhSig      :: !(Sig VKey)
  } deriving (Eq, Show, Generic)

makeLenses ''BlockProtocolHeader


-- Actually, there's nothing that is exclusive to the payload part of
-- the header so no need for this data structure
data BlockPayloadHeader

data BlockHeader
  = MkBlockHeader
  { -- | Absolute slot for which the block was generated.
    _bhSlot :: !Slot
    -- | Part of the header specific to the Permissive BFT protocol.
  , _bhPBFT :: !BlockProtocolHeader
    -- TODO: BlockVersion – the protocol (block) version that created the block

    -- TODO: SoftwareVersion – the software version that created the block
  } deriving (Eq, Show, Generic)

makeLenses ''BlockHeader

-- We declare a specific instance here to avoid recursing into cardano-crypto
instance HasTypeReps BlockHeader where
  typeReps x = typeOf x
               <| typeOf (undefined :: Hash)
               <| typeReps (x ^. bhSlot :: Slot)
               <> typeReps (x ^. bhPBFT ^. bhIssuer :: VKey)
               <> typeReps (x ^. bhPBFT ^. bhSig :: Sig VKey)

data BlockBody
  = BlockBody
  { _bDCerts  :: [DCert]
  -- ^ Delegation certificates.
  } deriving (Eq, Show, Generic)

instance HasTypeReps BlockBody
makeLenses ''BlockBody

-- | A block in the chain. The specification only models regular blocks since
-- epoch boundary blocks will be largely ignored in the Byron-Shelley bridge.
data Block
  = Block
  { _bHeader :: BlockHeader
  , _bBody :: BlockBody
  } deriving (Eq, Show, Generic)

instance HasTypeReps Block
makeLenses ''Block

-- | Compute the size (in words) that a block takes.
bSize :: Block -> Natural
bSize = fromIntegral . abstractSize acctMap
  where
    acctMap = []

-- | Compute the size (in words) that a block header.
bHeaderSize :: BlockHeader -> Natural
bHeaderSize = fromIntegral . abstractSize acctMap
  where
    acctMap = []

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
