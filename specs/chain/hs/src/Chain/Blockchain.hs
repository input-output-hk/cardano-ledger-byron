{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings #-}
module Chain.Blockchain where

import Control.Lens
import Crypto.Hash (hash, hashlazy)
import Data.Bits (shift)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Hedgehog.Gen (integral, double)
import Hedgehog.Range (constant, linear)
import Numeric.Natural

import Control.State.Transition
  ( Embed
  , Environment
  , IRC(IRC)
  , PredicateFailure
  , STS
  , Signal
  , State
  , TRC(TRC)
  , (?!)
  , judgmentContext
  , initialRules
  , trans
  , transitionRules
  , wrapFailed
  )
import Control.State.Transition.Generator (HasTrace, initEnvGen, sigGen)

import Ledger.Core
  ( Epoch
  , Sig
  , Slot
  , SlotCount(SlotCount)
  , VKey
  , VKeyGenesis
  , VKeyGenesis
  , verify
  )
import Ledger.Delegation (DIState, DELEG, DIEnv, delegationMap, DCert)
import Ledger.Signatures (Hash)

-- | Protocol parameters.
--
data PParams = PParams -- TODO: this should be a module of @cs-ledger@.
  { maxBlockSize  :: !Natural
  -- ^ Maximum block size in bytes.
  , maxHeaderSize :: !Natural
  -- ^ Maximum block header size in bytes.
  }

genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = hash ("" :: ByteString)

data BlockHeader
  = BlockHeader
  { _prevHHash :: Hash
    -- ^ Hash of the previous block header, or 'genesisHash' in case of the
    -- first block in a chain.
  , _bSlot :: Slot
    -- ^ Absolute slot for which the block was generated.
  , _bEpoch :: Epoch
    -- ^ Epoch for which the block was generated.

  , _bIssuer :: VKey
    -- ^ Block issuer.

  , _bSig :: Sig VKey
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

-- | Returns a key from a map for a given value.
maybeMapKeyForValue :: (Eq a, Ord k) => a -> Map.Map k a -> Maybe k
maybeMapKeyForValue v = listToMaybe . map fst . Map.toList . Map.filter (== v)

-- | Computes the hash of a block
hashBlock :: Block -> Hash
hashBlock = hashlazy . pack . show
-- TODO: we might want to serialize this properly, without using show...

-- | The 't' parameter in K * t in the range 0.2 <= t <= 0.25
-- that limits the number of blocks a signer can signed in a
-- block sliding window of size K
newtype T = MkT Double deriving (Eq, Ord)

-- | Environment for blockchain rules
data BlockchainEnv = MkBlockChainEnv
  {
    bcEnvPp    :: PParams
  , bcEnvDIEnv :: DIEnv
  , bcEnvK     :: SlotCount
  -- ^ Size of the moving window: how many block-signers before the current
  -- block do we consider when computing the total proportion of blocks a given
  -- key signed (see 'bcEnvT').
  , bcEnvT     :: T
  -- ^ Percentage of blocks (normalized to [0, 1]) that any given key can sign.
  }

-- | Block chain extension rules
data CEState
  = CEState
  { _signers :: [VKeyGenesis]
  , _prevBlockHash :: Hash
  , _pps :: PParams
  , _delegState :: DIState
  }

makeLenses ''CEState

data CHAIN

instance STS CHAIN where
  -- | The state comprises a map of genesis block verification keys to a queue
  -- of at most K blocks each key signed in a sliding window of size K,
  -- the previous block and the delegation interface state
  type State CHAIN = CEState
  -- | Transitions in the system are triggered by a new block
  type Signal CHAIN = Block
  -- | The environment consists of K and t parameters. To support a state
  -- transition subsystem, the environment also includes the environment of the
  -- subsystem.
  type Environment CHAIN = BlockchainEnv
  data PredicateFailure CHAIN
    = InvalidPredecessor
    | NoDelegationRight
    | InvalidBlockSignature
    | InvalidBlockSize
    | InvalidHeaderSize
    | SignedMaximumNumberBlocks
    | LedgerFailure (PredicateFailure DELEG)
    deriving (Eq, Show)

  -- There are only two inference rules: 1) for the initial state and 2) for
  -- extending the blockchain by a new block
  initialRules =
    [ do
        IRC env <- judgmentContext
        initDIState <- trans @DELEG $ IRC (bcEnvDIEnv env)
        return undefined
    ]
  transitionRules =
    [ do
        TRC jc <- judgmentContext
        undefined
    ]

instance Embed DELEG CHAIN where
  wrapFailed = LedgerFailure

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Verification keys located in the genesis block
--
-- initVKeys :: Set VKeyGenesis
-- initVKeys = fromList $ map (VKeyGenesis . VKey . Owner) [1 .. 7]


instance HasTrace CHAIN where
  initEnvGen
    = MkBlockChainEnv
    -- In the testnet the maximum block size is set to 2000000 and the maximum
    -- header size is set to 2000, so we have to make sure we cover those
    -- values here. The upper bound is arbitrary though.
    <$> (PParams <$> integral (constant 0 4000000) <*> integral (constant 0 4000))
    <*> initEnvGen @DELEG
    -- The size of the rolling widow is arbitrarily determined.
    <*> (SlotCount <$> integral (linear 0 10))
    -- The percentage of the slots will typically be between 1/5 and 1/4,
    -- however we want to stretch that range a bit for testing purposes.
    <*> (MkT <$> double (constant (1/6) (1/3)))

  sigGen _e _st = undefined
