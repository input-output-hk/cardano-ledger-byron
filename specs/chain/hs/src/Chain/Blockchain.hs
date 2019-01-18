{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings #-}
module Chain.Blockchain where

import Control.Lens (makeLenses, (^.))
import Crypto.Hash (hash, hashlazy)
import Data.Bits (shift)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Hedgehog.Gen (integral, double, set)
import Hedgehog.Range (constant, linear)
import Numeric.Natural

import Cardano.Prelude (HeapWords, heapWords, heapWords2, heapWords4)

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
  ( Epoch(Epoch)
  , Sig
  , Slot(Slot)
  , SlotCount(SlotCount)
  , VKey
  , VKeyGenesis
  , VKeyGenesis
  , verify
  )
import Ledger.Core.Generator (vkgenesisGen)
import Ledger.Delegation
  ( DCert
  , DELEG
  , DIEnv
  , DIState
  , DSEnv (DSEnv)
  , _dSEnvAllowedDelegators
  , _dSEnvEpoch
  , _dSEnvLiveness
  , _dSEnvSlot
  , delegationMap
  )
import Ledger.Signatures (Hash)

-- | Protocol parameters.
--
data PParams = PParams -- TODO: this should be a module of @cs-ledger@.
  { _maxBkSz  :: !Natural
  -- ^ Maximum (abstract) block size in words.
  , _maxHrdSz :: !Natural
  -- ^ Maximum (abstract) block header size in words.
  , _dLiveness :: !SlotCount
  -- ^ Delegation liveness parameter: number of slots it takes a delegation
  -- certificate to take effect.
  , _bkSgnCntW :: !Natural
  -- ^ Size of the moving window to count signatures.
  , _bkSgnCntT :: !Double
  -- ^ Fraction [0, 1] of the blocks that can be signed by any given key in a
  -- window of lenght '_bkSgnCntW'. This value will be typically between 1/5
  -- and 1/4.
  } deriving (Eq, Show)

makeLenses ''PParams

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

-- | Blockchain extension environment.
data CEEnv -- TODO: note that we only have to define an environment to be able
           -- to fit the generators framework as it is at the moment. This
           -- environment won't be used by the rules once the initial state is
           -- determined from it (actually copied).
  = CEEnv  { _initPps :: PParams
    -- ^ Initial protocol par_dSEnvAllowedDelegatorsameters.
  , _gKeys ::  Set VKeyGenesis
    -- ^ Initial genesis keys.
  } deriving (Eq, Show)

makeLenses ''CEEnv

-- | Blockchain extension state.
data CEState
  = CEState
  { _signers :: [VKeyGenesis]
  , _lastHHash :: Hash
  , _pps :: PParams
  , _delegState :: DIState
  }

makeLenses ''CEState

-- | Block chain extension rules
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
  type Environment CHAIN = CEEnv
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
        let dsenv
              = DSEnv
              { _dSEnvAllowedDelegators = env ^. gKeys
              , _dSEnvEpoch = Epoch 0
              , _dSEnvSlot = Slot 0
              , _dSEnvLiveness = env ^. initPps . dLiveness
              }
        initDIState <- trans @DELEG $ IRC dsenv
        return CEState
          { _signers = []
          , _lastHHash = genesisHash
          , _pps = env ^. initPps
          , _delegState = initDIState
          }
    ]
  transitionRules =
    [ do
        TRC (_, st, b) <- judgmentContext
        bSize b <= st ^. pps . maxBkSz ?! InvalidBlockSize
        undefined
    ]

instance Embed DELEG CHAIN where
  wrapFailed = LedgerFailure

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
    + heapWords4 (header ^. bSlot)
                 (header ^. bEpoch)
                 (header ^. bIssuer)
                 (header ^. bSig)

instance HeapWords BlockBody where
  heapWords body = undefined

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

instance HasTrace CHAIN where
  initEnvGen
    = do
    -- In mainet the maximum header size is set to 2000000 and the maximum
    -- block size is also set to 2000000, so we have to make sure we cover
    -- those values here. The upper bound is arbitrary though.
    mHSz <- integral (constant 0 4000000)
    mBSz <- integral (constant 0 4000000)
    -- The delegation liveness parameter is arbitrarily determined.
    d <- SlotCount <$> integral (linear 0 10)
    -- The size of the rolling widow is arbitrarily determined.
    w <- integral (linear 0 10)
    -- The percentage of the slots will typically be between 1/5 and 1/4,
    -- however we want to stretch that range a bit for testing purposes.
    t <- double (constant (1/6) (1/3))
    let initPPs
          = PParams
          { _maxHrdSz = mHSz
          , _maxBkSz = mBSz
          , _dLiveness = d
          , _bkSgnCntW = w
          , _bkSgnCntT = t
          }
    gKeys <- set (linear 1 7) vkgenesisGen
    return CEEnv
      { _initPps = initPPs
      , _gKeys = gKeys
      }

  sigGen _e _st = undefined
