{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Chain.Blockchain where

import Control.Lens
import qualified Data.Map.Strict as Map
import Data.Bits (shift)
import Numeric.Natural
import Crypto.Hash (hashlazy)
import Data.ByteString.Lazy.Char8 (pack)
import Hedgehog.Gen (integral, double)
import Hedgehog.Range (constant, linear)

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

import Data.Maybe (fromJust, listToMaybe, isJust)
import Data.Queue (Queue, headQueue, sizeQueue, pushQueue)
import Ledger.Core (SlotCount(SlotCount), verify, VKeyGenesis)
import Ledger.Delegation (DIState, DELEG, DIEnv, delegationMap)
import Ledger.Signatures (Hash)
import Types
  ( Block(..)
  , BlockIx(..)
  , PParams(PParams)
  , maxBlockSize
  , maxHeaderSize
  , genesisHash
  )

-- | Returns a key from a map for a given value.
maybeMapKeyForValue :: (Eq a, Ord k) => a -> Map.Map k a -> Maybe k
maybeMapKeyForValue v = listToMaybe . map fst . Map.toList . Map.filter (== v)

-- | Unsafely returns a key from a map for a given value. It assumes there is
-- exactly one key mapping to the given value. If there is no such key, it will
-- result in a runtime exception.
mapKeyForValue :: (Eq a, Ord k) => a -> Map.Map k a -> k
mapKeyForValue v = fromJust . maybeMapKeyForValue v


-- | Computes the hash of a block
hashBlock :: Block -> Hash
hashBlock = hashlazy . pack . show
-- TODO: we might want to serialize this properly, without using show...

-- | Computes the block size in bytes
bSize :: Block -> Natural
bSize (EBB{}) = 1
bSize b = 42 + (fromIntegral . length . rbCerts $ b)
  -- For now just a constant plus the number of delegation certificates. We
  -- might change this abstract size computation once things become clearer.

-- | Computes the block header size in bytes
bHeaderSize :: Block -> Natural
bHeaderSize b@(EBB{}) = 0
-- In the current rules we don't need the header size of an EBB, so we might
-- want to redefine the `Block` type and make this function more explicit on
-- the types it takes. We might want to define Block as Either EBB RBlock, and
-- define this function on RBlock's only.
bHeaderSize b@(RBlock{}) = rbHeaderSize b

-- | The 't' parameter in K * t in the range 0.2 <= t <= 0.25
-- that limits the number of blocks a signer can signed in a
-- block sliding window of size K
newtype T = MkT Double deriving (Eq, Ord)

-- Gives a map from delegator keys to a queue of block IDs of blocks that
-- the given key (indirectly) signed in the block sliding window of size K
type KeyToQMap = Map.Map VKeyGenesis (Queue BlockIx)


-- | Remove the oldest entry in the queues in the range of the map if it is
--   more than *K* blocks away from the given block index
trimIx :: KeyToQMap -> SlotCount -> BlockIx -> KeyToQMap
trimIx m (SlotCount k) ix = foldl (flip f) m (Map.keysSet m)
 where
  f :: VKeyGenesis -> KeyToQMap -> KeyToQMap
  f = Map.adjust (qRestrict ix)
  qRestrict :: BlockIx -> Queue BlockIx -> Queue BlockIx
  qRestrict (MkBlockIx ix') q = case headQueue q of
    Nothing               -> q
    Just (MkBlockIx h, r) -> if h + k < ix' then r else q

-- | Updates a map of genesis verification keys to their signed blocks in
-- a sliding window by adding a block index to a specified key's list
incIxMap :: BlockIx -> VKeyGenesis -> KeyToQMap -> KeyToQMap
incIxMap ix = Map.adjust (pushQueue ix)

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

-- | Extends a chain by a block
extendChain :: (Environment BC, State BC, Signal BC) -> DIState -> State BC
extendChain (env, st, b@(RBlock{})) dis =
  let
    p'             = hashBlock b
    (dienv, k, t ) = (bcEnvDIEnv env, bcEnvK env, bcEnvT env)
    (m    , p, ds) = st
    vk_d           = rbSigner b
    ix             = rbIx b
    vk_s           = mapKeyForValue vk_d . (^. delegationMap) $ ds
    m'             = incIxMap ix vk_s (trimIx m k ix)
  in (m', p', dis)

-- | Block chain extension rules
data BC

instance STS BC where
  -- | The state comprises a map of genesis block verification keys to a queue
  -- of at most K blocks each key signed in a sliding window of size K,
  -- the previous block and the delegation interface state
  type State BC = (KeyToQMap, Hash, DIState)
  -- | Transitions in the system are triggered by a new block
  type Signal BC = Block
  -- | The environment consists of K and t parameters. To support a state
  -- transition subsystem, the environment also includes the environment of the
  -- subsystem.
  type Environment BC = BlockchainEnv
  data PredicateFailure BC
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
        return (Map.empty, genesisHash, initDIState)
    ]
  transitionRules =
    [ do
        TRC jc <- judgmentContext
        validPredecessor jc ?! InvalidPredecessor
        validBlockSize jc ?! InvalidBlockSize
        validHeaderSize jc ?! InvalidHeaderSize
        hasRight jc ?! NoDelegationRight
        validSignature jc ?! InvalidBlockSignature
        lessThanLimitSigned jc
        dis <- trans @DELEG $ TRC (proj jc)
        return $ extendChain jc dis
    ]
    where
      -- valid predecessor
      validPredecessor (_, (_, prevBlockHash, _), b@(RBlock {})) =
         prevBlockHash == rbHash b
      -- has a block size within protocol limits
      validBlockSize (env, _, b) =
           bSize b <= blockSizeLimit (bcEnvPp env)
         where
           blockSizeLimit :: PParams -> Natural
           blockSizeLimit pp =
             if rbIsEBB b
               then 1 `shift` 21
               else maxBlockSize pp
      -- has a block header size within protocol limits
      validHeaderSize (env, _, b@(RBlock {})) =
        bHeaderSize b <= maxHeaderSize (bcEnvPp env)
      -- has a delegation right
      hasRight (_, (_, _, ds), b@(RBlock {})) =
        isJust $ maybeMapKeyForValue (rbSigner b) (ds ^. delegationMap)
      -- valid signature
      validSignature (_, _, b@(RBlock {})) =
         verify (rbSigner b) (rbData b) (rbSig b)
      -- the delegator has not signed more than an allowed number of blocks
      -- in a sliding window of the last k blocks
      lessThanLimitSigned (env, st, b@(RBlock {})) =
        let
          (m, _, ds) = st
          (SlotCount k, MkT t) = (bcEnvK env, bcEnvT env)
          vk_d = rbSigner b
          dsm = ds ^. delegationMap
        in case maybeMapKeyForValue vk_d dsm of
          Nothing   -> False ?! NoDelegationRight
          Just vk_s ->
            fromIntegral (sizeQueue (m Map.! vk_s)) <= fromIntegral k * t
              ?! SignedMaximumNumberBlocks
      proj (env, (_, _, d), b@(RBlock {})) = (bcEnvDIEnv env, d, rbCerts b)

instance Embed DELEG BC where
  wrapFailed = LedgerFailure

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

instance HasTrace BC where
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
