{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Chain.Blockchain where

import Control.Lens (makeLenses, (^.), makeFields, (&), (.~))
import Crypto.Hash (hash, hashlazy)
import Data.Bits (shift)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import Hedgehog.Gen (integral, double, set, frequency, element)
import Hedgehog.Range (constant, linear, exponential)
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
  , failBecause
  , initialRules
  , judgmentContext
  , trans
  , transitionRules
  , wrapFailed
  )
import Control.State.Transition.Generator (HasTrace, initEnvGen, sigGen)

import Ledger.Core
  ( Epoch(Epoch)
  , Sig(Sig)
  , Slot(Slot)
  , SlotCount(SlotCount)
  , VKey
  , VKeyGenesis
  , VKeyGenesis
  , owner
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
  , dcertsGen
  )
import Ledger.Update
  ( PParams(PParams)
  , _bkSgnCntT
  , _bkSgnCntW
  , _bkSlotsPerEpoch
  , _dLiveness
  , _maxBkSz
  , _maxHdrSz
  , dLiveness
  , maxHdrSz
  , maxBkSz
  , bkSgnCntT
  , bkSgnCntW
  , bkSlotsPerEpoch
  )
import Ledger.Signatures (Hash)

import Cardano.Spec.Chain.STS.Block

genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = hash ("" :: ByteString)

--------------------------------------------------------------------------------
-- | Block epoch change rules
--------------------------------------------------------------------------------

data BEC

data BECState
  = BECState
  { _bECStateCurrSlot :: Slot
    -- ^ Current absolute slot.
  , _bECStateCurrEpoch :: Epoch
  }

makeFields ''BECState

-- instance STS BEC where
--   type Environment BEC = PParams
--   type State BEC = BECState
--   type Signal BEC = Block
--   data PredicateFailure BEC
--     = NoSlotInc
--     { _becCurrSlot :: Slot
--     -- ^ Current slot in the state.
--     , _becSignalSlot :: Slot
--     -- ^ Slot we saw in the signal.
--     }
--     -- ^ We haven't seen an increment in the slot
--     | PastEpoch
--     { _becCurrEpoch :: Epoch
--     , _becSignalEpoch :: Epoch
--     }
--     -- ^ Epoch in the signal occurs in the past.
--     deriving (Eq, Show)
--   initialRules = []
--   transitionRules =
--     [ do
--         TRC (cPps, st, b) <- judgmentContext
--         let es = cPps ^. bkSlotsPerEpoch
--             e  = st ^. currEpoch
--             e' = b ^. bHeader . bEpoch
--         e <= e' ?! PastEpoch e e'
--         let
--           slot = st ^. currSlot
--           slot' :: Slot
--           -- TODO: This doesn't seem right, but neither does unpacking an
--           -- `Epoch`, `Slot` and `SlotCount`.
--           slot' = Slot $
--             coerce e' -  coerce e * coerce es + coerce (b ^. bHeader . bRelSlot)
--         slot < slot' ?! NoSlotInc slot slot'
--         return $ st & currSlot .~ slot'
--                     & currEpoch .~ e'

--     ]

--------------------------------------------------------------------------------
-- | Block head rules
--------------------------------------------------------------------------------

data BHEAD

instance STS BHEAD where
  type Environment BHEAD = PParams
  type State BHEAD = Hash
  type Signal BHEAD = BlockHeader
  data PredicateFailure BHEAD
    = HashesDontMatch -- TODO: Add fields so that users know the two hashes that don't match
    | HeaderSizeTooBig -- TODO: Add more information here as well.
    deriving (Eq, Show)
  initialRules = []
  transitionRules =
    [ do
        TRC (cPps, h, bh) <- judgmentContext
        let sMax = cPps ^. maxHdrSz
        bHeaderSize bh <= sMax ?! HeaderSizeTooBig
        bh ^. prevHHash == h ?! HashesDontMatch
        return $ hashHeader bh
    ]

--------------------------------------------------------------------------------
-- | Signers counting rule
--------------------------------------------------------------------------------

data SIGCNT

data SCEnv
  = SCEnv
  { _sCEnvPps :: PParams
  , _sCEnvDms :: Map VKeyGenesis VKey
  }

makeFields ''SCEnv

instance STS SIGCNT where
  type Environment SIGCNT = SCEnv
  type State SIGCNT = Seq VKeyGenesis
  type Signal SIGCNT = VKey
  data PredicateFailure SIGCNT
    = TooManyIssuedBlocks VKeyGenesis -- The given genesis key issued too many blocks.
    | NotADelegate
    -- ^ The key signing the block is not a delegate of a genesis key.
    | TooManyDelegators
    -- ^ The key signing the block is delegated by multiple genesis keys.
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC (env, sgs, vk) <- judgmentContext
        let w = env ^. pps . bkSgnCntW
            t = env ^. pps . bkSgnCntT
        case Map.keys (Map.filter (== vk) (env ^.dms)) of
          [] -> do
            failBecause NotADelegate
            return sgs -- TODO: this is a quite inconvenient encoding for this transition system!
          [vkG] -> do
            let sgs' = S.drop (S.length sgs + 1 - w) (sgs |> vkG)
                nrSignedBks = fromIntegral (S.length (S.filter (==vkG) sgs'))
            nrSignedBks <= fromIntegral w * t ?! TooManyIssuedBlocks vkG
            return sgs'
          (_:_) -> do
            failBecause TooManyDelegators
            return sgs
    ]
--------------------------------------------------------------------------------
-- | Blockchain extension rules
--------------------------------------------------------------------------------

-- | Blockchain extension environment.
data CEEnv
  = CEEnv
  { _initPps :: PParams -- TODO: it can be confusing to have this in the
                        -- environment. It will be used by the initial rule
                        -- only, and then we'll have to drag it for eternity.
                        -- The state contains the up to date protocol
                        -- parameters.
    -- ^ Initial protocol par_dSEnvAllowedDelegatorsameters.
  , _gKeys ::  Set VKeyGenesis
    -- ^ Initial genesis keys.
  } deriving (Eq, Show)

makeLenses ''CEEnv

-- | Blockchain extension state.
data CEState
  = CEState
  { _cEStateCurrSlot :: Slot
    -- ^ Current absolute slot.
  , _cEStateCurrEpoch :: Epoch
  , _cEStateLastHHash :: Hash
  , _cEStateSigners :: Seq VKeyGenesis
  , _cEStatePps :: PParams
  , _cEStateDelegState :: DIState
  }

makeFields ''CEState

data CHAIN

instance STS CHAIN where
  type Environment CHAIN = CEEnv
  type State CHAIN = CEState
  type Signal CHAIN = Block
  data PredicateFailure CHAIN
    = InvalidPredecessor
    | NoDelegationRight
    | InvalidBlockSignature
    | InvalidBlockSize
    | InvalidHeaderSize
    | SignedMaximumNumberBlocks
    | LedgerFailure (PredicateFailure DELEG)
    | BHEADFailure (PredicateFailure BHEAD)
    | SIGCNTFailure (PredicateFailure SIGCNT)
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
          { _cEStateCurrSlot = Slot 0
          , _cEStateCurrEpoch = Epoch 0
          , _cEStateLastHHash = genesisHash
          , _cEStateSigners = []
          , _cEStatePps = env ^. initPps
          , _cEStateDelegState = initDIState
          }
    ]
  transitionRules =
    [ do
        TRC (env, st, b) <- judgmentContext
        bSize b <= st ^. pps . maxBkSz ?! InvalidBlockSize
        -- let subSt = BECState (st ^. currSlot) (st ^. currEpoch)
        -- becSt <- trans @BEC $ TRC (st ^. pps, subSt, b)
        h' <- trans @BHEAD $ TRC (st ^. pps, st ^. lastHHash, b ^. bHeader)
        let scEnv = SCEnv (st ^. pps) (st ^. delegState . delegationMap)
        sgs' <- trans @SIGCNT
                      $ TRC (scEnv, st ^. signers, b ^. bHeader . bIssuer)
        let diEnv
              = DSEnv
              { _dSEnvAllowedDelegators = env ^. gKeys
              , _dSEnvEpoch = st ^. currEpoch
              , _dSEnvSlot = st ^. currSlot
              , _dSEnvLiveness = st ^. pps . dLiveness
              }
        ds' <- trans @DELEG
                     $ TRC (diEnv, st ^. delegState, b ^. bBody . bDCerts)
        return $ st
--               & currSlot -- .~ (becSt ^. currSlot)
--               & currEpoch -- .~ (becSt ^. currEpoch)
               & lastHHash .~ h'
               & signers .~ sgs'
               & delegState .~ ds'
    ]

instance Embed DELEG CHAIN where
  wrapFailed = LedgerFailure

instance Embed BHEAD CHAIN where
  wrapFailed = BHEADFailure

instance Embed SIGCNT CHAIN where
  wrapFailed = SIGCNTFailure

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
    -- The slots per-epoch is arbitrarily determined.
    spe <- SlotCount <$> integral (linear 0 1000)
    let initPPs
          = PParams
          { _maxHdrSz = mHSz
          , _maxBkSz = mBSz
          , _dLiveness = d
          , _bkSgnCntW = w
          , _bkSgnCntT = t
          , _bkSlotsPerEpoch = spe
          }
    initGKeys <- set (linear 1 7) vkgenesisGen
    return CEEnv
      { _initPps = initPPs
      , _gKeys = initGKeys
      }

  sigGen env st = do
    -- We'd expect the slot increment to be close to 1, even for large Gen's
    -- size numbers
    slotInc <- integral (exponential 1 10)
    -- We'd expect the epoch not to change most of the times, and if it changes
    -- it should increase by one (most of the times).
    epochInc <- frequency [ (90, pure 0)
                          , (5, pure 1)
                          , (5, integral (constant 2 10) )]
    -- Get some random issuer from the delegates of the delegation map.
    vkI <- element $ Map.elems (st ^. delegState . delegationMap)
    let dsEnv
          = DSEnv
          { _dSEnvAllowedDelegators = undefined
          , _dSEnvEpoch = undefined
          , _dSEnvSlot = undefined
          , _dSEnvLiveness = st ^. pps . dLiveness }
    dCerts <- dcertsGen dsEnv
    let bh
          = BlockHeader
          { _prevHHash = st ^. lastHHash
          , _bSlot = undefined
          , _bIssuer = vkI
          , _bSig = Sig vkI (owner vkI)
          }
        bb
          = BlockBody
          { _bDCerts = dCerts }
    return $ Block bh bb
