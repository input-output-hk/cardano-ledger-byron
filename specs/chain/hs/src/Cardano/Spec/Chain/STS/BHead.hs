{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Spec.Chain.STS.BHead where

import Control.Lens ((^.))
import Data.Map.Strict (Map)
import Data.Sequence (Seq, (|>))

import Control.State.Transition
import Ledger.Core
import Ledger.Update (PParams, maxHdrSz)
import Ledger.Signatures (Hash)

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Epoch
import Cardano.Spec.Chain.STS.SigCnt

data BHEAD

instance STS BHEAD where
  type Environment BHEAD
    = ( Slot
      , Map VKeyGenesis VKey
      )
  type State BHEAD
    = ( Epoch
      , Slot
      , Hash
      , PParams
      , Seq VKeyGenesis
      )

  type Signal BHEAD = BlockHeader

  data PredicateFailure BHEAD
    = HashesDontMatch -- TODO: Add fields so that users know the two hashes that don't match
    | HeaderSizeTooBig -- TODO: Add more information here as well.
    | SlotDidNotIncrease
    -- ^ The block header slot number did not increase w.r.t the last seen slot
    | SlotInTheFuture
    -- ^ The block header slot number is greater than the current slot (As
    -- specified in the environment).
    | EpochFailure (PredicateFailure EPOCH)
    | SigCntFailure (PredicateFailure SIGCNT)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ( (sNow, dms)
            , (eLast, sLast, hp, us, sgs)
            , bh ) <- judgmentContext
        -- Check header size
        let sMax = us ^. maxHdrSz
        bHeaderSize bh <= sMax ?! HeaderSizeTooBig
        -- Check that the previous hash matches
        bh ^. prevHHash == hp ?! HashesDontMatch
        -- Check sanity of current slot
        let sNext = bh ^. bSlot
        sLast < sNext ?! SlotDidNotIncrease
        sNext <= sNow ?! SlotInTheFuture
        -- Perform an epoch transition
        eNext <-  trans @EPOCH $ TRC (dms, eLast, sNext)
        -- Perform a signature count transition
        sgs' <- trans @SIGCNT $ TRC ((us, dms), sgs, bh ^. bIssuer)
        return $! ( eNext
                  , sNext
                  , hashHeader bh
                  , us
                  , sgs'
                  )
    ]

instance Embed EPOCH BHEAD where
  wrapFailed = EpochFailure

instance Embed SIGCNT BHEAD where
  wrapFailed = SigCntFailure
