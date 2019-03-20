{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump
  ()
where

import Cardano.Prelude hiding (State)

import Data.Maybe (fromJust)

import Cardano.Chain.Common.BlockCount (BlockCount)
import Cardano.Chain.Slotting (EpochIndex, FlatSlotId, twice)
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.Validation.Endorsement
  ( CandidateProtocolUpdate(CandidateProtocolUpdate)
  , cpvProtocolParameters
  , cpvProtocolVersion
  , cpvSlot
  )

data Environment = Environment
  { k           :: !BlockCount
  , currentSlot :: !FlatSlotId
  }

data State = State
  { currentEpoch              :: !EpochIndex
  , adoptedProtocolVersion    :: !ProtocolVersion
  , adoptedProtocolParameters :: !ProtocolParameters
  , candidateProtocolVersions :: ![CandidateProtocolUpdate]
  }

-- | Change the protocol version when an epoch change is detected, and there is
-- a candidate protocol update that was confirmed at least @2 * k@ slots ago,
-- where @k@ is the chain security parameter.
tryBumpVersion
  :: Environment
  -> State
  -> EpochIndex
  -> State
tryBumpVersion env st lastSeenEpoch =
  if currentEpoch < lastSeenEpoch && not (null stableCandidates)
  then
    let CandidateProtocolUpdate
          { cpvProtocolVersion
          , cpvProtocolParameters
          } = fromMaybe err $ head stableCandidates
        err = panic $  "The list of stable candidates shouldn't be empty,"
                    <> "since this was checked in this if branch"
    in
      st { currentEpoch = lastSeenEpoch
         , adoptedProtocolVersion = cpvProtocolVersion
         , adoptedProtocolParameters = cpvProtocolParameters
         , candidateProtocolVersions =
             filter ((currentSlot - twice k <) . cpvSlot) candidateProtocolVersions
         }
  else st

  where
    Environment { k, currentSlot} = env

    State
      { currentEpoch
      , adoptedProtocolVersion
      , adoptedProtocolParameters
      , candidateProtocolVersions } = st

    stableCandidates =
      filter ((<= currentSlot - twice k) . cpvSlot) candidateProtocolVersions
