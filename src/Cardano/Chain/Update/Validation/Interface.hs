{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Blockchain interface validation rules.
--
module Cardano.Chain.Update.Validation.Interface
  ( -- * Environment
    Environment (..)
    -- * State
  , State (..)
    -- *Error
  , Error (..)
    -- * Interface functions
  , registerProposal
  , registerVote
  , registerEndorsement
  , registerEpoch
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M
import Data.Set (union)
import qualified Data.Set as S

import Cardano.Chain.Slotting (EpochIndex, FlatSlotId)
import Cardano.Chain.Common.StakeholderId (StakeholderId)
import Cardano.Chain.Common.BlockCount (BlockCount)

import Cardano.Chain.Update.ApplicationName (ApplicationName)
import Cardano.Chain.Update.ProtocolParameters
  ( ProtocolParameters
  , ppUpdateImplicit
  )
import Cardano.Chain.Update.ProtocolParameterUpdate (ProtocolParameterUpdate)
import qualified Cardano.Chain.Update.ProtocolParameterUpdate as PPU
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion
  ( NumSoftwareVersion
  , SoftwareVersion
  , svAppName
  , svNumber
  )
import Cardano.Chain.Update.Validation.Endorsement
  ( CandidateProtocolUpdate
  , Endorsement
  , endorsementProtocolVersion
  )
import qualified Cardano.Chain.Update.Validation.Endorsement as Endorsement
import Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump
  ( tryBumpVersion
  )
import qualified Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump as PVBump
import qualified Cardano.Chain.Update.Validation.Registration as Registration
import qualified Cardano.Chain.Update.Validation.Voting as Voting
import Cardano.Chain.Update.Vote (AProposal, AVote, UpId, recoverUpId)
import Cardano.Crypto (ProtocolMagicId)


data Environment = Environment
  { protocolMagic :: !ProtocolMagicId
  , currentSlot   :: !FlatSlotId
  , delegationMap :: !(Map StakeholderId StakeholderId)
  , k             :: !BlockCount
  -- ^ TODO: this is the chain security parameter, a.k.a. @stableAfter@, it is not part
  -- of our protocol parameters, so it seems that we need to pass it in the
  -- environment. However we need to double-check this with others.
  }

-- | Update interface state.
data State = State
  { currentEpoch                      :: !EpochIndex
    -- ^ Current epoch
  , adoptedProtocolVersion            :: !ProtocolVersion
  , adoptedProtocolParameters         :: !ProtocolParameters
    -- ^ Adopted protocol parameters
  , candidateProtocolVersions         :: ![CandidateProtocolUpdate]
    -- ^ Candidate protocol versions
  , appVersions                       :: !(Map ApplicationName (NumSoftwareVersion, FlatSlotId))
    -- ^ Current application versions (by application name)
  , registeredProtocolUpdateProposals :: !(Map UpId (ProtocolVersion, ProtocolParameterUpdate))
    -- ^ Registered protocol update proposals
  , registeredSoftwareUpdateProposals :: !(Map UpId SoftwareVersion)
    -- ^ Registered software update proposals
  , confirmedProposals                :: !(Map UpId FlatSlotId)
    -- ^ Confirmed update proposals
  , proposalVotes                     :: !(Map UpId (Set StakeholderId))
    -- ^ Update proposals votes
  , registeredEndorsements            :: !(Set Endorsement)
    -- ^ Update proposals endorsements
  , proposalRegistrationSlot          :: !(Map UpId FlatSlotId)
    -- ^ Slot at which an update proposal was registered
  }

data Error
  = Registration Registration.Error
  | Voting Voting.Error
  | Endorsement Endorsement.Error
  | NumberOfGenesisKeysTooLarge Int

-- | Register an update proposal.
--
-- This corresponds to the @UPIREG@ rule in the Byron ledger specification.
registerProposal
  :: MonadError Error m
  => Environment
  -> State
  -> AProposal ByteString
  -> m State
registerProposal env st proposal = do
  Registration.State registeredProtocolUpdateProposals' registeredSoftwareUpdateProposals'
    <- Registration.registerProposal subEnv subSt proposal
       `wrapError` Registration
  pure $!
    st { registeredProtocolUpdateProposals = registeredProtocolUpdateProposals'
       , registeredSoftwareUpdateProposals = registeredSoftwareUpdateProposals'
       , proposalRegistrationSlot =
           M.insert (recoverUpId proposal) currentSlot proposalRegistrationSlot
       }

  where
    Environment
      { protocolMagic
      , currentSlot
      , delegationMap
      } = env

    State
      { adoptedProtocolVersion
      , adoptedProtocolParameters
      , appVersions
      , registeredProtocolUpdateProposals
      , registeredSoftwareUpdateProposals
      , proposalRegistrationSlot
      } = st

    subEnv =
      Registration.Environment
        protocolMagic
        adoptedProtocolVersion
        adoptedProtocolParameters
        appVersions
        delegationMap

    subSt =
      Registration.State
        registeredProtocolUpdateProposals
        registeredSoftwareUpdateProposals

-- | Register a vote for the given proposal.
--
-- If the proposal gets enough confirmations after adding the given vote, then
-- it will get added to the set of confirmed proposals.
--
-- This corresponds to the @UPIVOTE@ rule in the Byron ledger
-- specification.
--
registerVote
  :: MonadError Error m
  => Environment
  -> State
  -> AVote ByteString
  -> m State
registerVote env st vote = do
  Voting.State proposalVotes' confirmedProposals'
    <- Voting.registerVoteWithConfirmation protocolMagic subEnv subSt vote
      `wrapError` Voting
  let
    appVersions' =
      M.fromList $! [ (svAppName sv, (svNumber sv, currentSlot))
                    | (pid, sv) <- M.toList registeredSoftwareUpdateProposals
                    , pid `elem` M.keys confirmedProposals'
                    ]
  pure $!
    st { confirmedProposals = confirmedProposals'
       , proposalVotes = proposalVotes'
       -- Note that is important that the new application versions are passed
       -- as the first argument of @M.union@, since the values in this first
       -- argument overwrite the values in the second.
       , appVersions = M.union appVersions' appVersions
       , registeredSoftwareUpdateProposals =
           M.withoutKeys
             registeredSoftwareUpdateProposals
             (M.keysSet confirmedProposals)
       }
       -- TODO: consider using the `Relation` instances from `fm-ledger-rules` (see `Ledger.Core`)

  where
    Environment
      { protocolMagic
      , currentSlot
      , delegationMap
      } = env

    State
      { adoptedProtocolParameters
      , proposalRegistrationSlot
      , proposalVotes
      , confirmedProposals
      , appVersions
      , registeredSoftwareUpdateProposals
      } = st

    rups = M.keysSet proposalRegistrationSlot

    subEnv =
      Voting.Environment
        currentSlot
        adoptedProtocolParameters
        (Voting.RegistrationEnvironment rups delegationMap)

    subSt = Voting.State proposalVotes confirmedProposals

-- | Register an endorsement.
--
-- An endorsement represents the fact that a genesis stakeholder is ready to
-- start using the protocol version being endorsed. In the decentralized era
-- only genesis key holders can endorse protocol versions.
--
-- This corresponds to the @UPIEND@ rule in the Byron ledger
-- specification.
registerEndorsement
  :: MonadError Error m
  => Environment
  -> State
  -> Endorsement
  -> m State
registerEndorsement env st endorsement = do
  -- TODO: The domain of the delegation map should contain all the genesis keys.
  -- However I do not know whether we want to determine the number of genesis
  -- keys like this.
  ngk <- if M.size delegationMap <= fromIntegral (maxBound :: Word8)
         then pure $! fromIntegral (M.size delegationMap)
         else throwError $ NumberOfGenesisKeysTooLarge (M.size delegationMap)
  Endorsement.State candidateProtocolVersions' registeredEndorsements'
    <- Endorsement.register (subEnv ngk) subSt endorsement
       `wrapError` Endorsement
  let
    pidsKeep = nonExpiredPids `union` confirmedPids

    nonExpiredPids =
      M.keysSet $ M.filter (currentSlot - u <=) proposalRegistrationSlot

    confirmedPids = M.keysSet confirmedProposals

    registeredProtocolUpdateProposals' =
      M.restrictKeys registeredProtocolUpdateProposals pidsKeep

    vsKeep = S.fromList $ fst <$> M.elems registeredProtocolUpdateProposals'

  pure $!
    st { candidateProtocolVersions = candidateProtocolVersions'
       , registeredProtocolUpdateProposals = registeredProtocolUpdateProposals'
       , registeredSoftwareUpdateProposals =
           M.restrictKeys registeredSoftwareUpdateProposals pidsKeep
       , proposalVotes =
           M.restrictKeys proposalVotes pidsKeep
       , registeredEndorsements =
           S.filter ((`S.member` vsKeep) . endorsementProtocolVersion) registeredEndorsements'
       , proposalRegistrationSlot =
           M.restrictKeys proposalRegistrationSlot pidsKeep
       }

  where
    subEnv n =
      Endorsement.Environment
        k
        currentSlot
        delegationMap
        adoptedProtocolParameters
        confirmedProposals
        registeredProtocolUpdateProposals
        n -- Number of genesis keys

    Environment
      { k
      , currentSlot
      , delegationMap
      } = env

    State
      { adoptedProtocolParameters
      , confirmedProposals
      , registeredProtocolUpdateProposals
      , registeredSoftwareUpdateProposals
      , candidateProtocolVersions
      , proposalVotes
      , registeredEndorsements
      , proposalRegistrationSlot
      } = st

    subSt =
      Endorsement.State
        candidateProtocolVersions
        registeredEndorsements

    u = ppUpdateImplicit adoptedProtocolParameters

-- | Register an epoch. Whenever an epoch number is seen on a block this epoch
-- number should be passed to this function so that on epoch change the
-- protocol parameters can be updated, provided that there is an update
-- candidate that was accepted and endorsed by a majority of the genesis keys.
--
-- This corresponds to the @UPIEC@ rule in the Byron ledger specification.
registerEpoch
  :: MonadError Error m
  => Environment
  -> State
  -> EpochIndex
  -- ^ Epoch seen on the block.
  -> m State
registerEpoch env st lastSeenEpoch = do
  let PVBump.State
        currentEpoch'
        adoptedProtocolVersion'
        nextProtocolParameters'
        candidateProtocolVersions'
        = tryBumpVersion subEnv subSt lastSeenEpoch
      -- Keep only those update proposals that have a version lower than the
      -- candidate to be adopted.
      --
      -- NOTE: this could be optimized for the case in which there is no change
      -- in protocol version
      pidsKeep =
        S.fromList [ pid
                   | (pid, (pvi, _)) <- M.toList registeredProtocolUpdateProposals
                   , adoptedProtocolVersion' < pvi
                   ]
  pure $!
    st { currentEpoch = currentEpoch'
       , adoptedProtocolVersion = adoptedProtocolVersion'
       , adoptedProtocolParameters =
           PPU.apply nextProtocolParameters' adoptedProtocolParameters
       , candidateProtocolVersions = candidateProtocolVersions'
       , registeredProtocolUpdateProposals =
           M.restrictKeys registeredProtocolUpdateProposals pidsKeep
       , confirmedProposals =
           M.restrictKeys confirmedProposals pidsKeep
       , proposalVotes =
           M.restrictKeys proposalVotes pidsKeep
       , registeredEndorsements =
           S.filter ((adoptedProtocolVersion' <) . endorsementProtocolVersion)
                    registeredEndorsements
       , proposalRegistrationSlot =
           M.restrictKeys proposalRegistrationSlot pidsKeep
       }

  where
    subEnv = PVBump.Environment k currentSlot

    subSt =
      PVBump.State
        currentEpoch
        adoptedProtocolVersion
        PPU.empty -- FIXME: the PVBUMP rule doesn't use this parameter, so we should update the spec accordingly.
        candidateProtocolVersions

    Environment
      { k
      , currentSlot
      } = env

    State
      { currentEpoch
      , adoptedProtocolVersion
      , adoptedProtocolParameters
      , candidateProtocolVersions
      , registeredProtocolUpdateProposals
      , confirmedProposals
      , proposalVotes
      , registeredEndorsements
      , proposalRegistrationSlot
      } = st
