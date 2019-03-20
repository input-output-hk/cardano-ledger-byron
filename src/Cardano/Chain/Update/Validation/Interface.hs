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
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)
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
import qualified Cardano.Chain.Update.Validation.Registration as Registration
import qualified Cardano.Chain.Update.Validation.Voting as Voting
import Cardano.Chain.Update.Vote (UpId, AProposal, recoverUpId, AVote)
import Cardano.Crypto (ProtocolMagicId)


data Environment = Environment
  { protocolMagic :: ProtocolMagicId
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
  , registeredProtocolUpdateProposals :: !(Map UpId (ProtocolVersion, ProtocolParameters))
    -- ^ Registered protocol update proposals
  , registeredSoftwareUpdateProposals :: !(Map UpId SoftwareVersion)
    -- ^ Registered software update proposals
  , confirmedProposals                :: !(Map UpId FlatSlotId)
    -- ^ Confirmed update proposals
  , proposalVotes                     :: !(Map UpId (Set StakeholderId))
    -- ^ Update proposals votes
  , registeredEndorsements            :: !(Set Endorsement)
    -- ^ Update proposals endorsements
  , proposalRegistrationSlot          :: Map UpId FlatSlotId
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
  Registration.State rpus' raus' <-
    Registration.registerProposal pm pv pps avs dms regSubSt proposal
      `wrapError` Registration
  pure $!
    st { registeredProtocolUpdateProposals = rpus'
       , registeredSoftwareUpdateProposals = raus'
       , proposalRegistrationSlot = M.insert (recoverUpId proposal) currentSlot pws
       }

  where
    Environment
      { protocolMagic = pm
      , currentSlot
      , delegationMap = dms
      } = env

    State
      { adoptedProtocolVersion = pv
      , adoptedProtocolParameters = pps
      , appVersions = avs
      , registeredProtocolUpdateProposals = rpus
      , registeredSoftwareUpdateProposals = raus
      , proposalRegistrationSlot = pws
      } = st

    regSubSt = Registration.State rpus raus

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
  Voting.State vts' cps' <-
    Voting.registerVoteWithConfirmation pm subEnv subSt vote
      `wrapError` Voting
  let
    avsNew =
      M.fromList $! [ (svAppName sv, (svNumber sv, sn))
                    | (pid, sv) <- M.toList raus
                    , pid `elem` M.keys cps'
                    ]
  pure $!
    st { confirmedProposals = cps'
       , proposalVotes = vts'
       , appVersions = M.union avsNew avs
       , registeredSoftwareUpdateProposals = M.withoutKeys raus (M.keysSet cps)
       }
       -- TODO: consider using the `Relation` instances from `fm-ledger-rules` (see `Ledger.Core`)

  where
    Environment
      { protocolMagic = pm
      , currentSlot = sn
      , delegationMap = dms
      } = env

    State
      { adoptedProtocolParameters = pps
      , proposalRegistrationSlot
      , proposalVotes = vts
      , confirmedProposals = cps
      , appVersions =  avs
      , registeredSoftwareUpdateProposals = raus
      } = st

    rups = M.keysSet proposalRegistrationSlot

    subEnv = Voting.Environment sn pps (Voting.RegistrationEnvironment rups dms)

    subSt = Voting.State vts cps

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
  Endorsement.State fads' bvs' <-
    Endorsement.register (subEnv ngk) subSt endorsement
      `wrapError` Endorsement
  let
    pidsKeep = nonExpiredPids `union` confirmedPids

    sn = currentSlot

    nonExpiredPids =
      M.keysSet $ M.filter (currentSlot - u <=) proposalRegistrationSlot

    confirmedPids = M.keysSet confirmedProposals

    registeredProtocolUpdateProposals' =
      M.restrictKeys registeredUpdateProposals pidsKeep

    vsKeep = S.fromList $ fst <$> M.elems registeredProtocolUpdateProposals'

  pure $!
    st { candidateProtocolVersions = fads'
       , registeredProtocolUpdateProposals = registeredProtocolUpdateProposals'
       , registeredSoftwareUpdateProposals =
           M.restrictKeys registeredSoftwareUpdateProposals pidsKeep
       , proposalVotes =
           M.restrictKeys proposalVotes pidsKeep
       , registeredEndorsements =
           S.filter ((`S.member` vsKeep) . endorsementProtocolVersion) registeredEndorsements
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
        registeredUpdateProposals
        n -- Number of genesis keys

    Environment
      { k
      , currentSlot
      , delegationMap
      } = env

    State
      { adoptedProtocolParameters
      , confirmedProposals
      , registeredProtocolUpdateProposals = registeredUpdateProposals
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

    u = undefined
    -- TODO: what should our proposal time-to-live be?
    --
    -- We only have this, but it concerns the threshold for implicit adoption.
    -- https://github.com/input-output-hk/cardano-sl/search?q=updateImplicit&unscoped_q=updateImplicit
    --
    -- For a proposal time-to-live we might want a substantially lower value.

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
registerEpoch = undefined
