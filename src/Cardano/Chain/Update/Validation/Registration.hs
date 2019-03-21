{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Validation rules for registering updates
--
--   This is an implementation of the rules defined in the Byron ledger
--   specification
module Cardano.Chain.Update.Validation.Registration
  ( Error
  , Environment (..)
  , State (..)
  , ProtocolUpdateProposals
  , registerProposal
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M

import Cardano.Chain.Common (StakeholderId, mkStakeholderId)
import Cardano.Chain.Slotting (FlatSlotId)
import Cardano.Chain.Update.ApplicationName (ApplicationName)
import Cardano.Chain.Update.ProtocolParameters
  ( ProtocolParameters
  , ppMaxBlockSize
  , ppMaxProposalSize
  , ppScriptVersion
  )
import Cardano.Chain.Update.ProtocolParameterUpdate
  ( ProtocolParameterUpdate
  , ppuMaxBlockSize
  , ppuMaxTxSize
  , ppuScriptVersion
  )
import qualified Cardano.Chain.Update.ProtocolParameterUpdate as PPU
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion(ProtocolVersion))
import Cardano.Chain.Update.SoftwareVersion
  ( NumSoftwareVersion
  , SoftwareVersion(SoftwareVersion)
  )
import Cardano.Chain.Update.Vote
  ( AProposal(AProposal)
  , ProposalBody(ProposalBody)
  , UpId
  , pbProtocolParameterUpdate
  , pbSoftwareVersion
  , proposalAnnotation
  , proposalAnnotation
  , proposalBody
  , recoverUpId
  , pbProtocolVersion
  )
import Cardano.Crypto
  ( ProtocolMagicId
  , SignTag(SignUSProposal)
  , verifySignatureDecoded
  )


data Environment = Environment
  { protocolMagic             :: ProtocolMagicId
  , adoptedProtocolVersion    :: !ProtocolVersion
  , adoptedProtocolParameters :: !ProtocolParameters
  , appVersions               :: !(Map ApplicationName (NumSoftwareVersion, FlatSlotId))
  , delegationMap             :: !(Map StakeholderId StakeholderId)
  }

-- | State keeps track of registered protocol and software update
--   proposals
data State = State
  { rsProtocolUpdateProposals :: !ProtocolUpdateProposals
  , rsSoftwareUpdateProposals :: !SoftwareUpdateProposals
  }

type ProtocolUpdateProposals = Map UpId (ProtocolVersion, ProtocolParameterUpdate)
type SoftwareUpdateProposals = Map UpId SoftwareVersion

type ApplicationVersions = Map ApplicationName (NumSoftwareVersion, FlatSlotId)

-- | Error captures the ways in which registration could fail
data Error
  = RegistrationDuplicateProtocolVersion ProtocolVersion
  | RegistrationDuplicateSoftwareVersion SoftwareVersion
  | RegistrationInvalidProposer StakeholderId
  | RegistrationInvalidProtocolVersion ProtocolVersion
  | RegistrationInvalidScriptVersion Word16 Word16
  | RegistrationInvalidSignature
  | RegistrationInvalidSoftwareVersion SoftwareVersion
  | RegistrationMaxBlockSizeTooLarge (TooLarge Natural)
  | RegistrationMaxTxSizeTooLarge (TooLarge Natural)
  | RegistrationProposalEmpty
  | RegistrationProposalTooLarge (TooLarge Natural)
  deriving (Eq, Show)

data TooLarge n = TooLarge
  { tlExpected :: n
  , tlActual   :: n
  } deriving (Eq, Show)


-- | Register an update proposal after verifying its signature and validating
--   its contents. This corresponds to the @UPREG@ rules in the spec.
registerProposal
  :: MonadError Error m
  => Environment
  -> State
  -> AProposal ByteString
  -> m State
registerProposal env rs proposal = do

  -- Check that the proposer is delegated to by a genesis key
  not (null $ M.filter (== proposer) delegationMap)
    `orThrowError` RegistrationInvalidProposer proposer

  -- Verify the proposal signature
  verifySignatureDecoded protocolMagic SignUSProposal proposerPK body sig
    `orThrowError` RegistrationInvalidSignature

  -- Check that the proposal is valid
  registerProposalComponents
    adoptedProtocolVersion adoptedProtocolParameters appVersions rs proposal

 where
  AProposal body proposerPK sig _ = proposal

  proposer = mkStakeholderId proposerPK

  Environment
    { protocolMagic
    , adoptedProtocolVersion
    , adoptedProtocolParameters
    , appVersions
    , delegationMap
    } = env


-- | Register the individual components of an update proposal
--
--   The proposal may contain a protocol update, a software update, or both.
--   This corresponds to the `UPV` rules in the spec.
registerProposalComponents
  :: MonadError Error m
  => ProtocolVersion
  -> ProtocolParameters
  -> ApplicationVersions
  -> State
  -> AProposal ByteString
  -> m State
registerProposalComponents adoptedPV adoptedPP appVersions rs proposal = do

  (protocolVersionChanged || softwareVersionChanged)
    `orThrowError` RegistrationProposalEmpty

  -- Register protocol update if we have one
  registeredPUPs' <- if protocolVersionChanged
    then registerProtocolUpdate adoptedPV adoptedPP registeredPUPs proposal
    else pure registeredPUPs

  -- Register software update if we have one
  registeredSUPs' <- if softwareVersionChanged
    then registerSoftwareUpdate appVersions registeredSUPs proposal
    else pure registeredSUPs

  pure $ State registeredPUPs' registeredSUPs'
 where
  ProposalBody protocolVersion ppu softwareVersion _ _ =
    proposalBody proposal

  SoftwareVersion appName appVersion = softwareVersion

  softwareVersionChanged =
    maybe True ((/= appVersion) . fst) $ M.lookup appName appVersions

  protocolVersionChanged =
    not $ protocolVersion == adoptedPV && PPU.isEmpty ppu

  State registeredPUPs registeredSUPs = rs


-- | Validate a protocol update
--
--   We check that:
--
--   1) The protocol update hasn't already been registered
--   2) The protocol version is a valid next version
--   3) The new 'ProtocolParameters' represent a valid update
--
--   This corresponds to the `UPPVV` rule in the spec.
registerProtocolUpdate
  :: MonadError Error m
  => ProtocolVersion
  -> ProtocolParameters
  -> ProtocolUpdateProposals
  -> AProposal ByteString
  -> m ProtocolUpdateProposals
registerProtocolUpdate adoptedPV adoptedPP registeredPUPs proposal = do

  -- Check that this protocol version isn't already registered
  null (M.filter ((== newPV) . fst) registeredPUPs)
    `orThrowError` RegistrationDuplicateProtocolVersion newPV

  -- Check that this protocol version is a valid next version
  pvCanFollow newPV adoptedPV
    `orThrowError` RegistrationInvalidProtocolVersion newPV

  canUpdate adoptedPP ppu proposal

  pure $ M.insert (recoverUpId proposal) (newPV, ppu) registeredPUPs
 where
  body  = proposalBody proposal
  newPV = pbProtocolVersion body
  ppu   = pbProtocolParameterUpdate body

-- | Check that the new 'ProtocolVersion' is a valid next version
--
-- FIXME: this predicate should be checked at the moment of adoption, since the
-- current parameters might have changed.
pvCanFollow :: ProtocolVersion -> ProtocolVersion -> Bool
pvCanFollow newPV adoptedPV = adoptedPV < newPV && isNextVersion
 where
  ProtocolVersion newMajor     newMinor     _ = newPV
  ProtocolVersion adoptedMajor adoptedMinor _ = adoptedPV
  isNextVersion = case newMajor - adoptedMajor of
    0 -> newMinor == adoptedMinor + 1
    1 -> newMinor == 0
    _ -> False


-- | Check that the new 'ProtocolParameters' represent a valid update
--
--   This is where we enforce constraints on how the 'ProtocolParameters' change
--
-- FIXME: this predicate should be checked at the moment of adoption, since the
-- current parameters might have changed.
canUpdate
  :: MonadError Error m
  => ProtocolParameters
  -> ProtocolParameterUpdate
  -> AProposal ByteString
  -> m ()
canUpdate adoptedPP upp proposal = do

  -- Check that the proposal size is less than the maximum
  (proposalSize <= maxProposalSize) `orThrowError` RegistrationProposalTooLarge
    (TooLarge maxProposalSize proposalSize)

  -- Check that the new maximum block size is no more than twice the current one
  (newMaxBlockSize ?<= 2 * adoptedMaxBlockSize)
    `orThrowError` RegistrationMaxBlockSizeTooLarge
                    (TooLarge adoptedMaxBlockSize (value newMaxBlockSize))

  -- Check that the new max transaction size is less than the new max block size
  (newMaxTxSize ?<=?  newMaxBlockSize) `orThrowError` RegistrationMaxTxSizeTooLarge
    (TooLarge (value newMaxBlockSize) (value newMaxTxSize))

  -- Check that the new script version is either the same or incremented
  (0 <=? scriptVersionDiff && scriptVersionDiff ?<= 1)
    `orThrowError` RegistrationInvalidScriptVersion
                    adoptedScriptVersion
                    (value newScriptVersion)
 where
  proposalSize :: Natural
  proposalSize         = fromIntegral . BS.length $ proposalAnnotation proposal
  maxProposalSize      = ppMaxProposalSize adoptedPP

  adoptedMaxBlockSize  = ppMaxBlockSize adoptedPP
  newMaxBlockSize      = ppuMaxBlockSize upp

  newMaxTxSize         = ppuMaxTxSize upp

  adoptedScriptVersion = ppScriptVersion adoptedPP
  newScriptVersion     = ppuScriptVersion upp
  scriptVersionDiff    = (\x -> x - adoptedScriptVersion) <$> newScriptVersion

  value :: Maybe a -> a
  value (Just x) = x
  value Nothing =
    panic $  "Unexpected 'Nothing' value."
          <> " If a condition '?<=' or '<=?' failed it means "
          <> "there was a value which made the comparison evaluate to 'False'."

(?<=) :: Ord a => Maybe a -> a -> Bool
x ?<= y = maybe True (<= y) x

infixl 4 ?<=

(<=?) :: Ord a => a -> Maybe a -> Bool
x <=? y = maybe True (x <=) y

infixl 4 <=?

(?<=?) :: Ord a => Maybe a -> Maybe a -> Bool
x ?<=? y = fromMaybe True $ (<=) <$> x <*> y

infixl 4 ?<=?

-- | Check that a new 'SoftwareVersion' is valid
--
--   We check that:
--
--   1) The 'SoftwareVersion' hasn't already been registered
--   2) The new 'SoftwareVersion' is a valid next version
--
--   This corresponds to the `UPSVV` rule in the spec.
registerSoftwareUpdate
  :: MonadError Error m
  => ApplicationVersions
  -> SoftwareUpdateProposals
  -> AProposal ByteString
  -> m SoftwareUpdateProposals
registerSoftwareUpdate appVersions registeredSUPs proposal = do

  -- Check that this software version isn't already registered
  null (M.filter (== softwareVersion) registeredSUPs)
    `orThrowError` RegistrationDuplicateSoftwareVersion softwareVersion

  -- Check that this software version is a valid next version
  svCanFollow appVersions softwareVersion
    `orThrowError` RegistrationInvalidSoftwareVersion softwareVersion

  -- Add to the list of registered software update proposals
  pure $ M.insert (recoverUpId proposal) softwareVersion registeredSUPs
  where softwareVersion = pbSoftwareVersion $ proposalBody proposal


-- | Check that a new 'SoftwareVersion' is a valid next version
--
--   The new version is valid for a given application if it is the same or one
--   more than the current version
svCanFollow :: ApplicationVersions -> SoftwareVersion -> Bool
svCanFollow avs softwareVersion = case M.lookup appName avs of
  Nothing -> appVersion == 0
  Just (currentAppVersion, _) -> appVersion == currentAppVersion + 1
  where SoftwareVersion appName appVersion = softwareVersion
