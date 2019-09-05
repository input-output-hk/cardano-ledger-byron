{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Cardano.Chain.Update.Vote
  (
  -- * Vote
    Vote(..)

  -- * Vote Constructors
  , mkVote
  , mkVoteSafe

  -- * Vote Accessors
  , proposalId

  -- * Vote Binary Serialization
  , recoverSignedBytes

  -- * Vote Formatting
  , formatVoteShort
  , shortVoteF
  )
where

import Cardano.Prelude

import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, build, later)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , FromCBOR(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , encodeListLen
  , encodePreEncoded
  , enforceSize
  , serialize'
  )
import Cardano.Chain.Common (addressHash)
import Cardano.Chain.Update.Proposal (Proposal, UpId)
import Cardano.Crypto
  ( ProtocolMagicId
  , VerificationKey
  , SafeSigner
  , SigningKey
  , SignTag(SignUSVote)
  , Signature
  , safeSign
  , safeToVerification
  , shortHashF
  , sign
  , toVerification
  )


--------------------------------------------------------------------------------
-- Vote
--------------------------------------------------------------------------------

-- | Vote for update proposal
--
--   Invariant: The signature is valid.
data Vote = UnsafeVote
  { voterVK     :: !VerificationKey
  -- ^ Verification key casting the vote
  , aProposalId :: !(Annotated UpId ByteString)
  -- ^ Proposal to which this vote applies
  , signature   :: !(Signature (UpId, Bool))
  -- ^ Signature of (Update proposal, Approval/rejection bit)
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData


--------------------------------------------------------------------------------
-- Vote Constructors
--------------------------------------------------------------------------------

-- | A safe constructor for 'UnsafeVote'
mkVote
  :: ProtocolMagicId
  -> SigningKey
  -- ^ The voter
  -> UpId
  -- ^ Proposal which is voted for
  -> Bool
  -- ^ Approval/rejection bit
  -> Vote
mkVote pm sk upId decision = UnsafeVote
  (toVerification sk)
  (Annotated upId (serialize' upId))
  (sign pm SignUSVote sk (upId, decision))

-- | Same as 'mkVote', but uses 'SafeSigner'
mkVoteSafe
  :: ProtocolMagicId
  -> SafeSigner
  -- ^ The voter
  -> UpId
  -- ^ Proposal which is voted for
  -> Bool
  -- ^ Approval/rejection bit
  -> Vote
mkVoteSafe pm sk upId decision = UnsafeVote
  (safeToVerification sk)
  (Annotated upId (serialize' upId))
  (safeSign pm SignUSVote sk (upId, decision))


--------------------------------------------------------------------------------
-- Vote Accessors
--------------------------------------------------------------------------------

proposalId :: Vote -> UpId
proposalId = unAnnotated . aProposalId


--------------------------------------------------------------------------------
-- Vote Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Vote where
  toCBOR uv =
    encodeListLen 4
      <> toCBOR (voterVK uv)
      <> encodePreEncoded (annotation $ aProposalId uv)
      -- We encode @True@ here because we removed the decision bit. This is safe
      -- because we know that all @Vote@s on mainnet use this encoding and any
      -- changes to the encoding in our implementation will be picked up by
      -- golden tests.
      <> toCBOR True
      <> toCBOR (signature uv)

instance FromCBORAnnotated Vote where
  fromCBORAnnotated' =
    UnsafeVote <$ lift (enforceSize "Vote" 4)
      <*> lift fromCBOR
      <*> fromCBORAnnotated'
      -- Drop the decision bit that previously allowed negative voting
      <*  lift (fromCBOR @Bool)
      <*> lift fromCBOR

recoverSignedBytes :: Vote -> Annotated (UpId, Bool) ByteString
recoverSignedBytes v =
  let
    bytes = mconcat
      [ "\130"
      -- The byte above is part of the signed payload, but is not part of the
      -- transmitted payload
      , annotation $ aProposalId v
      , "\245"
      -- The byte above is the canonical encoding of @True@, which we hardcode,
      -- because we removed the possibility of negative voting
      ]
  in Annotated (proposalId v, True) bytes


--------------------------------------------------------------------------------
-- Vote Formatting
--------------------------------------------------------------------------------

instance B.Buildable Vote where
  build uv = bprint
    ( "Update Vote { voter: "
    . build
    . ", proposal id: "
    . build
    . " }"
    )
    (addressHash $ voterVK uv)
    (proposalId uv)

instance B.Buildable (Proposal, [Vote]) where
  build (up, votes) =
    bprint (build . " with votes: " . listJson) up (map formatVoteShort votes)

-- | Format 'Vote' compactly
formatVoteShort :: Vote -> Builder
formatVoteShort uv = bprint
  ("(" . shortHashF . " " . shortHashF . ")")
  (addressHash $ voterVK uv)
  (proposalId uv)

-- | Formatter for 'Vote' which displays it compactly
shortVoteF :: Format r (Vote -> r)
shortVoteF = later formatVoteShort
