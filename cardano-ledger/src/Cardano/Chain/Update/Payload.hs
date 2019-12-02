{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}

module Cardano.Chain.Update.Payload
  ( Payload(payloadProposal, payloadVotes, payloadSerialized)
  , pattern Payload
  )
where

import Cardano.Prelude
import qualified Data.ByteString.Lazy as BSL
import Formatting (bprint)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Decoded(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , encodeListLen
  , encodePreEncoded
  , enforceSize
  , serializeEncoding'
  , unwrapAnn
  , withAnnotation
  )
import Cardano.Chain.Update.Proposal
  ( Proposal
  , formatMaybeProposal
  )
import Cardano.Chain.Update.Vote
  ( Vote
  , formatVoteShort
  )


{-# COMPLETE Payload #-}
pattern Payload :: Maybe Proposal -> [Vote] -> Payload
pattern Payload{payloadProposal, payloadVotes} <-
  Payload' payloadProposal payloadVotes _
  where
    Payload pp pv =
      let bytes = serializeEncoding' $
            encodeListLen 2 <> toCBOR pp <> toCBOR pv
      in Payload' pp pv bytes

-- | Update System payload
data Payload = Payload'
  { payloadProposal'  :: !(Maybe Proposal)
  , payloadVotes'     :: ![Vote]
  , payloadSerialized :: ByteString
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance Decoded Payload where
  type BaseType Payload = Payload
  recoverBytes = payloadSerialized

instance B.Buildable Payload where
  build p
    | null (payloadVotes p)
    = formatMaybeProposal (payloadProposal p) <> ", no votes"
    | otherwise
    = formatMaybeProposal (payloadProposal p) <> bprint
      ("\n    votes: " . listJson)
      (map formatVoteShort (payloadVotes p))

instance ToCBOR Payload where
  toCBOR = encodePreEncoded . payloadSerialized

instance FromCBORAnnotated Payload where
  fromCBORAnnotated = withAnnotation $ do
    enforceSize "Update.Payload" 2
    pp <- unwrapAnn fromCBORAnnotated
    pv <- unwrapAnn fromCBORAnnotated
    return $ \bytes -> Payload' (pp bytes) (pv bytes) (BSL.toStrict bytes)
