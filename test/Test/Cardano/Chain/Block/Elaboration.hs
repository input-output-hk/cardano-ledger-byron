{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}

-- | This module provides functionality for translating abstract blocks into
-- concrete blocks. The abstract blocks are generated according the small-step
-- rules for the block chain (also called the blockchain specification).
module Test.Cardano.Chain.Block.Elaboration
  ( elaborate
  )
where

import Cardano.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M

import qualified Cardano.Binary.Class as Binary
import qualified Test.Cardano.Crypto.Dummy as Crypto
import qualified Cardano.Crypto.Hashing as H
import qualified Cardano.Crypto.Signing as Signing
import qualified Cardano.Crypto.Wallet as CC

import qualified Cardano.Chain.Block as Concrete
import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Ssc as Ssc
import Cardano.Chain.Slotting (SlotId (SlotId))
import qualified Cardano.Chain.Txp as Txp
import qualified Cardano.Chain.Update as Update

import qualified Cardano.Spec.Chain.STS.Block as Abstract
elaborate
  :: Concrete.ChainValidationState
  -> Abstract.Block
  -> Concrete.ABlock ByteString
elaborate = undefined

annotateBlock
  :: Concrete.Block
  -> Concrete.ABlock ByteString
annotateBlock block =
  case Binary.decodeFullDecoder "Block" Concrete.decodeABlock bytes of
    Left _ -> panic "This function should be able to decode the block it encoded."
    Right res ->
      map (LBS.toStrict . Binary.slice bytes) res
  where
    bytes = Binary.serializeEncoding (Concrete.encodeBlock block)

-- TODO: Make a block that will be accepted by 'updateChain'

-- | Some random block that could be the first block in the chain.
block0
  :: Genesis.GenesisHash
  -> Concrete.Block
-- TODO: the initial state of the concrete validator will have 'Nothing' as
-- previous hash, and in this case, it will look the for the genesisHash in the
-- config. So we need to make sure that this genesis hash is the same as the
-- config.
block0 genesisHash
  = Concrete.ABlock
  { Concrete.blockHeader = bh0
  , Concrete.blockBody = bb0
  , Concrete.aBlockExtraData = Binary.Annotated extraBodyData ()
  }
  where
    bh0
      = Concrete.mkHeader
        Crypto.dummyProtocolMagicId
        (Left genesisHash) -- Either GenesisHash Header
        (SlotId 0 0) -- SlotId
        (Signing.SecretKey ssk) -- SecretKey
        Nothing   -- Maybe Delegation.Certificate
        bb0
        extraHeaderData -- TODO: ExtraHeaderData  This is IMPORTANT!

    -- Signer secret key
    -- TODO: it seems we will have to have access to the secret keys of all the
    -- keys in the abstract environment.
    ssk = CC.generate ("foo" :: ByteString) ("bar" :: ByteString)

    extraHeaderData
      = Concrete.ExtraHeaderData
      { Concrete.ehdProtocolVersion = Update.ProtocolVersion 0 0 0
      , Concrete.ehdSoftwareVersion =
          Update.SoftwareVersion (Update.ApplicationName "baz") 0
      , Concrete.ehdAttributes = emptyAttrs
      , Concrete.ehdEBDataProof = H.hash extraBodyData
      }

    emptyAttrs = Common.Attributes () (Common.UnparsedFields [])

    extraBodyData = Concrete.ExtraBodyData emptyAttrs

    bb0
      = Concrete.ABody
      { Concrete.bodyTxPayload = Txp.ATxPayload []
      , Concrete.bodySscPayload = Ssc.SscPayload
      , Concrete.bodyDlgPayload = Delegation.UnsafeAPayload [] ()
      , Concrete.bodyUpdatePayload = Update.APayload Nothing [] ()
      }
