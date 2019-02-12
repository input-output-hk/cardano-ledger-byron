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

import Cardano.Prelude hiding (to)

import Control.Lens ((^.), to, makeLenses, (.~))
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M

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
import qualified Cardano.Chain.Slotting as Slotting
import qualified Cardano.Chain.Txp as Txp
import qualified Cardano.Chain.Update as Update

import qualified Control.State.Transition as Transition
import Cardano.Spec.Chain.STS.Rule.Chain (CHAIN, dis, epoch, pps)
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import qualified Ledger.Core as Abstract
import Ledger.Delegation (DCert, mkDCert, delegationMap)
import Ledger.Update (bkSlotsPerEpoch, PParams)

import Test.Cardano.Chain.Interpreter
  ( interpretDCert
  , interpretKeyPair
  , vKeyPair
  )
 -- TODO: discuss with Ru whether the genesis hash can be in the
 -- 'ChainValidationState'.

-- | Elaboration environment. It contains the abstract environment of the chain
-- extension rules, plus additional information needed to elaborate an abstract
-- block into a concrete.
--
data Environment
  = Environment
  { _chainEnv :: !(Transition.Environment CHAIN)

  -- TODO: I'm not sure this is needed it: if you want to cache concrete keys,
  -- then this will probably become an environment.

  -- , _vKeys    :: !(Set Abstract.VKey)
  -- -- ^ All the keys defined in the environment. This will be probab
  -- , _avkToCvk :: !(Map Abstract.VKey (CC.XPub, CC.XPrv))
  -- -- ^ Abstract verifying key to concrete signing and verifying keys.
  }

makeLenses ''Environment

elaborate
  :: Genesis.GenesisHash -- TODO: Do we want this to coincide with the hash of
                         -- the abstract environment? (and in such case we
                         -- wouldn't need this parameter)
  -> Environment
  -> Transition.State CHAIN
  -> Concrete.ChainValidationState
  -> Abstract.Block
  -> Concrete.ABlock ()
elaborate genesisHash env ast st ab
  = Concrete.ABlock
  { Concrete.blockHeader = bh0
  , Concrete.blockBody = bb0
  , Concrete.aBlockExtraData = Binary.Annotated extraBodyData ()
  }
  where
    bh0
      = Concrete.mkHeaderExplicit
        Crypto.dummyProtocolMagicId
        prevHash
        0 -- Chain difficulty.
        -- TODO: we might want to generate the chain difficulty
        sid -- SlotId
        ssk -- SecretKey
        cDCert -- Maybe Delegation.Certificate
        bb0
        extraHeaderData

    emptyAttrs = Common.Attributes () (Common.UnparsedFields [])

    extraBodyData = Concrete.ExtraBodyData emptyAttrs

    -- Once the update mechanism is in place we might need to fill this in with
    -- the update data.
    extraHeaderData
      = Concrete.ExtraHeaderData
      { Concrete.ehdProtocolVersion = Update.ProtocolVersion 0 0 0
      , Concrete.ehdSoftwareVersion =
        Update.SoftwareVersion (Update.ApplicationName "baz") 0
      , Concrete.ehdAttributes = emptyAttrs
      , Concrete.ehdEBDataProof = H.hash extraBodyData
      }

    prevHash
      = fromMaybe (Genesis.getGenesisHash genesisHash) $ Concrete.cvsPreviousHash st

    sid = Slotting.unflattenSlotId
      (coerce (env ^. chainEnv . pps . bkSlotsPerEpoch))
      -- TODO: I don't like this inconsistency between qualifying and not, but
      -- some qualifying some identifiers can get too awkward. And using this
      -- with lenses gets even worse.
      (ab ^. Abstract.bHeader . Abstract.bSlot . to Abstract.getValue)

    issuer = ab ^. Abstract.bHeader . Abstract.bIssuer

    (_, ssk) = interpretKeyPair $ vKeyPair $ issuer

    -- See test/Test/Cardano/Chain/Interpreter.hs
    cDCert :: Maybe Delegation.Certificate
    cDCert = Just $ interpretDCert $ rcDCert issuer ast

    bb0 = undefined

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

-- TODO: We might want to use the concrete generators in the 'elaborate' function.
--

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

    emptyAttrs = Common.Attributes () (Common.UnparsedFields [])

    extraHeaderData
      = Concrete.ExtraHeaderData
      { Concrete.ehdProtocolVersion = Update.ProtocolVersion 0 0 0
      , Concrete.ehdSoftwareVersion =
        Update.SoftwareVersion (Update.ApplicationName "baz") 0
      , Concrete.ehdAttributes = emptyAttrs
      , Concrete.ehdEBDataProof = H.hash extraBodyData
      }

    extraBodyData = Concrete.ExtraBodyData emptyAttrs

    bb0
      = Concrete.ABody
      { Concrete.bodyTxPayload = Txp.ATxPayload []
      , Concrete.bodySscPayload = Ssc.SscPayload
      , Concrete.bodyDlgPayload = Delegation.UnsafeAPayload [] ()
      , Concrete.bodyUpdatePayload = Update.APayload Nothing [] ()
      }


-- | Re-construct an abstract delegation certificate from the abstract state.
rcDCert
  :: Abstract.VKey
  -- ^ Key for which the delegation certificate is being constructed.
  -> Transition.State CHAIN
  -> DCert
rcDCert vk ast
  = mkDCert vkg sigVkg vk (ast ^. epoch)
  where
    dm = ast ^. dis . delegationMap
    vkg = case M.keys $ M.filter (== vk) dm of
            [res] -> res
            _ -> panic $ "No delegator found for key " <> show vk

    vkp = vKeyPair $ coerce vkg

    sigVkg = Abstract.sign (Abstract.sKey vkp) vkg
