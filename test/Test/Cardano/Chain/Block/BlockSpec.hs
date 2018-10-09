{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

-- | Specification of Cardano.Chain.Block and Cardano.Chain.Block.Pure.

module Test.Cardano.Chain.Block.BlockSpec
       ( spec
       ) where

import           Cardano.Prelude

import           Serokell.Util (isVerSuccess)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, (===), (==>))

import           Cardano.Chain.Block (BlockHeader (..), BlockSignature (..),
                     GenesisBody (..), GenesisConsensusData (..),
                     GenesisExtraHeaderData (..), MainBody (..),
                     MainConsensusData (..), MainExtraHeaderData (..),
                     MainToSign (..), gbhProtocolMagic, headerHash,
                     mkGenericBlockHeaderUnsafe, mkGenesisHeader,
                     mkGenesisProof, mkMainHeader, mkMainProof)
import qualified Cardano.Chain.Block as Block
import           Cardano.Chain.Delegation (HeavyDlgIndex (..))
import           Cardano.Chain.Genesis (GenesisHash (..))
import           Cardano.Core (EpochIndex (..), SlotId (..), difficultyL)
import           Cardano.Core.Attributes (mkAttributes)
import           Cardano.Core.Chrono (NewestFirst (..))
import           Cardano.Crypto (ProtocolMagic (..), ProxySecretKey (pskIssuerPk),
                     SecretKey, SignTag (..), createPsk, proxySign, sign,
                     toPublic)

import           Test.Cardano.Chain.Block.Arbitrary as BT
import           Test.Cardano.Chain.Genesis.Dummy (dummyGenesisHash)
import           Test.Cardano.Crypto.Dummy (dummyProtocolMagic)

-- This tests are quite slow, hence max success is at most 20.
spec :: Spec
spec = describe "Block properties" $ modifyMaxSuccess (min 20) $ do
        describe "mkMainHeader" $ do
            prop mainHeaderFormationDesc mainHeaderFormation
        describe "mkGenesisHeader" $ do
            prop genesisHeaderFormationDesc genesisHeaderFormation
        describe "verifyHeader" $ do
            prop verifyHeaderDesc validateGoodMainHeader
            prop invalidProtocolMagicHeaderDesc
                 validateBadProtocolMagicMainHeader
        describe "verifyHeaders" $ modifyMaxSuccess (const 1) $ do
            prop verifyHeadersDesc validateGoodHeaderChain
            emptyHeaderChain (NewestFirst [])
  where
    mainHeaderFormationDesc =
        "Manually generating a main header block and using mkMainHeader is the same"
    genesisHeaderFormationDesc =
        "Manually generating a genesis header block and using mkGenesisHeader is the same"
    verifyHeaderDesc = "Successfully verifies a correct main block header"
    invalidProtocolMagicHeaderDesc =
        "Header with invalid protocol magic does not validate"
    verifyHeadersDesc =
        "Successfully verifies a correct chain of block headers"
    verifyEmptyHsDesc = "Successfully validates an empty header chain"
    emptyHeaderChain
        :: NewestFirst [] BlockHeader
        -> Spec
    emptyHeaderChain l =
        it verifyEmptyHsDesc $ isVerSuccess $ Block.verifyHeaders dummyProtocolMagic Nothing l

-- | Both of the following tests are boilerplate - they use `mkGenericHeader` to create
-- headers and then compare these with manually built headers.
--
-- This is to keep vigilant over changes in the behavior of `mkGenericHeader` because of
-- the ensuing failed tests.

genesisHeaderFormation
    :: Maybe BlockHeader
    -> EpochIndex
    -> GenesisBody
    -> Property
genesisHeaderFormation prevHeader epoch body = header === manualHeader
  where
    header = mkGenesisHeader
        dummyProtocolMagic
        (maybe (Left dummyGenesisHash) Right prevHeader)
        epoch
        body
    manualHeader = mkGenericBlockHeaderUnsafe
        dummyProtocolMagic
        h
        proof
        (consensus h proof)
        (GenesisExtraHeaderData $ mkAttributes ())
    h          = maybe (getGenesisHash dummyGenesisHash) headerHash prevHeader
    proof      = mkGenesisProof body
    difficulty = maybe 0 (view difficultyL) prevHeader
    consensus _ _ = GenesisConsensusData
        { _gcdEpoch      = epoch
        , _gcdDifficulty = difficulty
        }

mainHeaderFormation
    :: Maybe BlockHeader
    -> SlotId
    -> Either SecretKey (SecretKey, SecretKey)
    -> MainBody
    -> MainExtraHeaderData
    -> Property
mainHeaderFormation prevHeader slotId signer body extra =
    correctSigner signer ==> (header === manualHeader)
  where
    correctSigner (Left  _        ) = True
    correctSigner (Right (i, d))    = i /= d
    header = mkMainHeader dummyProtocolMagic prevHeader' slotId sk pske body extra
    manualHeader =
        mkGenericBlockHeaderUnsafe
            dummyProtocolMagic
            prevHash
            proof
            (consensus proof)
            extra
    prevHash = maybe (getGenesisHash dummyGenesisHash) headerHash prevHeader
    prevHeader' = maybe (Left dummyGenesisHash) Right prevHeader
    proof = mkMainProof body
    (sk, pSk) = either (, Nothing) mkProxySk signer
    mkProxySk (issuerSK, delegateSK) =
        let epoch = siEpoch slotId
            delegatePK = toPublic delegateSK
            proxy = createPsk dummyProtocolMagic issuerSK delegatePK (HeavyDlgIndex epoch)
        in (delegateSK, Just proxy)
    pske = pSk >>= \proxy -> Just (proxy, pskIssuerPk proxy)
    difficulty = maybe 0 (succ . view difficultyL) prevHeader
    makeSignature toSign psk =
        BlockPSignatureHeavy $ proxySign dummyProtocolMagic SignMainBlockHeavy sk psk toSign
    signature p =
        let toSign = MainToSign prevHash p slotId difficulty extra
        in maybe
               (BlockSignature (sign dummyProtocolMagic SignMainBlock sk toSign))
               (makeSignature toSign)
               pSk
    consensus p =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey =
              maybe (toPublic sk) pskIssuerPk pSk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature p
        }

--------------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
--------------------------------------------------------------------------------

validateGoodMainHeader :: BT.HeaderAndParams -> Bool
validateGoodMainHeader (BT.getHAndP -> (params, header)) =
    isVerSuccess $ Block.verifyHeader dummyProtocolMagic params header

-- FIXME should sharpen this test to ensure that it fails with the expected
-- reason.
validateBadProtocolMagicMainHeader :: BT.HeaderAndParams -> Bool
validateBadProtocolMagicMainHeader (BT.getHAndP -> (params, header)) =
    let protocolMagic' = ProtocolMagic (getProtocolMagic dummyProtocolMagic + 1)
        header' = case header of
            BlockHeaderGenesis h -> BlockHeaderGenesis (h & gbhProtocolMagic .~ protocolMagic')
            BlockHeaderMain h    -> BlockHeaderMain    (h & gbhProtocolMagic .~ protocolMagic')
    in  not $ isVerSuccess $ Block.verifyHeader dummyProtocolMagic params header'

validateGoodHeaderChain :: BT.BlockHeaderList -> Bool
validateGoodHeaderChain (BT.BHL (l, _)) =
    isVerSuccess $ Block.verifyHeaders dummyProtocolMagic Nothing (NewestFirst l)
