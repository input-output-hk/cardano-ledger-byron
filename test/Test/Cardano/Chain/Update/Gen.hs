module Test.Cardano.Chain.Update.Gen
       ( genApplicationName
       , genBlockVersion
       , genBlockVersionData
       , genBlockVersionModifier
       , genSoftforkRule
       , genSoftwareVersion
       , genSystemTag
       , genUpdateData
       , genPayload
       , genProof
       , genProposal
       , genProposalBody
       , genProposals
       , genVote
       , genUpId
       , genUpsData
       , genVoteId
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import qualified Data.Map.Strict as Map
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Chain.Common (mkAttributes)
import           Cardano.Chain.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), BlockVersionModifier (..),
                     Payload (..), Proof, Proposal (..), ProposalBody (..),
                     Proposals, SoftforkRule (..), SoftwareVersion (..),
                     SystemTag (..), UpId, UpdateData (..), Vote (..), VoteId,
                     mkVote)
import           Cardano.Crypto (ProtocolMagic)

import           Test.Cardano.Chain.Common.Gen (genCoinPortion,
                     genScriptVersion, genTxFeePolicy)
import           Test.Cardano.Chain.Slotting.Gen (genEpochIndex, genFlatSlotId)
import           Test.Cardano.Crypto.Gen (genAbstractHash, genHashRaw,
                     genPublicKey, genSecretKey, genSignature)


genApplicationName :: Gen ApplicationName
genApplicationName =
    ApplicationName <$> Gen.text (Range.constant 0 10) Gen.alphaNum

genBlockVersion :: Gen BlockVersion
genBlockVersion =
    BlockVersion
        <$> Gen.word16 Range.constantBounded
        <*> Gen.word16 Range.constantBounded
        <*> Gen.word8 Range.constantBounded

genBlockVersionData :: Gen BlockVersionData
genBlockVersionData =
    BlockVersionData
        <$> genScriptVersion
        <*> genNominalDiffTime
        <*> genNatural
        <*> genNatural
        <*> genNatural
        <*> genNatural
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genFlatSlotId
        <*> genSoftforkRule
        <*> genTxFeePolicy
        <*> genEpochIndex

genBlockVersionModifier :: Gen BlockVersionModifier
genBlockVersionModifier =
    BlockVersionModifier
        <$> Gen.maybe genScriptVersion
        <*> Gen.maybe genNominalDiffTime
        <*> Gen.maybe genNatural
        <*> Gen.maybe genNatural
        <*> Gen.maybe genNatural
        <*> Gen.maybe genNatural
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genFlatSlotId
        <*> Gen.maybe genSoftforkRule
        <*> Gen.maybe genTxFeePolicy
        <*> Gen.maybe genEpochIndex

genSoftforkRule :: Gen SoftforkRule
genSoftforkRule =
    SoftforkRule <$> genCoinPortion <*> genCoinPortion <*> genCoinPortion

genSoftwareVersion :: Gen SoftwareVersion
genSoftwareVersion =
    SoftwareVersion <$> genApplicationName <*> Gen.word32 Range.constantBounded

genSystemTag :: Gen SystemTag
genSystemTag = SystemTag <$> Gen.text (Range.constant 0 10) Gen.alphaNum

genUpdateData :: Gen UpdateData
genUpdateData =
    UpdateData <$> genHashRaw <*> genHashRaw <*> genHashRaw <*> genHashRaw

genPayload :: ProtocolMagic -> Gen Payload
genPayload pm =
    Payload <$> Gen.maybe (genProposal pm) <*> Gen.list
        (Range.linear 0 10)
        (genVote pm)

genProof :: ProtocolMagic -> Gen Proof
genProof pm = genAbstractHash (genPayload pm)

genProposal :: ProtocolMagic -> Gen Proposal
genProposal pm =
    Proposal
        <$> genProposalBody
        <*> genPublicKey
        <*> genSignature pm genProposalBody

genProposals :: ProtocolMagic -> Gen Proposals
genProposals pm =
    Gen.map (Range.linear 0 10) ((,) <$> genUpId pm <*> genProposal pm)

genProposalBody :: Gen ProposalBody
genProposalBody =
    ProposalBody
        <$> genBlockVersion
        <*> genBlockVersionModifier
        <*> genSoftwareVersion
        <*> genUpsData
        <*> pure (mkAttributes ())

genUpId :: ProtocolMagic -> Gen UpId
genUpId pm = genAbstractHash (genProposal pm)

genUpsData :: Gen (Map SystemTag UpdateData)
genUpsData = do
    hMapSize   <- Gen.int (Range.linear 0 20)
    sysTagList <- Gen.list (Range.singleton hMapSize) genSystemTag
    upDataList <- Gen.list (Range.singleton hMapSize) genUpdateData
    pure $ Map.fromList $ zip sysTagList upDataList

genVote :: ProtocolMagic -> Gen Vote
genVote pm = mkVote pm <$> genSecretKey <*> genUpId pm <*> Gen.bool

genVoteId :: ProtocolMagic -> Gen VoteId
genVoteId pm = (,,) <$> genUpId pm <*> genPublicKey <*> Gen.bool
