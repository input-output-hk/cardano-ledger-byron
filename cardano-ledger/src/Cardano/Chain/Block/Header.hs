{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Chain.Block.Header
  (
  -- * Header
  Header(..)

  -- * Header Constructors
  , mkHeader
  , mkHeaderExplicit

  -- * Header Accessors
  , headerProtocolMagicId
  , headerPrevHash
  , headerSlot
  , headerIssuer
  , headerLength
  , headerDifficulty
  , headerToSign

  -- * Header Binary Serialization
  , toCBORHeader
  , toCBORHeaderToHash
  , fromCBORHeader
  , fromCBORHeaderToHash
  , wrapHeaderBytes

  -- * Header Formatting
  , renderHeader

  -- * Boundary Header
  , ABoundaryHeader(..)
  , toCBORABoundaryHeader
  , fromCBORABoundaryHeader
  , boundaryHeaderHashAnnotated
  , wrapBoundaryBytes

  -- * HeaderHash
  , HeaderHash
  , headerHashF
  , hashHeader
  , genesisHeaderHash

  -- * BlockSignature
  , BlockSignature(..)

  -- * ToSign
  , ToSign(..)
  , recoverSignedBytes
  )
where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map (singleton)
import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, build, int)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , AnnotatedDecoder
  , ByteSpan
  , Decoded(..)
  , Decoder
  , DecoderError(..)
  , Encoding
  , FromCBOR(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , annotatedDecoder
  , dropBytes
  , dropInt32
  , encodeListLen
  , enforceSize
  , serializeEncoding
  , withSlice'
  , serialize'
  , encodePreEncoded
  , serializeEncoding'
  )
import Cardano.Chain.Block.Body (Body)
import Cardano.Chain.Block.Boundary
  (fromCBORBoundaryConsensusData, dropBoundaryExtraHeaderDataRetainGenesisTag)
import Cardano.Chain.Block.Proof (Proof(..), mkProof)
import Cardano.Chain.Common (ChainDifficulty(..), dropEmptyAttributes)
import qualified Cardano.Chain.Delegation.Certificate as Delegation
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Slotting
  ( EpochSlots
  , SlotNumber(..)
  , EpochAndSlotCount(..)
  , WithEpochSlots(WithEpochSlots)
  , toSlotNumber
  , fromSlotNumber
  )
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto
  ( Hash
  , ProtocolMagicId(..)
  , Signature
  , SignTag(..)
  , SigningKey
  , VerificationKey
  , hash
  , hashDecoded
  , hashHexF
  , hashRaw
  , sign
  , unsafeAbstractHash
  )


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

data Header = Header
  { aHeaderProtocolMagicId :: !(Annotated ProtocolMagicId ByteString)
  , aHeaderPrevHash        :: !(Annotated HeaderHash ByteString)
  -- ^ Pointer to the header of the previous block
  , aHeaderSlot            :: !(Annotated SlotNumber ByteString)
  -- ^ The slot number this block was published for
  , aHeaderDifficulty      :: !(Annotated ChainDifficulty ByteString)
  -- ^ The chain difficulty up to this block
  , headerProtocolVersion  :: !ProtocolVersion
  -- ^ The version of the protocol parameters this block is using
  , headerSoftwareVersion  :: !SoftwareVersion
  -- ^ The software version this block was published from
  , headerProof            :: !Proof
  -- ^ Proof of body
  , headerGenesisKey       :: !VerificationKey
  -- ^ The genesis key that is delegating to publish this block
  , headerSignature        :: !BlockSignature
  -- ^ The signature of the block, which contains the delegation certificate
  , headerExtraAnnotation  :: ByteString
  -- ^ An annotation that captures the bytes from the deprecated ExtraHeaderData
  , headerAnnotation       :: ByteString
  -- ^ An annotation that captures the full header bytes
  } deriving (Eq, Show, Generic, NFData)


--------------------------------------------------------------------------------
-- Header Constructors
--------------------------------------------------------------------------------

-- | Smart constructor for 'Header'
mkHeader
  :: ProtocolMagicId
  -> Either GenesisHash Header
  -> EpochSlots
  -- ^ Number of slots per epoch. This is needed to convert the slot number to
  -- the legacy format used in 'ToSign', where a slot is identified by the
  -- epoch to which it belongs and the offset within that epoch (counted in
  -- number of slots).
  -> SlotNumber
  -> SigningKey
  -- ^ The 'SigningKey' used for signing the block
  -> Delegation.Certificate
  -- ^ A certificate of delegation from a genesis key to the 'SigningKey'
  -> Body
  -> ProtocolVersion
  -> SoftwareVersion
  -> Header
mkHeader pm prevHeader epochSlots = mkHeaderExplicit
  pm
  prevHash
  difficulty
  epochSlots
 where
  prevHash   = either genesisHeaderHash hashHeader prevHeader
  difficulty = either
    (const $ ChainDifficulty 0)
    (succ . headerDifficulty)
    prevHeader

-- | Make a 'Header' for a given slot, with a given body, parent hash,
--   and difficulty. This takes care of some signing and consensus data.
mkHeaderExplicit
  :: ProtocolMagicId
  -> HeaderHash
  -- ^ Parent
  -> ChainDifficulty
  -> EpochSlots
  -- ^ See 'mkHeader'.
  -> SlotNumber
  -> SigningKey
  -- ^ The 'SigningKey' used for signing the block
  -> Delegation.Certificate
  -- ^ A certificate of delegation from a genesis key to the 'SigningKey'
  -> Body
  -> ProtocolVersion
  -> SoftwareVersion
  -> Header
mkHeaderExplicit pm prevHash difficulty epochSlots slotNumber sk dlgCert body pv sv
  = Header
    (Annotated pm pmBytes)
    (Annotated prevHash prevHashBytes)
    (Annotated slotNumber slotNumberBytes)
    (Annotated difficulty difficultyBytes)
    pv
    sv
    proof
    genesisVK
    sig
    headerExtraBytes
    headerBytes
 where
  pmBytes = serialize' pm
  prevHashBytes = serialize' prevHash
  slotNumberBytes = serialize' (fromSlotNumber epochSlots $ slotNumber)
  difficultyBytes = serialize' difficulty

  proof     = mkProof body
  genesisVK = Delegation.issuerVK dlgCert
  sig       = BlockSignature dlgCert $ sign pm (SignBlock genesisVK) sk toSign
  toSign    = ToSign prevHash proof epochAndSlotCount difficulty pv sv
  epochAndSlotCount = fromSlotNumber epochSlots slotNumber

  headerExtraBytes = serializeEncoding' $ toCBORBlockVersions pv sv

  headerBytes = serializeEncoding' $
    encodeListLen 5
      <> encodePreEncoded pmBytes
      <> encodePreEncoded prevHashBytes
      <> encodePreEncoded (serialize' proof)
      <> (  encodeListLen 4
         <> encodePreEncoded slotNumberBytes
         <> toCBOR genesisVK
         <> encodePreEncoded difficultyBytes
         <> toCBOR sig
         )
      <> encodePreEncoded headerExtraBytes



--------------------------------------------------------------------------------
-- Header Accessors
--------------------------------------------------------------------------------

headerProtocolMagicId :: Header -> ProtocolMagicId
headerProtocolMagicId = unAnnotated . aHeaderProtocolMagicId

headerPrevHash :: Header -> HeaderHash
headerPrevHash = unAnnotated . aHeaderPrevHash

headerSlot :: Header -> SlotNumber
headerSlot = unAnnotated . aHeaderSlot

headerDifficulty :: Header -> ChainDifficulty
headerDifficulty = unAnnotated . aHeaderDifficulty

headerIssuer :: Header -> VerificationKey
headerIssuer h = case headerSignature h of
  BlockSignature cert _ -> Delegation.delegateVK cert

headerToSign :: EpochSlots -> Header -> ToSign
headerToSign epochSlots h = ToSign
  (headerPrevHash h)
  (headerProof h)
  (fromSlotNumber epochSlots $ headerSlot h)
  (headerDifficulty h)
  (headerProtocolVersion h)
  (headerSoftwareVersion h)

headerLength :: Header -> Natural
headerLength = fromIntegral . BS.length . headerAnnotation


--------------------------------------------------------------------------------
-- Header Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Header where
  toCBOR = encodePreEncoded . headerAnnotation


-- | Encode a header, without taking in to account deprecated epoch boundary
-- blocks.
toCBORHeader :: Header -> Encoding
toCBORHeader h = toCBOR h

toCBORBlockVersions :: ProtocolVersion -> SoftwareVersion -> Encoding
toCBORBlockVersions pv sv =
  encodeListLen 4
    <> toCBOR pv
    <> toCBOR sv
    -- Encoding of empty Attributes
    <> toCBOR (mempty :: Map Word8 LByteString)
    -- Hash of the encoding of empty ExtraBodyData
    <> toCBOR (hashRaw "\129\160")

fromCBORHeader :: EpochSlots -> AnnotatedDecoder s Header
fromCBORHeader epochSlots = withSlice' $ do
  lift $ enforceSize "Header" 5
  pm <- fromCBORAnnotated'
  prevHash <- fromCBORAnnotated'
  proof <- fromCBORAnnotated'
  lift $ enforceSize "ConsensusData" 4
  slot <- fmap (first (toSlotNumber epochSlots)) fromCBORAnnotated'
  genesisKey <- lift fromCBOR
  difficulty <- fromCBORAnnotated'
  sig <- fromCBORAnnotated'
  ((protocolVersion, softwareVersion), extraBytes) <- withSlice' $
    (,) <$> lift fromCBORBlockVersions
  pure $ Header
    pm
    prevHash
    slot
    difficulty
    protocolVersion
    softwareVersion
    proof
    genesisKey
    sig
    extraBytes

{-

fromCBORAHeader :: EpochSlots -> Decoder s (AHeader ByteSpan)
fromCBORAHeader epochSlots = do
  Annotated
    ( pm
    , prevHash
    , proof
    , (slot, genesisKey, difficulty, sig)
    , Annotated (protocolVersion, softwareVersion) extraByteSpan
    )
    byteSpan <-
    annotatedDecoder $ do
      enforceSize "Header" 5
      (,,,,)
        <$> fromCBORAnnotated
        <*> fromCBORAnnotated
        <*> fromCBORAnnotated'
        <*> do
              enforceSize "ConsensusData" 4
              (,,,)
                -- Next, we decode a 'EpochAndSlotCount' into a 'SlotNumber': the `EpochAndSlotCount`
                -- used in 'AConsensusData' is encoded as a epoch and slot-count
                -- pair.
                <$> fmap (first (toSlotNumber epochSlots)) fromCBORAnnotated
                <*> fromCBOR
                <*> fromCBORAnnotated
                <*> fromCBOR
        <*> annotatedDecoder fromCBORBlockVersions
  pure $ AHeader
    pm
    prevHash
    slot
    difficulty
    protocolVersion
    softwareVersion
    proof
    genesisKey
    sig
    byteSpan
    extraByteSpan
-}
fromCBORBlockVersions :: Decoder s (ProtocolVersion, SoftwareVersion)
fromCBORBlockVersions = do
  enforceSize "BlockVersions" 4
  (,) <$> fromCBOR <*> fromCBOR <* dropEmptyAttributes <* dropBytes

instance Decoded Header where
  type BaseType Header = Header
  recoverBytes = headerAnnotation

-- | Encode a 'Header' accounting for deprecated epoch boundary blocks
--
--   This encoding is only used when hashing the header for backwards
--   compatibility, but should not be used when serializing a header within a
--   block
toCBORHeaderToHash :: Header -> Encoding
toCBORHeaderToHash h =
  encodeListLen 2 <> toCBOR (1 :: Word) <> toCBOR h

fromCBORHeaderToHash :: EpochSlots -> AnnotatedDecoder s (Maybe Header)
fromCBORHeaderToHash epochSlots = do
  lift $ enforceSize "Header" 2
  lift (fromCBOR @Word) >>= \case
    0 -> do
      void $ lift $ fromCBORABoundaryHeader
      pure Nothing
    1 -> Just <$!> fromCBORHeader epochSlots
    t -> lift $ cborError $ DecoderErrorUnknownTag "Header" (fromIntegral t)


--------------------------------------------------------------------------------
-- Header Formatting
--------------------------------------------------------------------------------

instance B.Buildable (WithEpochSlots Header) where
  build (WithEpochSlots _ header) = renderHeader header

renderHeader :: Header -> Builder
renderHeader header = bprint
  ( "Header:\n"
  . "    hash: " . hashHexF . "\n"
  . "    previous block: " . hashHexF . "\n"
  . "    slot: " . build . "\n"
  . "    difficulty: " . int . "\n"
  . "    protocol: v" . build . "\n"
  . "    software: " . build . "\n"
  . "    genesis key: " . build . "\n"
  . "    signature: " . build
  )
  headerHash
  (headerPrevHash header)
  (headerSlot header)
  (unChainDifficulty $ headerDifficulty header)
  (headerProtocolVersion header)
  (headerSoftwareVersion header)
  (headerGenesisKey header)
  (headerSignature header)
 where
  headerHash :: HeaderHash
  headerHash = hashHeader header


--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

-- | 'Hash' of block header
type HeaderHash = Hash Header

-- | Specialized formatter for 'HeaderHash'
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- | Extract the genesis hash and cast it into a header hash.
genesisHeaderHash :: GenesisHash -> HeaderHash
genesisHeaderHash = coerce . unGenesisHash

-- | These bytes must be prepended when hashing raw boundary header data
--
--   In the Byron release, hashes were taken over a data type that was never
--   directly serialized to the blockchain, so these magic bytes cannot be
--   determined from the raw header data.
--
--   These bytes are from `encodeListLen 2 <> toCBOR (1 :: Word8)`
wrapHeaderBytes :: ByteString -> ByteString
wrapHeaderBytes = mappend "\130\SOH"

-- | Hash the serialised representation of a `Header`
--
--   For backwards compatibility we have to take the hash of the header
--   serialised with 'toCBORHeaderToHash'
hashHeader :: Header -> HeaderHash
hashHeader = unsafeAbstractHash . serializeEncoding . toCBORHeaderToHash


--------------------------------------------------------------------------------
-- BoundaryHeader
--------------------------------------------------------------------------------

data ABoundaryHeader a = ABoundaryHeader
  { boundaryPrevHash         :: !(Either GenesisHash HeaderHash)
  , boundaryEpoch            :: !Word64
  , boundaryDifficulty       :: !ChainDifficulty
  , boundaryHeaderAnnotation :: !a
  } deriving (Eq, Show, Functor)

instance Decoded (ABoundaryHeader ByteString) where
  type BaseType (ABoundaryHeader ByteString) = ABoundaryHeader ()
  recoverBytes = boundaryHeaderAnnotation

-- | Compute the hash of a boundary block header from its annotation.
-- It uses `wrapBoundaryBytes`, for the hash must be computed on the header
-- bytes tagged with the CBOR list length and tag discriminator, which is
-- the encoding chosen by cardano-sl.
boundaryHeaderHashAnnotated :: ABoundaryHeader ByteString -> HeaderHash
boundaryHeaderHashAnnotated = coerce . hashDecoded . fmap wrapBoundaryBytes

-- | Encode from a boundary header with any annotation. This does not
-- necessarily invert `fromCBORBoundaryHeader`, because that decoder drops
-- information that this encoder replaces, such as the body proof (assumes
-- the body is empty) and the extra header data (sets it to empty map).
toCBORABoundaryHeader :: ProtocolMagicId -> ABoundaryHeader a -> Encoding
toCBORABoundaryHeader pm hdr =
    encodeListLen 5
      <> toCBOR pm
      <> ( case boundaryPrevHash hdr of
             Left  gh -> toCBOR (genesisHeaderHash gh)
             Right hh -> toCBOR hh
         )
      -- Body proof
      <> toCBOR (hash (mempty :: LByteString))
      -- Consensus data
      <> ( encodeListLen 2
          -- Epoch
          <> toCBOR (boundaryEpoch hdr)
          -- Chain difficulty
          <> toCBOR (boundaryDifficulty hdr)
         )
      -- Extra data
      <> ( encodeListLen 1
          <> toCBOR genesisTag
         )
  where
    -- Genesis tag to indicate the presence of a genesis hash in a non-zero
    -- epoch. See 'dropBoundaryExtraHeaderDataRetainGenesisTag' for more
    -- details on this.
    genesisTag = case (boundaryPrevHash hdr, boundaryEpoch hdr) of
      (Left _, n) | n > 0 -> Map.singleton 255 "Genesis"
      _ -> mempty :: Map Word8 LByteString

fromCBORABoundaryHeader :: Decoder s (ABoundaryHeader ByteSpan)
fromCBORABoundaryHeader = do
  Annotated header bytespan <- annotatedDecoder $ do
    enforceSize "BoundaryHeader" 5
    dropInt32
    -- HeaderHash
    hh <- fromCBOR
    -- BoundaryBodyProof
    dropBytes
    (epoch, difficulty) <- fromCBORBoundaryConsensusData
    isGen <- dropBoundaryExtraHeaderDataRetainGenesisTag
    let hh' = if epoch == 0 || isGen then Left (coerce hh) else Right hh
    pure $ ABoundaryHeader
      { boundaryPrevHash         = hh'
      , boundaryEpoch            = epoch
      , boundaryDifficulty       = difficulty
      , boundaryHeaderAnnotation = ()
      }
  pure (header { boundaryHeaderAnnotation = bytespan })

-- | These bytes must be prepended when hashing raw boundary header data
--
--   In the Byron release, hashes were taken over a data type that was never
--   directly serialized to the blockchain, so these magic bytes cannot be
--   determined from the raw header data.
wrapBoundaryBytes :: ByteString -> ByteString
wrapBoundaryBytes = mappend "\130\NUL"


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

-- | Signature of the 'Block'
--
--   We use a heavyweight delegation scheme, so the signature has two parts:
--
--   1. A delegation certificate from a genesis key to the block signer
--   2. The actual signature over `ToSign`
data BlockSignature = BlockSignature
  { delegationCertificate :: Delegation.Certificate
  , signature             :: Signature ToSign
  } deriving (Show, Eq, Generic)
    deriving anyclass NFData

instance B.Buildable BlockSignature where
  build (BlockSignature cert _) = bprint
    ( "BlockSignature:\n"
    . "  Delegation certificate: " . build
    )
    cert

instance ToCBOR BlockSignature where
  toCBOR (BlockSignature cert sig) =
    -- Tag 0 was previously used for BlockSignature (no delegation)
    -- Tag 1 was previously used for BlockPSignatureLight
    encodeListLen 2
      <> toCBOR (2 :: Word8)
      <> (encodeListLen 2 <> toCBOR cert <> toCBOR sig)

instance FromCBORAnnotated BlockSignature where
  fromCBORAnnotated' = do
    lift $ enforceSize "BlockSignature" 2
    lift fromCBOR >>= \case
      2 ->
        BlockSignature
          <$  lift (enforceSize "BlockSignature" 2)
          <*> fromCBORAnnotated'
          <*> lift fromCBOR
      t -> lift $ cborError $ DecoderErrorUnknownTag "BlockSignature" t



--------------------------------------------------------------------------------
-- ToSign
--------------------------------------------------------------------------------

-- | Produces the ByteString that was signed in the block
recoverSignedBytes
  :: EpochSlots -> Header -> Annotated ToSign ByteString
recoverSignedBytes es h = Annotated (headerToSign es h) bytes
 where
  bytes = BS.concat
    [ "\133"
    -- This is the value of Codec.CBOR.Write.toLazyByteString (encodeListLen 5)
    -- It is hard coded here because the signed bytes included it as an
    -- implementation artifact
    , (annotation . aHeaderPrevHash) h
    , (proofSerialized . headerProof) h
    , (annotation . aHeaderSlot) h
    , (annotation . aHeaderDifficulty) h
    , headerExtraAnnotation h
    ]

-- | Data to be signed in 'Block'
data ToSign = ToSign
  { tsHeaderHash      :: !HeaderHash
  -- ^ Hash of previous header in the chain
  , tsBodyProof       :: !Proof
  , tsSlot            :: !EpochAndSlotCount
  , tsDifficulty      :: !ChainDifficulty
  , tsProtocolVersion :: !ProtocolVersion
  , tsSoftwareVersion :: !SoftwareVersion
  } deriving (Eq, Show, Generic)

instance ToCBOR ToSign where
  toCBOR ts =
    encodeListLen 5
      <> toCBOR (tsHeaderHash ts)
      <> toCBOR (tsBodyProof ts)
      <> toCBOR (tsSlot ts)
      <> toCBOR (tsDifficulty ts)
      <> toCBORBlockVersions (tsProtocolVersion ts) (tsSoftwareVersion ts)

instance FromCBORAnnotated ToSign where
  fromCBORAnnotated' = do
    lift $ enforceSize "ToSign" 5
    headerHash <- lift fromCBOR
    bodyProof <- fromCBORAnnotated'
    slotCount' <- lift fromCBOR
    difficulty <- lift fromCBOR
    (protocolVersion, softwareVersion) <- lift fromCBORBlockVersions
    pure $ ToSign headerHash bodyProof slotCount' difficulty protocolVersion softwareVersion
