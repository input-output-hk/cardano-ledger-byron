{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Chain.Epoch.File
  ( parseEpoch
  , parseEpochFile
  ) where

import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Read as D
import           Control.Monad.Trans (MonadTrans (..))
import qualified Data.Binary as B
import           Data.Binary.Get (getWord32be)
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Word (Word32)
import           Streaming.Prelude (Of (..), Stream)
import qualified Streaming.Prelude as S
import           System.IO (Handle, IOMode (ReadMode), hIsEOF, withBinaryFile)

import           Cardano.Binary.Class (Bi (..))
import           Cardano.Chain.Block.Block (Block)
import           Cardano.Chain.Block.Undo (Undo)
import           Cardano.Prelude
import           Control.Monad (guard, unless)

-- Epoch file format:
--
-- EpochFile := "Epoch data v1\n" *SlotData
-- SlotData := "blnd" BlockLength UndoLength Block Undo
-- BlockLength := Word32BE
-- UndoLength := Word32BE
-- Block := CBOR
-- Undo := CBOR

decodeAll :: (forall s. D.Decoder s a) -> LBS.ByteString -> Either D.DeserialiseFailure a
decodeAll decoder bytes = case D.deserialiseFromBytes decoder bytes of
  Left err -> Left err
  Right (remaining, result) ->
    if LBS.null remaining
    then Right result
    else Left (D.DeserialiseFailure 0 "Deserialisation did not consume all input.")

getBlock :: Word32 -> B.Get Block
getBlock size = do
  bytes <- B.getLazyByteString (fromIntegral size)
  case decodeAll (decode @Block) bytes of
    Left (D.DeserialiseFailure _offset message) -> fail message
    Right result                                -> pure result

getUndo :: Word32 -> B.Get Undo
getUndo size = do
  bytes <- B.getLazyByteString (fromIntegral size)
  case decodeAll (decode @Undo) bytes of
    Left (D.DeserialiseFailure _offset message) -> fail message
    Right result                                -> pure result

data UnexpectedFormat = UnexpectedFormat deriving (Show)
instance Exception UnexpectedFormat

epochHeader :: LBS.ByteString
epochHeader = "Epoch data v1\n"

parseEpoch :: Handle -> Stream (Of (Block, Undo)) IO ()
parseEpoch h = do
  header <- lift $ LBS.hGet h (fromIntegral $ LBS.length epochHeader)
  lift $ unless (header == epochHeader) $ throwM UnexpectedFormat
  decodeStream slotData (readBytes h)

parseEpochFile :: FilePath -> (Stream (Of (Block, Undo)) IO () -> IO a) -> IO a
parseEpochFile f s = withBinaryFile f ReadMode $ \h -> s (parseEpoch h)

readBytes :: Handle -> Stream (Of BS.ByteString) IO ()
readBytes h = go
  where
    go = do
      eof <- lift (hIsEOF h)
      unless eof $ do
        bytes <- lift (BS.hGet h 1024)
        S.yield bytes
        go

data ParseFailed = ParseFailed String deriving (Show)
instance Exception ParseFailed

decodeStream :: forall m a r. (MonadThrow m) => B.Get a
           -> Stream (Of BS.ByteString) m r
           -> Stream (Of a) m r
decodeStream getA stream = loop newDecoder stream
  where
    newDecoder = B.runGetIncremental getA
    loop :: B.Decoder a -> Stream (Of ByteString) m r -> Stream (Of a) m r
    loop decoder s = lift (S.next s) >>= \case
      Left r -> pure r
      Right (chunk, rest) -> push decoder chunk rest
    push :: B.Decoder a -> ByteString -> Stream (Of ByteString) m r -> Stream (Of a) m r
    push decoder chunk s = case B.pushChunk decoder chunk of
      decoder'@(B.Partial _) ->  loop decoder' s
      B.Fail _remaining _offset message -> lift (throwM (ParseFailed message))
      B.Done remaining _offset result -> S.yield result >> push newDecoder remaining s

slotDataHeader :: LBS.ByteString
slotDataHeader = "blnd"

slotData :: B.Get (Block, Undo)
slotData = do
    header <- B.getLazyByteString (LBS.length slotDataHeader)
    guard (header == slotDataHeader)
    blockSize <- getWord32be
    undoSize <- getWord32be
    block <- getBlock blockSize
    undo <- getUndo undoSize
    pure $ (block, undo)
