{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Chain.Epoch.File
  ( parseEpoch
  , parseEpochFile
  ) where

import           Control.Monad (guard, unless)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.Trans (MonadTrans (..))
import qualified Data.Binary as B
import           Data.Binary.Get (getWord32be)
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Streaming.Prelude (Of (..), Stream)
import qualified Streaming.Prelude as S
import           System.IO (Handle, IOMode (ReadMode), hIsEOF, withBinaryFile)

import           Cardano.Binary.Class (DecoderError, decodeFull)
import           Cardano.Chain.Block.Block (Block)
import           Cardano.Chain.Block.Undo (Undo)
import           Cardano.Prelude

-- Epoch file format:
--
-- EpochFile := "Epoch data v1\n" *SlotData
-- SlotData := "blnd" BlockLength UndoLength Block Undo
-- BlockLength := Word32BE
-- UndoLength := Word32BE
-- Block := CBOR
-- Undo := CBOR

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

data ParseError
  = ParseErrorDecoder DecoderError
  | ParseErrorBinary ByteString B.ByteOffset String
  deriving (Eq, Show)
instance Exception ParseError

decodeStream :: forall m a r. (MonadThrow m) => B.Get (Either DecoderError a)
           -> Stream (Of BS.ByteString) m r
           -> Stream (Of a) m r
decodeStream getA stream = loop newDecoder stream
  where
    newDecoder = B.runGetIncremental getA
    loop :: B.Decoder (Either DecoderError a) -> Stream (Of ByteString) m r -> Stream (Of a) m r
    loop decoder s = lift (S.next s) >>= \case
      Left r -> pure r
      Right (chunk, rest) -> push decoder chunk rest
    push :: B.Decoder (Either DecoderError a) -> ByteString -> Stream (Of ByteString) m r -> Stream (Of a) m r
    push decoder chunk s = case B.pushChunk decoder chunk of
      decoder'@(B.Partial _) ->  loop decoder' s
      B.Fail remaining offset message -> lift (throwM (ParseErrorBinary remaining offset message))
      B.Done remaining _offset binaryResult ->
        case binaryResult of
          Left err     -> lift (throwM (ParseErrorDecoder err))
          Right result -> S.yield result >> push newDecoder remaining s

slotDataHeader :: LBS.ByteString
slotDataHeader = "blnd"

slotData :: B.Get (Either DecoderError (Block, Undo))
slotData = runExceptT $ do
    header <- lift $ B.getLazyByteString (LBS.length slotDataHeader)
    lift $  guard (header == slotDataHeader)
    blockSize <- lift getWord32be
    undoSize <- lift getWord32be
    block <- ExceptT $ decodeFull <$> B.getLazyByteString (fromIntegral blockSize)
    undo  <- ExceptT $ decodeFull <$> B.getLazyByteString (fromIntegral undoSize)
    pure $ (block, undo)
