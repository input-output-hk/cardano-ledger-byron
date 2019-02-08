{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- | This module provides functionality for translating abstract blocks into
-- concrete blocks. The abstract blocks are generated according the small-step
-- rules for the block chain (also called the blockchain specification).
module Test.Cardano.Chain.Block.Elaboration
  ( elaborate
  )
where

import Cardano.Prelude

import qualified Data.ByteString.Lazy as LBS

import qualified Cardano.Binary.Class as Binary

import qualified Cardano.Chain.Block as Concrete
import qualified Cardano.Spec.Chain.STS.Block as Abstract


elaborate
  :: Concrete.ChainValidationState
  -> Abstract.Block
  -> Concrete.ABlock ByteString
elaborate = undefined

annotateBlock :: Concrete.Block -> Concrete.ABlock ByteString
annotateBlock block =
  case Binary.decodeFullDecoder "Block" Concrete.decodeABlock bytes of
    Left _ -> panic "This function should be able to decode the block it encoded."
    Right res ->
      map (LBS.toStrict . Binary.slice bytes) res
  where
    bytes = Binary.serializeEncoding (Concrete.encodeBlock block)
