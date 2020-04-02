{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Cardano.Chain.Common.Compact
  ( CompactAddress
  , toCompactAddress
  , fromCompactAddress
  )
where

import Cardano.Prelude

import Cardano.Binary (FromCBOR(..), ToCBOR(..), serialize', decodeFull')
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as SBS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.Map.Strict as Map
import Control.Monad.Fail (MonadFail(fail))

import Cardano.Crypto.Hashing
import Cardano.Chain.Common.Address (Address(..), Address')
import Cardano.Chain.Common.AddressHash (AddressHash, addressHashSize)
import Cardano.Chain.Common.Attributes (Attributes(..), UnparsedFields(..))
import Cardano.Chain.Common.AddrAttributes (AddrAttributes(..), HDAddressPayload(..))
import Cardano.Chain.Common.AddrSpendingData (AddrType(..))
import Cardano.Chain.Common.NetworkMagic (NetworkMagic(..))

--------------------------------------------------------------------------------
-- Compact Address
--------------------------------------------------------------------------------

-- | A compact in-memory representation for an 'Address'.
--
-- Convert using 'toCompactAddress' and 'fromCompactAddress'.
--
data CompactAddress =
     CAMainnet       {-# UNPACK #-} !ShortByteString
   | CAMainnetAttrs  {-# UNPACK #-} !ShortByteString
                                    !UnparsedFields
   | CAMainnetRedeem {-# UNPACK #-} !ShortByteString
                                    !UnparsedFields
   | CATestnet       {-# UNPACK #-} !ShortByteString
                     {-# UNPACK #-} !Word32
   | CATestnetAttrs  {-# UNPACK #-} !ShortByteString
                     {-# UNPACK #-} !Word32
                                    !UnparsedFields
   | CATestnetRedeem {-# UNPACK #-} !ShortByteString
                     {-# UNPACK #-} !Word32
                                    !UnparsedFields

  deriving (Eq, Ord, Generic, Show)
  deriving anyclass NFData
  deriving anyclass NoUnexpectedThunks

instance HeapWords CompactAddress where
  heapWords (CAMainnet       payload)         = 1 + heapWordsUnpacked payload
  heapWords (CAMainnetAttrs  payload attrs)   = 1 + heapWordsUnpacked payload
                                                  + heapWords attrs
  heapWords (CAMainnetRedeem payload attrs)   = 1 + heapWordsUnpacked payload
                                                  + heapWords attrs
  heapWords (CATestnet       payload _)       = 2 + heapWordsUnpacked payload
  heapWords (CATestnetAttrs  payload _ attrs) = 2 + heapWordsUnpacked payload
                                                  + heapWords attrs
  heapWords (CATestnetRedeem payload _ attrs) = 2 + heapWordsUnpacked payload
                                                  + heapWords attrs

instance FromCBOR CompactAddress where
  fromCBOR = do
    bs <- fromCBOR
    case decodeFull' bs of
      Left err      -> fail (show err)
      Right decAddr -> return $! toCompactAddress decAddr

instance ToCBOR CompactAddress where
  toCBOR = toCBOR . serialize' . fromCompactAddress

toCompactAddress :: Address -> CompactAddress
toCompactAddress Address {
                   addrRoot
                 , addrType
                 , addrAttributes =
                     Attributes {
                       attrData   =
                         AddrAttributes {
                           aaNetworkMagic
                         , aaVKDerivationPath
                         }
                     , attrRemain = attrs@(UnparsedFields attrsMap)
                     }
                 } =
    let !payload = toCompactAddressPayload addrRoot aaVKDerivationPath in
    case (aaNetworkMagic, addrType) of
      (NetworkMainOrStage,   ATVerKey)
        | Map.null attrsMap            -> CAMainnet       payload
        | otherwise                    -> CAMainnetAttrs  payload attrs
      (NetworkMainOrStage,   ATRedeem) -> CAMainnetRedeem payload attrs
      (NetworkTestnet magic, ATVerKey)
        | Map.null attrsMap            -> CATestnet       payload magic
        | otherwise                    -> CATestnetAttrs  payload magic attrs
      (NetworkTestnet magic, ATRedeem) -> CATestnetRedeem payload magic attrs

fromCompactAddress :: CompactAddress -> Address
fromCompactAddress (CAMainnet payload) =
    mkAddress NetworkMainOrStage ATVerKey attrs addrRoot aaVKDerivationPath
  where
    (addrRoot, aaVKDerivationPath) = fromCompactAddressPayload payload
    attrs = UnparsedFields Map.empty

fromCompactAddress (CAMainnetAttrs payload attrs) =
    mkAddress NetworkMainOrStage ATVerKey attrs addrRoot aaVKDerivationPath
  where
    (addrRoot, aaVKDerivationPath) = fromCompactAddressPayload payload

fromCompactAddress (CAMainnetRedeem payload attrs) =
    mkAddress NetworkMainOrStage ATRedeem attrs addrRoot aaVKDerivationPath
  where
    (addrRoot, aaVKDerivationPath) = fromCompactAddressPayload payload

fromCompactAddress (CATestnet payload magic) =
    mkAddress (NetworkTestnet magic) ATVerKey attrs addrRoot aaVKDerivationPath
  where
    (addrRoot, aaVKDerivationPath) = fromCompactAddressPayload payload
    attrs = UnparsedFields Map.empty

fromCompactAddress (CATestnetAttrs payload magic attrs) =
    mkAddress (NetworkTestnet magic) ATVerKey attrs addrRoot aaVKDerivationPath
  where
    (addrRoot, aaVKDerivationPath) = fromCompactAddressPayload payload

fromCompactAddress (CATestnetRedeem payload magic attrs) =
    mkAddress (NetworkTestnet magic) ATRedeem attrs addrRoot aaVKDerivationPath
  where
    (addrRoot, aaVKDerivationPath) = fromCompactAddressPayload payload

mkAddress !aaNetworkMagic !addrType !attrRemain !addrRoot !aaVKDerivationPath =
    Address {
      addrRoot
    , addrType
    , addrAttributes =
        Attributes {
          attrData   = AddrAttributes {
                         aaNetworkMagic
                       , aaVKDerivationPath
                       }
        , attrRemain
        }
    }


toCompactAddressPayload :: AddressHash Address'
                        -> Maybe HDAddressPayload
                        -> ShortByteString
toCompactAddressPayload addr Nothing =
    abstractHashToBytesShort addr

toCompactAddressPayload addr (Just (HDAddressPayload hd)) =
    abstractHashToBytesShort addr <> SBS.toShort hd

fromCompactAddressPayload :: ShortByteString
                          -> (AddressHash Address', Maybe HDAddressPayload)
fromCompactAddressPayload payload =
    (addr', hd')
  where
    addr' :: AddressHash Address'
    !addr' = unsafeAbstractHashFromBytes addr
    !hd' | BS.null hd = Nothing
         | otherwise  = Just (HDAddressPayload hd)

    (addr, hd) = BS.splitAt addressHashSize (SBS.fromShort payload)

