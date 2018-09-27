module Ledger where

import Ledger.Abstract
import qualified Data.ByteArray        as BA
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Char8 as BS

instance Ledger.Abstract.HasHash Tx where
  hash = Crypto.hash

instance Ledger.Abstract.HasHash VKey where
  hash = Crypto.hash

instance BA.ByteArrayAccess VKey where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance BA.ByteArrayAccess Tx where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show
