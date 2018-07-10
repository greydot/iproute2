{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.IP.Types where

import Data.Attoparsec.ByteString.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.Bits (Bits,FiniteBits)
import Data.DoubleWord (Word128)
import Data.Word (Word8,Word32)

newtype IPv4 = IPv4 Word32
  deriving (Eq,Ord,Enum,Bounded,Bits,FiniteBits)

newtype IPv6 = IPv6 Word128
  deriving (Eq,Ord,Enum,Bounded,Bits,FiniteBits)

parseIPv4 :: ByteString -> Maybe IPv4
parseIPv4 bs = undefined
