{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.IP.Range where

import Data.IP.Addr

import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8 hiding (Fail, Done, parse, take)
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Monoid ((<>))
import Data.Word (Word8)

data AddrRange a = AddrRange a Word8
  deriving (Eq, Ord)

makeAddrRange :: Address a => a -> Int -> AddrRange a
makeAddrRange a l = AddrRange (a `mask` l) (fromIntegral l)

instance Read (AddrRange IPv4) where
  readsPrec _ s = let bs = ByteString.pack s
                  in case parse ip4range bs of
                       Fail _ _ _ -> []
                       Done rest ip -> [(ip, ByteString.unpack rest)]

ip4range :: Parser (AddrRange IPv4)
ip4range = do ip <- ip4
              _ <- char '/'
              m <- decimal :: Parser Int
              when (m < 0 || m > 32) $
                fail "invalid mask"
              return (makeAddrRange ip m)

showIP4Range :: AddrRange IPv4 -> ByteString
showIP4Range (AddrRange a m) = showIPv4 a <> "/" <> mb
  where mb = ByteString.pack $ show m

instance Show (AddrRange IPv4) where
  show = ByteString.unpack . showIP4Range

instance Read (AddrRange IPv6) where
  readsPrec _ s = let bs = ByteString.pack s
                  in case parse ip6range bs of
                       Fail _ _ _ -> []
                       Done rest ip -> [(ip, ByteString.unpack rest)]

ip6range :: Parser (AddrRange IPv6)
ip6range = do ip <- ip6
              _ <- char '/'
              m <- decimal :: Parser Int
              when (m < 0 || m > 128) $
                fail "invalid mask"
              return (makeAddrRange ip m)

showIP6Range :: AddrRange IPv6 -> ByteString
showIP6Range (AddrRange a m) = showIPv6 a <> "/" <> mb
  where mb = ByteString.pack $ show m

instance Show (AddrRange IPv6) where
  show = ByteString.unpack . showIP6Range

iprange :: Parser (AddrRange IPAddr)
iprange = do ip <- ipaddr
             _ <- char '/'
             m <- decimal :: Parser Int
             when (m < 0 || m > 128) $
               fail "invalid mask"
             return (makeAddrRange ip m)

showIPRange :: AddrRange IPAddr -> ByteString
showIPRange (AddrRange a m) = showIPAddr a <> "/" <> mb
  where mb = ByteString.pack $ show m

instance Read (AddrRange IPAddr) where
  readsPrec _ s = let bs = ByteString.pack s
                  in case parse iprange bs of
                       Fail _ _ _ -> []
                       Done rest ip -> [(ip, ByteString.unpack rest)]


instance Show (AddrRange IPAddr) where
  show = ByteString.unpack . showIPRange
