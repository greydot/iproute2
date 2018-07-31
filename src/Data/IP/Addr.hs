{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.IP.Addr where

import Control.Applicative ((<|>), many)
import Control.Monad (when,void)
import Data.Attoparsec.ByteString.Char8 hiding (Fail, Done, parse, take)
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Bits
import Data.Char (digitToInt)
import Data.DoubleWord (DoubleWord(..),Word128)
import Data.Foldable (foldl')
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Word (Word16,Word32)
import Numeric (showHex)

newtype IPv4 = IPv4 Word32
  deriving (Eq,Ord,Enum,Bounded,Bits,FiniteBits)

newtype IPv6 = IPv6 Word128
  deriving (Eq,Ord,Enum,Bounded,Bits,FiniteBits)

instance Show IPv4 where
  show ip = ByteString.unpack $ showIPv4 ip

showIPv4 :: IPv4 -> ByteString
showIPv4 (IPv4 a) = let l = [ (a .&. 0xff000000) `shiftR` 24
                            , (a .&. 0x00ff0000) `shiftR` 16
                            , (a .&. 0x0000ff00) `shiftR` 8
                            ,  a .&. 0x000000ff
                            ]
                    in mconcat $ intersperse "." $ map s l
  where
    s = ByteString.pack . show

instance Read IPv4 where
  readsPrec _ s = let bs = ByteString.pack s
                  in case parse ip4 bs of
                       Fail _ _ _ -> []
                       Done rest ip -> [(ip, ByteString.unpack rest)]

instance IsString IPv4 where
  fromString = read

ip4 :: Parser IPv4
ip4 = do ns <- word32 `sepBy1'` char '.'
         when (length ns /= 4) $
           fail "IPv4: wrong number of octets"
         when (any (> 255) ns) $
           fail "IPv4: invalid value"
         let [a0,a1,a2,a3] = ns
         pure (IPv4 $ (a0 `shift` 24)
                  .|. (a1 `shift` 16)
                  .|. (a2 `shift` 8)
                  .|.  a3
              )
  where
    word32 :: Parser Word32
    word32 = 0 <$ char '0' <|> toW8 <$> oneOf ['1' .. '9'] <*> many digit
    toW8 n ns = foldl' (\x y -> x * 10 + y) 0 . map (fromIntegral . digitToInt) $ n : ns

instance Show IPv6 where
  show ip = ByteString.unpack $ showIPv6 ip

showIPv6 :: IPv6 -> ByteString
showIPv6 ip
    -- IPv4-mapped IPv6 address
    | all (== 0) [w0,w1,w2,w3,w4] && w5 == 0xffff = "::ffff:" <> showIPv4 (IPv4 $ fromHiAndLo w6 w7)
    -- IPv4-Compatible IPv6 Address (exclude IPRange ::/112)
    | all (== 0) [w0,w1,w2,w3,w4,w5] && w6 >= 1 = "::" <> showIPv4 (IPv4 $ fromHiAndLo w6 w7)
    -- IPv6 with ::
    | end - begin > 1 = showFields prefix <> "::" <> showFields suffix
    -- IPv6 without ::
    | otherwise = showFields fields
  where
    fields@[w0,w1,w2,w3,w4,w5,w6,w7] = ip6ToWords ip
    prefix = take begin fields  -- fields before "::"
    suffix = drop end fields    -- fields after "::"
    begin = end + diff          -- the longest run of zeros
    (diff, end) = minimum $
        scanl (\c i -> if i == 0 then c - 1 else 0) 0 fields `zip` [0..]
    showFields = mconcat . intersperse ":" . map (\f -> ByteString.pack $ showHex f "")

instance Read IPv6 where
  readsPrec _ s = let bs = ByteString.pack s
                  in case parse ip6 bs of
                       Fail _ _ _ -> []
                       Done rest ip -> [(ip, ByteString.unpack rest)]

instance IsString IPv6 where
  fromString = read

ip6 :: Parser IPv6
ip6 = ip4embedded
      -- Matches ::1
  <|> do _ <- colons
         bs <- hexcolon
         let l = length bs
         when (l > 7) $
           fail "Ipv6 address too long"
         pure (wordsToIP6 $ take (8 - l) zs ++ bs)
      -- Matches 2001:db8::1
  <|> do pref <- hexcolon
         _ <- colons
         suf <- hexcolon
         let l = length pref + length suf
         when (l > 7) $
           fail "Invalid IPv6"
         pure $ wordsToIP6 $ pref ++ take (8 - l) zs ++ suf
      -- Matches 2001:db8::
  <|> do pref <- hexcolon
         _ <- colons
         let l = length pref
         when (l > 7) $
           fail "Invalid IPv6"
         pure $ wordsToIP6 $ pref ++ take (8 - l) zs
  <|> do bs <- hexcolon
         when (length bs /= 8) $
           fail "Invalid IPv6"
         pure $ wordsToIP6 bs
  where
    colons = void $ string "::"
    hex = do n <- hexadecimal :: Parser Word
             when (n > 65535) $
               fail "hex is too large"
             pure (fromIntegral n :: Word16)
    hexcolon = hex `sepBy1'` char ':'
    ip4embedded = -- Matches ::ffff:192.168.0.1
                  -- XXX: Check that bs here is equal to ::ffff: ?
                  do _ <- colons
                     bs <- beforeEmbedded
                     emb <- splitIP4 <$> ip4
                     when (length bs > 5) $
                       fail "Invalid address length"
                     let zl = 8 - length bs - 2
                     pure (wordsToIP6 $ take zl zs ++ bs ++ emb)
                  -- Matches 2001:db8::192.168.0.1
              <|> do bs <- hexcolon -- beforeEmbedded
                     _ <- colons
                     emb <- splitIP4 <$> ip4
                     let zl = 8 - length bs - 2
                     when (length bs > 6) $
                       fail "Invalid address length"
                     pure (wordsToIP6 $ bs ++ take zl zs ++ emb)
              <|> do bs <- hexcolon
                     _ <- char ':'
                     when (length bs /= 6) $
                       fail "Invalid address"
                     emb <- splitIP4 <$> ip4
                     pure (wordsToIP6 $ bs ++ emb)
    beforeEmbedded = many (hex <* char ':')
    zs = repeat 0

splitIP4 :: IPv4 -> [Word16]
splitIP4 (IPv4 a) = [hiWord a, loWord a]

wordsToIP6 :: [Word16] -> IPv6
wordsToIP6 ws | length ws /= 8 = error "wordsToIP6: invalid list length"
              | otherwise = IPv6 $ go ws
  where
    go = foldl' (\r w -> r * 0x10000 + fromIntegral w) 0

ip6ToWords :: IPv6 -> [Word16]
ip6ToWords (IPv6 a) = reverse $ foldr f [] [0..7]
  where s = (16 *)
        f n r = fromIntegral ((a .&. (0xffff `shift` s n)) `shiftR` s n) : r

oneOf :: [Char] -> Parser Char
oneOf xs = satisfy (`elem` xs)

data IPAddr = IP4 {-# UNPACK #-} !IPv4
            | IP6 {-# UNPACK #-} !IPv6
  deriving (Eq, Ord)

showIPAddr :: IPAddr -> ByteString
showIPAddr (IP4 ip) = showIPv4 ip
showIPAddr (IP6 ip) = showIPv6 ip

instance Show IPAddr where
  show = ByteString.unpack . showIPAddr

ipaddr :: Parser IPAddr
ipaddr = IP4 <$> ip4
     <|> IP6 <$> ip6

instance Read IPAddr where
  readsPrec _ s = let bs = ByteString.pack s
                  in case parse ipaddr bs of
                       Fail _ _ _ -> []
                       Done rest ip -> [(ip, ByteString.unpack rest)]

instance IsString IPAddr where
  fromString = read

class Address a where
  mask :: a -> Int -> a
  default mask :: FiniteBits a => a -> Int -> a
  mask a l = let m = foldl' setBit zeroBits [finiteBitSize a - l..finiteBitSize a]
             in a .&. m

  addrLength :: a -> Int
  default addrLength :: FiniteBits a => a -> Int
  addrLength = finiteBitSize

instance Address IPv4
instance Address IPv6

instance Address IPAddr where
  mask (IP4 a) l = IP4 (mask a l)
  mask (IP6 a) l = IP6 (mask a l)

  addrLength (IP4 a) = addrLength a
  addrLength (IP6 a) = addrLength a
