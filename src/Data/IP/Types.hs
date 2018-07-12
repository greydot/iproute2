{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.IP.Types where

import Control.Applicative ((<|>), many)
import Control.Monad (when,void)
import Data.Attoparsec.ByteString.Char8 hiding (Fail, Done, parse, maybeResult,take)
import Data.Attoparsec.ByteString.Lazy (Result(..), parse, maybeResult)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Bits
import Data.Char (digitToInt)
import Data.DoubleWord (DoubleWord(..),Word128)
import Data.Foldable (foldl')
import Data.List (intersperse)
import Data.Word (Word16,Word32)

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

parseIPv4 :: ByteString -> Maybe IPv4
parseIPv4  = maybeResult . parse ip4

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

showIPv6 :: IPv6 -> ByteString
showIPv6 (IPv6 a) = undefined
  where
    
  
{-
showIPv6 :: IPv6 -> ShowS
showIPv6 ip@(IP6 (a1,a2,a3,a4))
  -- IPv4-Mapped IPv6 Address
  | a1 == 0 && a2 == 0 && a3 == 0xffff =
      showString "::ffff:" . showIPv4 (IP4 a4)
  -- IPv4-Compatible IPv6 Address (exclude IPRange ::/112)
  | a1 == 0 && a2 == 0 && a3 == 0 && a4 >= 0x10000 =
      showString "::" . showIPv4 (IP4 a4)
  -- length of longest run > 1, replace it with "::"
  | end - begin > 1 =
      showFields prefix . showString "::" . showFields suffix
  -- length of longest run <= 1, don't use "::"
  | otherwise =
      showFields fields
  where
    fields = fromIPv6 ip
    showFields = foldr (.) id . intersperse (showChar ':') . map showHex
    prefix = take begin fields  -- fields before "::"
    suffix = drop end fields    -- fields after "::"
    begin = end + diff          -- the longest run of zeros
    (diff, end) = minimum $
        scanl (\c i -> if i == 0 then c - 1 else 0) 0 fields `zip` [0..]
-}

ip6 :: Parser IPv6
ip6 = undefined
  where
    colons = void $ string "::"
    hex = do n <- hexadecimal :: Parser Word
             when (n > 65535) $
               fail "hex is too large"
             pure (fromIntegral n :: Word16)
    hexcolon = hex `sepBy1'` char ':'

    ip4embedded = -- Matches ::ffff:192.168.0.1
                  do colons
                     bs <- beforeEmbedded
                     emb <- splitIP4 <$> ip4
                     pure (wordsToIP6 $ bs ++ emb)
              <|> do undefined
                     
    beforeEmbedded = many (hex <* char ':')

splitIP4 :: IPv4 -> [Word16]
splitIP4 (IPv4 a) = [hiWord a, loWord a]

wordsToIP6 :: [Word16] -> IPv6
wordsToIP6 ws | length ws > 8 = error "wordsToIP6: invalid list length"
              | otherwise = IPv6 $ go (take 8 $ ws ++ repeat 0)
  where
    go = foldr (\w r -> r * 0x10000 + fromIntegral w) 0

ip6ToWords :: IPv6 -> [Word16]
ip6ToWords (IPv6 a) = foldr (\n r -> fromIntegral (a .&. (0xffff `shift` 16 * n)) : r) [] [0..7]

{-
ip6' :: Parser [Int]
ip6' = ip4Embedded
   <|> do colon2
          bs <- option [] hexcolon
          format [] bs
   <|> try (do rs <- hexcolon
               check rs
               return rs)
   <|> do bs1 <- hexcolon2
          bs2 <- option [] hexcolon
          format bs1 bs2
  where
    hexcolon = hex `sepBy1` char ':'
    hexcolon2 = manyTill (hex <* char ':') (char ':')
    check bs = when (length bs /= 8) (fail "IPv6 address4")

ip4Embedded :: Parser [Int]
ip4Embedded = try (do colon2
                      bs <- beforeEmbedded
                      embedded <- ip4'
                      format [] (bs ++ ip4ToIp6 embedded))
              -- matches 2001:db8::192.0.2.1
          <|> try (do bs1 <- manyTill (try $ hex <* char ':') (char ':')
                      bs2 <- option [] beforeEmbedded
                      embedded <- ip4'
                      format bs1 (bs2 ++ ip4ToIp6 embedded))
              -- matches 2001:db8:11e:c00:aa:bb:192.0.2.1
          <|> try (do bs <- beforeEmbedded
                      embedded <- ip4'
                      let rs = bs ++ ip4ToIp6 embedded
                      check rs
                      return rs)
  where
    beforeEmbedded = many $ try $ hex <* char ':'
    ip4ToIp6 [a,b,c,d] = [ a `shiftL` 8 .|. b
                         , c `shiftL` 8 .|. d ]
    ip4ToIp6 _ = error "ip4ToIp6"
    check bs = when (length bs /= 8) (fail "IPv6 address4")
-}


oneOf :: [Char] -> Parser Char
oneOf xs = satisfy (`elem` xs)
