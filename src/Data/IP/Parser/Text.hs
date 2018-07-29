{-# LANGUAGE OverloadedStrings #-}
module Data.IP.Parser.Text ( ip4
                           , showIPv4
                           , ip6
                           , showIPv6
                           , ipaddr
                           , showIPAddr
                           ) where

import Data.IP.Addr (IPv4(..), IPv6(..), IPAddr(..))

import Control.Applicative ((<|>), many)
import Control.Monad (when,void)
import Data.Attoparsec.Text hiding (take)
import Data.Bits
import Data.Char (digitToInt)
import Data.DoubleWord (DoubleWord(..))
import Data.Foldable (foldl')
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word32)
import Numeric (showHex)

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

showIPv4 :: IPv4 -> Text
showIPv4 (IPv4 a) = let l = [ (a .&. 0xff000000) `shiftR` 24
                            , (a .&. 0x00ff0000) `shiftR` 16
                            , (a .&. 0x0000ff00) `shiftR` 8
                            ,  a .&. 0x000000ff
                            ]
                    in mconcat $ intersperse "." $ map s l
  where
    s = Text.pack . show


ip6 :: Parser IPv6
ip6 = ip4embedded
      -- Matches ::1
  <|> do _ <- colons
         bs <- hexcolon
         let l = length bs
         when (l > 7) $
           fail "Ipv6 address too long"
         pure (wordsToIP6 $ take (8 - l) (repeat 0) ++ bs)
      -- Matches 2001:db8::1
  <|> do pref <- hexcolon
         _ <- colons
         suf <- hexcolon
         let l = length pref + length suf
         when (l > 7) $
           fail "Invalid IPv6"
         pure $ wordsToIP6 $ pref ++ take (8 - l) (repeat 0) ++ suf
      -- Matches 2001:db8::
  <|> do pref <- hexcolon
         _ <- colons
         let l = length pref
         when (l > 7) $
           fail "Invalid IPv6"
         pure $ wordsToIP6 $ pref ++ take (8 - l) (repeat 0)
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
                     pure (wordsToIP6 $ bs ++ emb)
                  -- Matches 2001:db8::192.168.0.1
              <|> do bs <- beforeEmbedded
                     _ <- colons
                     emb <- splitIP4 <$> ip4
                     when (length bs > 6) $
                       fail "Invalid address length"
                     pure (wordsToIP6 $ bs ++ emb)
              <|> do bs <- beforeEmbedded
                     when (length bs /= 6) $
                       fail "Invalid address"
                     emb <- splitIP4 <$> ip4
                     pure (wordsToIP6 $ bs ++ emb)
    beforeEmbedded = many (hex <* char ':')

showIPv6 :: IPv6 -> Text
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
    showFields = mconcat . intersperse ":" . map (\f -> Text.pack $ showHex f "")


ipaddr :: Parser IPAddr
ipaddr = IP4 <$> ip4
     <|> IP6 <$> ip6

showIPAddr :: IPAddr -> Text
showIPAddr (IP4 ip) = showIPv4 ip
showIPAddr (IP6 ip) = showIPv6 ip

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
