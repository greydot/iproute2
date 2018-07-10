{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.IP.Types where

import Control.Applicative ((<|>), many)
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8 hiding (parse, maybeResult)
import Data.Attoparsec.ByteString.Lazy (parse, maybeResult)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Bits
import Data.Char (digitToInt)
import Data.DoubleWord (Word128)
import Data.Foldable (foldl',foldr1)
import Data.List (intersperse)
import Data.Word (Word8,Word32)

import Debug.Trace

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
                            , a .&. 0x000000ff
                            ]
                    in mconcat $ intersperse "." $ map s l
  where
    s = ByteString.pack . show

instance Read IPv4 where
  readsPrec _ s = undefined
 
parseIPv4 :: ByteString -> Maybe IPv4
parseIPv4  = maybeResult . parse ip4
  where
    ip4 = do ns <- word32 `sepBy1'` char '.'
             when (length ns /= 4) $
               fail "IPv4: wrong number of octets"
             when (any (> 255) ns) $
               fail "IPv4: invalid value"
             let [a0,a1,a2,a3] = ns
             pure (IPv4 $ (a0 `shift` 24)
                      .|. (a1 `shift` 16)
                      .|. (a2 `shift` 8)
                      .|. a3
                  )
    word32 :: Parser Word32
    word32 = 0 <$ char '0' <|> toW8 <$> oneOf ['1' .. '9'] <*> many digit
    toW8 n ns = foldl' (\x y -> x * 10 + y) 0 . map (fromIntegral . digitToInt) $ n : ns

oneOf :: [Char] -> Parser Char
oneOf xs = satisfy (`elem` xs)
