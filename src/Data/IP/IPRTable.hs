{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.IP.IPRTable ( IPRTable
                        , empty
                        , fromList
                        , toList
                        , insert
                        , lookupAll
                        , lookup
                        , delete
                        , foldlWithKey
                        , foldrWithKey
                        ) where

import Data.IP.Addr
import Data.IP.Range

import Prelude hiding (lookup)

import Data.Bits
import Data.Maybe (listToMaybe)
import GHC.Generics

{-|
  The Tree structure for IP routing table based on TRIE with
  one way branching removed. This is an abstract data type,
  so you cannot touch its inside. Please use 'insert' or 'lookup', instead.
-}
data IPRTable k a = Nil
                  | Node !(AddrRange k) !k !(Maybe a) !(IPRTable k a) !(IPRTable k a)
  deriving (Eq, Generic, Generic1, Functor, Foldable, Traversable)

deriving instance ( Show k
                  , Show a
                  , Show (AddrRange k)
                  ) => Show (IPRTable k a)

{-| The 'empty' function returns an empty IP routing table.

>>> (empty :: IPRTable IPv4 ()) == fromList []
True
-}
empty :: Address k => IPRTable k a
empty = Nil

fromList :: (Address k, Bits k) => [(AddrRange k, v)] -> IPRTable k v
fromList = foldr (uncurry insert) empty

toList :: IPRTable k a -> [(AddrRange k, a)]
toList = foldt f []
  where f Nil xs = xs
        f (Node _ _ Nothing _ _) xs = xs
        f (Node k _ (Just v) _ _) xs = (k,v):xs

foldt :: (IPRTable k a -> b -> b) -> b -> IPRTable k a -> b
foldt _ v Nil = v
foldt f v rt@(Node _ _ _ l r) = foldt f (foldt f (f rt v) l) r

insert :: (Address k, Bits k) => AddrRange k -> a -> IPRTable k a -> IPRTable k a
insert k v Nil = Node k (rangeToTestBit k) (Just v) Nil Nil
insert k v s@(Node k1 tb1 v1 left right) | k == k1 = Node k1 tb1 (Just v) left right
                                         | k1 >:> k && isLeft k tb1 = Node k1 tb1 v1 (insert k v left) right
                                         | k1 >:> k  = Node k1 tb1 v1 left (insert k v right)
                                         | otherwise = let n = Node k (rangeToTestBit k) (Just v) Nil Nil
                                                       in link n s

lookupAll :: (Address k, Bits k) => AddrRange k -> IPRTable k a -> [(AddrRange k, a)]
lookupAll range = go []
  where go acc Nil = acc
        go acc (Node k tb Nothing l r) | k == range = acc
                                       | k >:> range = go acc $ if isLeft range tb
                                                                  then l
                                                                  else r
                                       | otherwise = acc
        go acc (Node k tb (Just v) l r) | k == range = (k,v):acc
                                        | k >:> range = go ((k,v):acc) $ if isLeft range tb
                                                                           then l
                                                                           else r
                                        | otherwise = acc

lookup :: (Address k, Bits k) => AddrRange k -> IPRTable k a -> Maybe a
lookup r t = snd <$> listToMaybe (lookupAll r t)

delete :: (Address k, Bits k) => AddrRange k -> IPRTable k a -> IPRTable k a
delete _ Nil = Nil
delete k s@(Node k1 tb1 v1 l r) | k == k1 = node k1 tb1 Nothing l r
                                | k1 >:> k && isLeft k tb1 = node k1 tb1 v1 (delete k l) r
                                | k1 >:> k = node k1 tb1 v1 l (delete k r)
                                | otherwise = s
  where
    node _ _ Nothing Nil b = b
    node _ _ Nothing b Nil = b
    node k' tb v l' r' = Node k' tb v l' r'

-- | /O(n)/. Fold the keys and values in the IPRTable using the given
--   left-associative binary operator.
--   This function is equivalent to Data.Map.foldlWithKey with necessary to
--   IPRTable changes.
--   Since: 1.7.5
foldlWithKey :: (b -> AddrRange k -> a -> b) -> b -> IPRTable k a -> b
foldlWithKey f zr = go zr
  where
    go z Nil = z
    go z (Node _ _ Nothing l r) = go (go z l) r
    go z (Node n _ (Just v) l r) = go (f (go z l) n v) r
{-# INLINE foldlWithKey #-}

-- | /O(n)/. Fold the keys and values in the IPRTable using the given
--   right-associative binary operator.
--   This function is equivalent to Data.Map.foldrWithKey with necessary to
--   IPRTable changes.
--   Since: 1.7.5
foldrWithKey :: (AddrRange k -> a -> b -> b) -> b -> IPRTable k a -> b
foldrWithKey f zr = go zr
  where
    go z Nil = z
    go z (Node _ _ Nothing l r) = go (go z r) l
    go z (Node n _ (Just v) l r) = go (f n v (go z r)) l
{-# INLINE foldrWithKey #-}

rangeToTestBit :: Bits k => AddrRange k -> k
rangeToTestBit (AddrRange _ l) = bit $ fromIntegral l

isLeft :: (Address k, Bits k) => AddrRange k -> k -> Bool
isLeft (AddrRange a _)= isZero a

isZero :: Bits k => k -> k -> Bool
isZero a b = a .&. b == zeroBits

link :: (Address k, Bits k) => IPRTable k a -> IPRTable k a -> IPRTable k a
link s1@(Node k1 _ _ _ _) s2@(Node k2 _ _ _ _)
  | isLeft k1 tbg = Node kg tbg Nothing s1 s2
  | otherwise     = Node kg tbg Nothing s2 s1
  where
    kg = glue k1 k2
    tbg = rangeToTestBit kg
link _ _ = error "link"

-- | 'glue' find the common prefix between two address ranges
glue :: Address k => AddrRange k -> AddrRange k -> AddrRange k
glue (AddrRange ip1 _) (AddrRange ip2 _) = go 0
  where go n | ip1 `mask` n == ip2 `mask` n = go (n + 1)
             | otherwise = makeAddrRange ip1 (n - 1)
