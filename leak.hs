{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Data.List
import System.Random
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as M

-- This is an exercise in finding memory leaks.
-- If it is compiled with "-rtsopts -O1", it can run with
-- a small heap and stack, by adding these arguments:
-- "+RTS -M1M -K2k".  It seems like the main trick is to add
-- enough seq statements everywhere, and to not lose faith!

-- The stack incantation is: stack build --ghc-options="-rtsopts -O1"

-- There was an important clue here:
-- https://mail.haskell.org/pipermail/beginners/2014-August/014027.html

-- This program generates ten million random ints and
-- keeps a count of the end digits by accumulating the ten
-- totals in an IntMap, which requires the following three
-- ingredients:
--   Random numbers
--   State monad
--   IntMap

main :: IO ()
main = do
  --print $ s 0 $ map (*2) [1 .. 10 * 1000 * 1000]
  --print $ t (0,0) [1..10000]
  --print $ m [1..10000]
  --print $ k (B 0) [1..100000]
  --print $ evalState (sumOfRnd (2000 * 1000) 0) $ mkStdGen 1
  print $ evalState (histOfRnd (1000 * 1000) M.empty) $ mkStdGen 1 
  --print $ evalState (treeOfRnd (1000 * 1000) Empty) $ mkStdGen 1 
  --print $ hist M.empty [1..1000000]
  --print $ histTree Empty [1..100 * 1000 * 1000] 

s :: Int -> [Int] -> Int 
s tot [] = tot
--s !tot (x:xs) = s (tot + x) xs
s tot (x:xs) = tot `seq` s (tot + x) xs
--s tot (x:xs) = s (tot + x) xs
--s tot (x:xs) = s tot xs

hist :: M.IntMap Int -> [Int] -> M.IntMap Int
hist ans [] = ans
hist ans (x:xs) =
  --let d = x `mod` 10 :: Int
  --    ans' = d `seq` M.insertWith (+++) d 1 ans
  let ans' = M.insert 0 1 ans
  in hist ans' xs

(+++) :: Int -> Int -> Int
x +++ y = x `seq` y `seq` x + y

t :: (Int,Int) -> [Int] -> (Int,Int)
t tot [] = tot
--t (!a,!b) (x:xs) = t (a + x, b + x) xs
t (a,b) (x:xs) =
  a `seq` b `seq` t (a+x+a+x , b + x) xs

m :: [Int] -> (Int,Int)
m xs = foldl' step (0, 0) xs
{-
step (s, l) a = let s' = s + a
                    l' = l + 1
                in s' `seq` l' `seq` (s', l')
                --in (s', l')
-}
--step (!s, !l) a = (s + a, l + 1)

--step (s, l) a = seqpair (s + a, l + 1)
--seqpair (p,q) = p `seq` q `seq` (p,q)

--step (s, l) a = s `seq` l `seq` (s + a, l + 1)
--step (s, l) a = (s+l) `seq` (s + a, l + 1)
step (s, l) a = (s,l) `seq` (s + a, l + 1)

data B a = B a deriving Show

k :: B Int -> [Int] -> B Int
k tot [] = tot
--k !tot (x:xs) = k (tot + x) xs
k (B tot) (x : xs) = (tot+4) `seq` k (B (tot + x)) xs
--k tot (x:xs) = k (tot + x) xs

sumOfRnd :: Int -> Int -> State StdGen Int
sumOfRnd 0 ans = return ans
sumOfRnd n ans = do
  g <- get
  let (a::Int, g') = random g
  put g'
  ans `seq` sumOfRnd (n-1) (ans+a)

histOfRnd :: Int -> M.IntMap Int -> State StdGen (M.IntMap Int)
histOfRnd 0 ans = return ans
histOfRnd n ans = do
  g <- get
  let (a, g') = random g
  put g'
  histOfRnd (n-1) $! M.insertWith (+) (mod a 10) 1 ans

treeOfRnd :: Int -> Tree Int Int -> State StdGen (Tree Int Int)
treeOfRnd 0 ans = return ans
treeOfRnd n ans = do
  g <- get
  let (a::Int, g') = random g
  put g'
  let d = a `mod` 10 :: Int
  let ans' = ans `seq` d `seq` treeInsertWith (+++) d 1 ans
  ans' `seq` treeOfRnd (n-1) ans'

u :: M.IntMap Int
u = M.fromList [(1,2)]

histTree :: Tree Int Int -> [Int] -> Tree Int Int
histTree ans [] = ans
histTree ans (x:xs) =
  let d = x `mod` 10 :: Int
      ans' = ans `seq` d `seq` treeInsertWith (+++) d 1 ans
  in histTree ans' xs


data Tree a b = Empty | MkTree !(Tree a b) a b !(Tree a b) deriving Show

treeInsert :: Ord a => a -> b -> Tree a b -> Tree a b
treeInsert e v Empty = MkTree Empty e v Empty
treeInsert e v t@(MkTree a x y b)
  | e > x     = MkTree a x y (treeInsert e v b)
  | e < x     = MkTree (treeInsert e v a) x y b
  | otherwise = MkTree a x v b

treeInsertWith :: Ord a => (b -> b -> b) -> a -> b -> Tree a b -> Tree a b
treeInsertWith _ e v Empty = MkTree Empty e v Empty
treeInsertWith f e v t@(MkTree a x y b)
  | e > x     = MkTree a x y (treeInsertWith f e v b)
  | e < x     = MkTree (treeInsertWith f e v a) x y b
  | otherwise = y `seq` v `seq` MkTree a x (f y v) b
