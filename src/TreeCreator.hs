module TreeCreator where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

perform :: [a] -> Tree a
perform xs = toTree (zip xs [1..]) Empty

toTree :: [(a, Int)] -> Tree a -> Tree a
toTree []          t =                                       t
toTree ((x, i):xs) t = toTree xs $ insert x (toDirections i) t

data Direction = L | R deriving (Show)

toDirections :: Int -> [Direction]
toDirections i = map toDirection $ tail $ toBinary i
  where
    toDirection b = if b == '1' then R else L
    toBinary    i = showIntAtBase 2 intToDigit i ""

insert :: a -> [Direction] -> Tree a -> Tree a
insert a []     Empty          = Branch a Empty Empty
insert a (L:ds) (Branch x l r) = Branch x (insert a ds l) r
insert a (R:ds) (Branch x l r) = Branch x l (insert a ds r)

-- 01 - 0001 - []
-- 02 - 0010 - [L]
-- 03 - 0011 - [R]
-- 04 - 0100 - [L, L]
-- 05 - 0101 - [L, R]
-- 06 - 0110 - [R, L]
-- 07 - 0111 - [R, R]
-- 08 - 1000 - [L, L, L]
-- 09 - 1001 - [L, L, R]
-- 10 - 1010 - [L, R, L]
-- 11 - 1011 - [L, R, R]
-- 12 - 1100 - [R, L, L]
-- 13 - 1101 - [R, L, R]
-- 14 - 1110 - [R, R, L]
-- 15 - 1111 - [R, R, R]
