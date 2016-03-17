module MaxHeap where

data Tree a = Empty | Branch (Tree a) a (Tree a)
          -- deriving (Show, Eq)

extractNumber :: Tree a -> [a]
extractNumber Empty          = []
extractNumber (Branch _ a _) = [a]

extractNumbers :: Tree a -> [a]
extractNumbers Empty          = []
extractNumbers (Branch l _ r) = (extractNumber l) ++ (extractNumber r)
