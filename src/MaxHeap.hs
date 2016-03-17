module MaxHeap where

data Tree a = Empty | Branch (Tree a) a (Tree a)
          -- deriving (Show, Eq)

extractNumber :: Tree a -> [a]
extractNumber Empty          = []
extractNumber (Branch _ a _) = [a]

hasHeapProperty :: (Ord a) => Tree a -> Bool
hasHeapProperty Empty          = True
hasHeapProperty (Branch l i r) = all (\a -> i >= a) ((extractNumber l) ++ (extractNumber r)) && (hasHeapProperty l) && (hasHeapProperty r)
