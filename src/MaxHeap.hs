module MaxHeap where

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Ord, Show)

instance Foldable Tree where
   foldMap f Empty = mempty
   foldMap f (Branch a l r) = foldMap f l `mappend` f a `mappend` foldMap f r

-- instance Show a => Show (Tree a) where
--     show Empty          = "-"
--     show (Branch l a r) = "(" ++ show l ++ " " ++ show a ++ " " ++ show r ++ ")"

extractNumber :: Tree a -> [a]
extractNumber Empty          = []
extractNumber (Branch a _ _) = [a]

hasHeapProperty :: (Ord a) => Tree a -> Bool
hasHeapProperty Empty          = True
hasHeapProperty (Branch i l r) = all (\a -> i >= a) ((extractNumber l) ++ (extractNumber r)) && (hasHeapProperty l) && (hasHeapProperty r)
