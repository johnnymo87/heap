module MaxHeapSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import MaxHeap
import Control.Monad (replicateM, liftM)

import Data.Tree hiding (Tree)

main :: IO ()
main = hspec spec

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = return Empty
arbTree n = Branch <$> arbitrary <*> subtree <*> subtree
  where subtree = do
          Positive m <- arbitrary
          let n' = n `div` (m + 1)
          arbTree n'

-- To test, cabal repl spec, import Test.QuickCheck, then ...
-- sample arb
arb = arbitrary :: Gen (MaxHeap.Tree Int)

toDataTree Empty          = Node (show ".") []
toDataTree (Branch a l r) = Node (show a) [toDataTree l, toDataTree r]

-- for pretty-printing fun
printTree = samples >>= mapM_ putStrLn
  where samples = sample' $ drawTree . toDataTree <$> arb

safeInit :: [a] -> [a]
safeInit [] = []
safeInit (x : xs) = [x]

-- sample $ arb >>= extractLine
extractLine :: Tree a -> Gen [a]
extractLine Empty          = return []
extractLine (Branch a l r) = joinLeaves (return [a]) (safeInit <$> shuffleLeaves l r)
  where
    shuffleLeaves x y = joinLeaves (extractLine x) (extractLine y) >>= shuffle
    joinLeaves    x y = (++) <$> x <*> y

-- descending order
ordered :: Ord a => [a] -> Bool
ordered xs = and (zipWith (>=) xs (drop 1 xs))

prop_hasHeapProperty =
  -- for all random trees
  forAll arb $ \t ->
  -- filter by heap property
  -- for all randomly chosen lines from trees
  hasHeapProperty t ==> forAll (extractLine t) $ \xs ->
  -- the are ordered
  ordered xs

spec :: Spec
spec = do
  describe "hasHeapProperty" $ do
    it "works" $ prop_hasHeapProperty
