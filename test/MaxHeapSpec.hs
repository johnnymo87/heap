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

-- To test, cabal repl spec, import Test.QuickCheck, then ...
-- sample (arbitrary :: Gen (MaxHeap.Tree Int))
arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = return Empty
arbTree n = do
              (Positive m) <- arbitrary
              let n' = n `div` (m + 1)
              (Positive m) <- arbitrary
              let n'' = n `div` (m + 1)
              l <- (arbTree n')
              a <- arbitrary
              r <- (arbTree n'')
              return $ Branch a l r

-- toDataTree :: Show a => MaxHeap.Tree a -> Data.Tree.Tree String
toDataTree Empty          = Node (show ".") []
toDataTree (Branch a l r) = Node (show a) [toDataTree l, toDataTree r]

printTree = fmap toDataTree (arbitrary :: Gen (MaxHeap.Tree Int))
printTree' = fmap drawTree printTree
printTree'' = sample printTree'

spec :: Spec
spec = do
  describe "extractNumber" $ do
    context "full trees" $ do
      it "returns the value from branch" $ do
        extractNumber (Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)) `shouldBe` [1]
  --
  --     it "unless it is not a branch" $ do
  --       extractNumber (Empty :: (Tree Integer)) `shouldBe` []
  --
  --   context "partial trees" $ do
  --     it "returns the value from the branch" $ do
  --       extractNumber (Branch (Branch Empty 2 Empty) 1 Empty) `shouldBe` [1]
  --
  -- describe "hasHeapProperty" $ do
  --   context "full trees" $ do
  --     context "when heap property is not fulfilled" $ do
  --       it "fails when immediate descendants are greater than the parent" $ do
  --         hasHeapProperty (Branch (Branch Empty 2 Empty) 1 (Branch Empty 3 Empty)) `shouldBe` False
  --
  --       it "fails when more distant descendants are greater than their parents" $ do
  --         hasHeapProperty (Branch (Branch (Branch Empty 4 Empty) 2 (Branch Empty 5 Empty)) 3 (Branch (Branch Empty 6 Empty) 1 (Branch Empty 7 Empty))) `shouldBe` False
  --
  --     context "when heap property is fulfilled" $ do
  --       it "succeeds when immediate descendants are less than the parent" $ do
  --         hasHeapProperty (Branch (Branch Empty 2 Empty) 3 (Branch Empty 1 Empty)) `shouldBe` True
  --
  --       it "succeeds when immediate descendants are equal to the parent" $ do
  --         hasHeapProperty (Branch (Branch Empty 3 Empty) 3 (Branch Empty 1 Empty)) `shouldBe` True
  --
  --       it "succeeds when more distant descendants are less than their parents" $ do
  --         hasHeapProperty (Branch (Branch (Branch Empty 4 Empty) 6 (Branch Empty 3 Empty)) 7 (Branch (Branch Empty 2 Empty) 5 (Branch Empty 1 Empty))) `shouldBe` True
  --
  --       it "succeeds when more distant descendants are equal to their parents" $ do
  --         hasHeapProperty (Branch (Branch (Branch Empty 6 Empty) 6 (Branch Empty 3 Empty)) 7 (Branch (Branch Empty 5 Empty) 5 (Branch Empty 1 Empty))) `shouldBe` True
  --
  --       it "succeeds when there are no descendants" $ do
  --         hasHeapProperty (Branch Empty 1 Empty) `shouldBe` True
  --
  --   context "partial trees" $ do
  --     context "when heap property is not fulfilled" $ do
  --       it "fails when the immediate descendant is greater than the parent" $ do
  --         hasHeapProperty (Branch (Branch Empty 2 Empty) 1 Empty) `shouldBe` False
  --
  --     context "when heap property is not fulfilled" $ do
  --       it "succeeds when the immediate descendant is less than the parent" $ do
  --         hasHeapProperty (Branch (Branch Empty 1 Empty) 2 Empty) `shouldBe` True
  --
  --       it "succeeds when the immediate descendant is equal to the parent" $ do
  --         hasHeapProperty (Branch (Branch Empty 1 Empty) 1 Empty) `shouldBe` True
