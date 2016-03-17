module MaxHeapSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import MaxHeap

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "extractNumbers" $ do
    context "full trees" $ do
      it "returns the values from the immediate descendants" $ do
        extractNumbers (Branch (Branch Empty 2 Empty) 1 (Branch Empty 3 Empty)) `shouldBe` [2,3]

      it "even if there are more distant descendants" $ do
        extractNumbers (Branch (Branch (Branch Empty 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch (Branch Empty 6 Empty) 3 (Branch Empty 7 Empty))) `shouldBe` [2,3]

      it "unless there are no descendants" $ do
        extractNumbers (Branch Empty 1 Empty) `shouldBe` []

    context "partial trees" $ do
      it "returns what it can" $ do
        extractNumbers (Branch (Branch Empty 2 Empty) 1 Empty) `shouldBe` [2]
