module TreeCreatorSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TreeCreator
import Numeric (readInt)
import Data.Char (digitToInt)

main :: IO ()
main = hspec spec

getRandomPath :: Tree a -> Gen [Direction]
getRandomPath Empty          = return []
getRandomPath (Branch a l r) = do d <- elements [L, R]
                                  case d of
                                    L -> case l of
                                      Empty -> return []
                                      _     -> fmap ([L] ++) $ getRandomPath l
                                    R -> case r of
                                      Empty -> return []
                                      _     -> fmap ([R] ++) $ getRandomPath r

toBin :: [Direction] -> String
toBin []     = ""
toBin (L:ds) = "0" ++ (toBin ds)
toBin (R:ds) = "1" ++ (toBin ds)

toIndex :: [Direction] -> Int
toIndex ds = fst . head $ toDec $ "1" ++ toBin ds
  where toDec = readInt 2 (`elem` "01") digitToInt

toIndicies :: [Direction] -> [([Direction], Int)]
toIndicies []  = []
toIndicies [d] = [([d], toIndex [d])]
toIndicies ds  = toIndicies (init ds) ++ [(ds, toIndex ds)]

elemAt :: [Direction] -> Tree a -> a
elemAt []     (Branch x _ _) = x
elemAt (L:ds) (Branch _ l _) = elemAt ds l
elemAt (R:ds) (Branch _ _ r) = elemAt ds r

prop_treeCreator =
  -- for a random list of ints
  forAll (arbitrary :: Gen [Int]) $ \xs ->
  -- create a tree from the list
  -- for a random path in the tree
  let tree = TreeCreator.perform xs in forAll (getRandomPath $ tree) $ \ds ->
  -- print more info when failing
  whenFail (print (xs, ds, tree)) $
  -- convert selected nodes to their list index
  -- assert they match their respective elements in the original list
  all id $ (\(ds, i) -> xs !! (i - 1) == (elemAt ds tree)) <$> toIndicies ds


spec :: Spec
spec = do
  describe "TreeCreator.perform" $ do
    it "works" $ prop_treeCreator
