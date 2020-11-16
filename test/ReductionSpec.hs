module ReductionSpec where

import Reduction (dirReduce)
import Reduction.Direction
import Control.Monad
import Test.Hspec
import Test.QuickCheck

import Test.Hspec
import Reduction

spec :: Spec
spec = do
  describe "simple tests" $ do
    it "basic tests" $ do
      0 `shouldBe` 0
  describe "dirReduce - simple tests" $ do
    it "should work for some small examples" $ do
      dirReduce [     ]             `shouldBe` [     ]
      dirReduce [North]             `shouldBe` [North]
      dirReduce [North, West]       `shouldBe` [North,West]      
      dirReduce [North, West, East] `shouldBe` [North]
      dirReduce [North, West, South, East] `shouldBe` [North, West, South, East]
      dirReduce [North, South, South, East, West, North, West] `shouldBe` [West]
      dirReduce [North, South, South, East, West, North]       `shouldBe` []
      
    it "should return a single direction unchanged" $ do
      property $ forAll (elements [North, West, East, South]) $ \x ->
        dirReduce [x] `shouldBe` [x]
        
    it "should return a random list of north-west unchanged" $ do
      property $ forAll (listOf $ elements [North, West]) $ \xs ->
        dirReduce xs `shouldBe` xs
        
  describe "running there and back again" $ do
    forM_ [(North, South), (South, North), (West, East), (East, West)] $ \(a,b) ->
      it ("should return the right amount of " ++ show a) $ 
        property $ \t y -> t > 0 && y > 0 ==>
          let x = t + y
          in dirReduce (replicate x a ++ replicate y b) `shouldBe` replicate (x - y) a
