{-# LANGUAGE OverloadedStrings #-}
module TestDay07 where

import           System.IO.Unsafe

import           Test.Tasty
import           Test.Tasty.Hspec
import           Day07                          ( parseEdge )

tests = unsafePerformIO $ testSpec "Day 7 tests" $ do
  describe "parseEdge" $ do
    it "parses an edge" $ do
      let input = "Step A must be finished before step Z can begin."
      let expected = ('A', 'Z')
      parseEdge input `shouldBe` expected
