{-# LANGUAGE OverloadedStrings #-}
module TestDay06 where

import           System.IO.Unsafe

import           Test.Tasty
import           Test.Tasty.Hspec
import qualified Data.Text                     as T

import           Day06                          ( Point(..)
                                                , Box(..)
                                                , parsePoint
                                                , boundingBox
                                                , borderPoints
                                                )

tests = unsafePerformIO $ testSpec "Day 6 tests" $ do
  describe "parsePoint" $ do
    it "parses a point" $ do
      let expected = Point 84 212
      let actual   = parsePoint "84, 212"
      actual `shouldBe` expected

  describe "boundingBox" $ do
    it "produces a bounding box with only 2 points" $ do
      let points = [Point 3 4, Point 1 2]
      let box    = boundingBox points
      box `shouldBe` Box (Point 1 2) (Point 3 4)

    it "produces a bounding box with more points" $ do
      let points =
            [ Point (-1) 18
            , Point 10   18
            , Point 10   4
            , Point (-3) 25
            , Point 3    4
            , Point 1    2
            ]
      let box = boundingBox points
      box `shouldBe` Box (Point (-3) 2) (Point 10 25)

  describe "borderPoints" $ do
    it "contains all of the border points" $ do
      let box = Box (Point 1 1) (Point 4 4)
      let expected =
            [ Point 1 1
            , Point 2 1
            , Point 3 1
            , Point 4 1
            , Point 1 4
            , Point 2 4
            , Point 3 4
            , Point 4 4
            , Point 1 2
            , Point 1 3
            , Point 4 2
            , Point 4 3
            ]
      (borderPoints box) `shouldBe` expected
