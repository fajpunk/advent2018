{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T

import qualified TestDay06

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [
    TestDay06.tests
  ]
