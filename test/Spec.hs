{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T

import qualified TestDay06
import qualified TestDay07

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [
    TestDay06.tests
  , TestDay07.tests
  ]
