module Main (main) where

import Test.Tasty
import Test.Tasty.Hspec

import UnionSpecs

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs [unionSpecs]
  defaultMain $ testGroup "All Tests" [testGroup "Specs" specs]
