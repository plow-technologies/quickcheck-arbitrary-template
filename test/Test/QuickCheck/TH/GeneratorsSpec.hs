{-# LANGUAGE TemplateHaskell #-}
module Test.QuickCheck.TH.GeneratorsSpec (tests) where

import Test.QuickCheck.TH.Generators

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord



-- | These example types should build arbitrary instances correctly 

data ExampleSumTypes = ExampleSum0 
                    | ExampleSum1 Int
                    | ExampleSum2 Int Int
                    | ExampleSum3 Int Int Int
                    | ExampleSum4 Int Int Int Int
                    | ExampleSum5 Int Int Int Int Int
                    | ExampleSum6 Int Int Int Int Int Int
                    | ExampleSum7 Int Int Int Int Int Int Int
 deriving (Show,Ord,Eq)

makeArbitrary ''ExampleSumTypes


instance Arbitrary ExampleSumTypes where
  arbitrary = arbitraryExampleSumTypes

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" (
       \list -> sort (list :: [ExampleSumTypes]) == sort (reverse list)) ]

