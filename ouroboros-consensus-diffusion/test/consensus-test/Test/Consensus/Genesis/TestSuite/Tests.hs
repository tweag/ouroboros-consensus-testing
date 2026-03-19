{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Genesis.TestSuite.Tests (tests) where

import           Data.List.Extra (anySame)
import           Test.Consensus.Genesis.TestSuite
import qualified Test.Consensus.Genesis.TestSuite.All as All
import           Test.Consensus.Genesis.TestSuite.SmallKey
import qualified Test.Consensus.Genesis.TestSuite.SmallKey.Tests (tests)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "TestSuite"
  [ testCase "All test keys have distinct names" $
      assertBool "Test key names must be unique" $
        not . anySame $ fmap toKey $ getAllKeys @All.TestKey
  ,  Test.Consensus.Genesis.TestSuite.SmallKey.Tests.tests
  ]
