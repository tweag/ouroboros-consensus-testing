{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- NOTE: This tests defer the type errors that would other wise prevent
-- the following 'allKeys' instances from being called (or defined).
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Test.Consensus.Genesis.TestSuite.SmallKey.Tests (tests) where

import           Control.DeepSeq (NFData)
import           Data.List (permutations)
import           GHC.Generics
import           Test.Consensus.Genesis.TestSuite.SmallKey
import           Test.ShouldNotTypecheck (shouldNotTypecheck)
import           Test.Tasty
import           Test.Tasty.HUnit

data SimpleInfiniteType = Nil | More SimpleInfiniteType
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NFData
  deriving SmallKey via Generically SimpleInfiniteType

data UnitSumType = L () | R ()
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically UnitSumType

data UnitProductType = P () ()
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NFData
  deriving SmallKey via Generically UnitProductType

data IntUnaryType = U Int
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NFData
  deriving SmallKey via Generically IntUnaryType

tests :: TestTree
tests = testGroup "SmallKey"
  [ testCase "A minimal sum type instance" $
      assertBool "allKeys must be a permutation of the list of all values" $
        elem (allKeys @UnitSumType) $ permutations [L (), R ()]
  , testCase "A minimal product type does not typecheck" $
      shouldNotTypecheck $ allKeys @UnitProductType
  , testCase "A minimal unary type with black-listed Int argument does not typecheck" $
      shouldNotTypecheck $ allKeys @IntUnaryType
  , testCase "A simple recursive infinite type does not typecheck" $
      shouldNotTypecheck $ allKeys @SimpleInfiniteType
  ]
