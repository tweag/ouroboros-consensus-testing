-- | Property tests are reified by the 'ConformanceTest' type in order to
-- expose them to the conformance testing harness.
-- Here, a 'TestSuite' arranges them is a rose tree like structure.
module Test.Consensus.Genesis.TestSuite (
    TestSuite
  , allTheTests
  , insert
  , toListWithKey
  ) where

import           Test.Consensus.Genesis.Setup (ConformanceTest)

-- | Sum type of all the existing genesis test properties
-- TODO: Populate this type with the ported tests.
data TestClass = TestClass deriving (Eq, Ord)

data TestSuite a = SingleTest TestClass a
                 | TestGroup [TestSuite a] deriving (Eq)

singleton :: TestClass -> a -> TestSuite a
singleton c t = SingleTest c t

insert :: TestClass -> a -> TestSuite a -> TestSuite a
insert c t ts@(SingleTest _ _) = TestGroup [singleton c t, ts]
insert c t (TestGroup tss)     = TestGroup (singleton c t : tss)

toListWithKey :: TestSuite a -> [(TestClass, a)]
toListWithKey (SingleTest c x) = [(c, x)]
toListWithKey (TestGroup tss)  = concatMap toListWithKey tss

allTheTests :: TestSuite (ConformanceTest blk)
allTheTests = undefined
