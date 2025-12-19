{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.BlockTree.Tests {- (tests) -} where

import           Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Proxy
import qualified Data.Set as S
import           Ouroboros.Consensus.Block.Abstract (HasHeader, HeaderHash)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockHash)
import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup.GenChains (GenesisTest(..), genChains)
import           Test.QuickCheck
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.TestBlock (TestBlock)
import           Test.Util.TestEnv


genTestBlockTree :: QC.Gen Word -> QC.Gen (BlockTree TestBlock)
genTestBlockTree = fmap gtBlockTree . genChains

genTestAnchoredFragment :: QC.Gen (AF.AnchoredFragment TestBlock)
genTestAnchoredFragment = fmap btTrunk $ genTestBlockTree (pure 0)

tests :: TestTree
tests =
  let branchFactor = pure 4 in
  adjustQuickCheckTests (* 100) $
  adjustOption (\(QuickCheckMaxSize n) -> QuickCheckMaxSize (n `div` 10)) $
  testGroup "BlockTree"
    [ testGroup "nonemptyPrefixesOf"
      [ testProperty "nonemptyPrefixesArePrefixes" $ forAll genTestAnchoredFragment $
          prop_nonemptyPrefixesOf_nonemptyPrefixesArePrefixes
      , testProperty "nonemptyPrefixesAreNonempty" $ forAll genTestAnchoredFragment $
          prop_nonemptyPrefixesOf_nonemptyPrefixesAreNonempty
      , testProperty "nonemptyPrefixesAreUnique" $ forAll genTestAnchoredFragment $
          prop_nonemptyPrefixesOf_nonemptyPrefixesAreUnique
      , testProperty "allShareInputAnchor" $ forAll genTestAnchoredFragment $
          prop_nonemptyPrefixesOf_allShareInputAnchor
      ]
    , testGroup "deforestBlockTree"
      [ testProperty "headPointsAreDistinct" $ forAll (genTestBlockTree branchFactor) $
          prop_deforestBlockTree_headPointsAreDistinct
      , testProperty "imagesAreNonempty" $ forAll (genTestBlockTree branchFactor) $
          prop_deforestBlockTree_imagesAreNonempty
      , testProperty "allShareTrunkAnchor" $ forAll (genTestBlockTree branchFactor) $
          prop_deforestBlockTree_allShareTrunkAnchor
      , testProperty "fullBranchesAreBranches" $ forAll (genTestBlockTree branchFactor) $
          prop_deforestBlockTree_fullBranchesAreBranches
      , testProperty "everyHeaderHashIsInTheMap" $ forAll (genTestBlockTree branchFactor) $
          prop_deforestBlockTree_everyHeaderHashIsInTheMap
      , testProperty "prefixMaximalPrefixesAreBranches" $ forAll (genTestBlockTree branchFactor) $
          prop_deforestBlockTree_prefixMaximalPrefixesAreBranches
      ]
    ]



-- | The nonempty prefixes of an `AF.AnchoredFragment` are in fact prefixes.
prop_nonemptyPrefixesOf_nonemptyPrefixesArePrefixes
  :: (Eq blk, HasHeader blk) => AF.AnchoredFragment blk -> QC.Property
prop_nonemptyPrefixesOf_nonemptyPrefixesArePrefixes fragment =
  QC.property . all (flip AF.isPrefixOf fragment) . nonemptyPrefixesOf $ fragment

-- | The nonempty prefixes of an `AF.AnchoredFragment` are in fact nonempty.
prop_nonemptyPrefixesOf_nonemptyPrefixesAreNonempty
  :: (Eq blk, HasHeader blk) => AF.AnchoredFragment blk -> QC.Property
prop_nonemptyPrefixesOf_nonemptyPrefixesAreNonempty fragment =
  QC.property . all (not . AF.null) . nonemptyPrefixesOf $ fragment

-- | The nonempty prefixes of an `AF.AnchoredFragment` are unique.
prop_nonemptyPrefixesOf_nonemptyPrefixesAreUnique
  :: forall blk. (Eq blk, HasHeader blk) => AF.AnchoredFragment blk -> QC.Property
prop_nonemptyPrefixesOf_nonemptyPrefixesAreUnique fragment =
  let
    isNew :: forall u. (Eq u) => u -> ([u], Bool) -> ([u], Bool)
    isNew u (seen, restOk) =
      if not (elem u seen) && restOk
        then (u:seen, True)
        else (seen, False)
  in QC.property . snd . foldr isNew ([], True) . nonemptyPrefixesOf $ fragment

-- | All the nonempty prefixes should share the original fragment's anchor.
prop_nonemptyPrefixesOf_allShareInputAnchor
  :: (HasHeader blk) => AF.AnchoredFragment blk -> QC.Property
prop_nonemptyPrefixesOf_allShareInputAnchor originalFragment =
  let sharesTrunkAnchor thisFragment restOk =
        (((==) `on` AF.anchor) thisFragment originalFragment) && restOk
  in QC.property . foldr sharesTrunkAnchor True . nonemptyPrefixesOf $ originalFragment



-- | The head points of all the branches are distinct.
-- (Points uniquely determine positions in the tree.)
prop_deforestBlockTree_headPointsAreDistinct
  :: (HasHeader blk) => BlockTree blk -> QC.Property
prop_deforestBlockTree_headPointsAreDistinct =
  let headPointsDistinct thisChain (restOk, seenSoFar) =
        let hP = AF.headPoint thisChain
        in ((not $ elem hP seenSoFar) && restOk, hP : seenSoFar)
  in QC.property . fst . foldr headPointsDistinct (True, mempty) . deforestBlockTree

-- | The deforested branches are all populated.
prop_deforestBlockTree_imagesAreNonempty
  :: (HasHeader blk) => BlockTree blk -> QC.Property
prop_deforestBlockTree_imagesAreNonempty =
  let chainsNotEmpty thisChain restOk = not (AF.null thisChain) && restOk
  in QC.property . foldr chainsNotEmpty True . deforestBlockTree

-- | All the deforested branches share the trunk's anchor.
prop_deforestBlockTree_allShareTrunkAnchor
  :: (HasHeader blk) => BlockTree blk -> QC.Property
prop_deforestBlockTree_allShareTrunkAnchor tree =
  let sharesTrunkAnchor thisChain restOk =
        (((==) `on` AF.anchor) thisChain (btTrunk tree)) && restOk
  in QC.property . foldr sharesTrunkAnchor True . deforestBlockTree $ tree

-- | Full branches are in the deforestation.
prop_deforestBlockTree_fullBranchesAreBranches
  :: (Eq blk, HasHeader blk) => BlockTree blk -> QC.Property
prop_deforestBlockTree_fullBranchesAreBranches tree =
  let inDeforestation thisChain restOk =
        (elem thisChain (deforestBlockTree tree)) && restOk
  in QC.property . foldr inDeforestation True $ fmap btbFull $ btBranches tree

-- | Every block header from the `BlockTree` is in the deforestation map.
prop_deforestBlockTree_everyHeaderHashIsInTheMap
  :: forall blk. (Eq blk, HasHeader blk) => BlockTree blk -> QC.Property
prop_deforestBlockTree_everyHeaderHashIsInTheMap tree@(BlockTree trunk branches) =
  let
    allBranchHeaderHashes :: BlockTreeBranch blk -> [HeaderHash blk]
    allBranchHeaderHashes (BlockTreeBranch prefix suffix trunk full) =
      fmap blockHash $ concatMap AF.toOldestFirst [ prefix, suffix, trunk, full ]

    allHeaderHashes :: [HeaderHash blk]
    allHeaderHashes = fmap blockHash (AF.toOldestFirst trunk) <>
      concatMap allBranchHeaderHashes branches

  in QC.property $ all (flip M.member $ deforestBlockTree tree) allHeaderHashes

-- | An `AF.AnchoredFragment` is /prefix maximal/ in a list if it is not a nontrivial
-- prefix of another fragment in the list. After deforesting a tree, the maximal
-- prefixes in the result are precisely the trunk and branches of the tree in
-- some order.
prop_deforestBlockTree_prefixMaximalPrefixesAreBranches
  :: forall blk. (Eq blk, HasHeader blk) => BlockTree blk -> QC.Property
prop_deforestBlockTree_prefixMaximalPrefixesAreBranches tree@(BlockTree trunk branches) =
  QC.property $ isPermutation
    (foldr (insertIfMaximalBy AF.isPrefixOf) [] $ deforestBlockTree tree)
    (trunk : fmap btbFull branches)

-- | If u is smaller than any of the elements of xs, return xs.
-- Otherwise, remove any elements of xs smaller than u and append
-- u to the remainder on the right.
insertIfMaximalBy :: forall u. (u -> u -> Bool) -> u -> [u] -> [u]
insertIfMaximalBy cmp u =
  let
    go xs = case xs of
      [] -> [u]
      x:rest -> if cmp x u
        then go rest
        else x : if cmp u x
          then rest else go rest
  in go

-- | Detect if one list is a permutation of another; duplicates allowed.
-- (Remember that `L.delete` only removes the first occurrence.)
--
-- Examples:
--   isPermutation [1,2,3] [1,2,3]   == True
--   isPermutation [1,2,3] [1,3,2]   == True
--   isPermutation [1,2,3] [1,2,4]   == False
--   isPermutation [1,2,3] [1,2,3,3] == False
--   isPermutation []      []        == True  -- fun fact, this is why 0! = 1
isPermutation :: (Eq u) => [u] -> [u] -> Bool
isPermutation xs ys = ((&&) `on` null)
  (foldr L.delete ys xs) -- find any ys not in xs
  (foldr L.delete xs ys) -- find any xs not in ys
