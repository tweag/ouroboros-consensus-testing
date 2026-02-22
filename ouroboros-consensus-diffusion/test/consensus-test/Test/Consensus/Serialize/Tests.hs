{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Consensus.Serialize.Tests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo (..), HasHeader, SlotNo (..),
                     StandardHash)
import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup.GenChains (GenesisTest (..),
                     IssueTestBlock (..), genChains)
import           Test.Consensus.Genesis.ShrinkIndex
import qualified Test.Consensus.PointSchedule as Schedule
import           Test.Consensus.Serialize
import qualified Test.QuickCheck as QC
import           Test.Tasty (TestTree, localOption, testGroup)
import           Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import           Test.Util.TestBlock (TestBlock, Validity (..),
                     testHashFromList, unsafeTestBlockWithPayload)



-- TODO: Currently using `()` as a dummy key type; this should be replaced
-- with a proper key type.
tests :: TestTree
tests = testGroup "JSON Serialization"
  [ testGroup "serialize . deserialize . serialize == serialize"
    [ testProperty "ReifiedTestCase () BlockRep  <===>  JSON" $
      QC.forAll (genReifiedTestCase (pure 1))
        (prop_serialize_weak_inverse (Proxy @(ReifiedTestCase () BlockRep)))
    ]
  , testGroup "deserialize . serialize == id"
    [ testProperty "ReifiedTestCase () BlockRep  <===>  JSON" $
      QC.forAll (genReifiedTestCase (pure 1))
        (prop_serialize_inverse (Proxy @(ReifiedTestCase () BlockRep)))
    ]
  , testGroup "fromReifiedBlockTree . toReifiedBlockTree == id"
    [ testProperty "BlockTree TestBlock  <===>  ReifiedBlockTree BlockRep" $
      QC.forAllShrink (genTestBlockTree (pure 1)) shrinkBlockTree
        (prop_reified_block_tree_conversion)
    ]
  , test_fromReifiedBlockTree_cases
  ]



-- Generators and Shrinkers --
------------------------------

genReifiedTestCase
  :: (QC.Arbitrary key)
  => QC.Gen Word -> QC.Gen (ReifiedTestCase key BlockRep)
genReifiedTestCase branchFactor = do
  (rtcBlockTree, rtcPointSchedule) <- genTestBlockTreeAndPointSchedule branchFactor
  rtcTestKey <- QC.arbitrary
  rtcTestVersion <- QC.arbitrary
  rtcShrinkIndex <- fmap (path . fmap QC.getNonNegative) QC.arbitrary
  rtcSeed <- fmap Seed QC.arbitrary
  pure ReifiedTestCase {..}

genTestBlockTreeAndPointSchedule
  :: QC.Gen Word -> QC.Gen (ReifiedBlockTree BlockRep, Schedule.PointSchedule (SlotNo, BlockNo))
genTestBlockTreeAndPointSchedule branchFactor = do
  -- Create a block tree with @1@ alternative chain.
  blockTree <- genTestBlockTree (pure 1)
  -- Create a 'longRangeAttack' schedule based on the generated chains.
  ps <- Schedule.stToGen (Schedule.longRangeAttack blockTree)
  reifiedBlockTree <- fmap toReifiedBlockTree $ genTestBlockTree branchFactor
  pure (reifiedBlockTree, toReifiedPointSchedule ps)

genTestBlockTree :: QC.Gen Word -> QC.Gen (BlockTree TestBlock)
genTestBlockTree = fmap gtBlockTree . genChains

shrinkBlockTree :: (HasHeader blk) => BlockTree blk -> [BlockTree blk]
shrinkBlockTree (BlockTree trunk branches) = mconcat
  [ -- Shrink the branches first, if any. This avoids
    -- removing trunk nodes to which branches are attached.
    case branches of
      [] -> []
      _:_ -> do
        -- Shrink the branch suffixes, and filter out any that become empty.
        -- If all branches were shorter than the trunk before, then they still are.
        branches' <- fmap (filter (shareAnchorButNoBlocksWith trunk) . filter (not . AF.null)) $
          QC.shrinkList shrinkAnchoredFragment $ fmap btbSuffix branches
        case fromTrunkAndBranches trunk branches' of
          Nothing -> []
          Just bt -> pure bt

  , -- Shrink the trunk, removing any branches whose anchors are removed.
    do
      trunk' <- shrinkAnchoredFragment trunk
      case fromTrunkAndBranches trunk' $ fmap btbSuffix branches of
        Nothing -> []
        Just bt ->
          let
              -- Filter out shrinks where the trunk is shorter than the longest branch.
              BlockTree shrunkTrunk shrunkBranches = bt
              trunkLength = AF.length shrunkTrunk
              maxBranchLength = maximum (0 : fmap (AF.length . btbFull) shrunkBranches)
            in case compare trunkLength maxBranchLength of
              GT -> pure bt
              _  -> []
  ]

shareAnchorButNoBlocksWith
  :: (HasHeader blk) => AF.AnchoredFragment blk -> AF.AnchoredFragment blk -> Bool
shareAnchorButNoBlocksWith fragment1 fragment2 =
  case AF.intersect fragment1 fragment2 of
    Nothing -> False
    Just (prefix1, prefix2, _, _) -> min (AF.length prefix1) (AF.length prefix2) > 0

-- | If the fragment is not empty, drop the most recent block.
shrinkAnchoredFragment
  :: (HasHeader blk) => AF.AnchoredFragment blk -> [AF.AnchoredFragment blk]
shrinkAnchoredFragment fragment = case AF.toNewestFirst fragment of
  []     -> []
  _:rest -> pure $ AF.fromNewestFirst (AF.anchor fragment) rest



-- Properties --
----------------

-- | deserialize . serialize == id
--
-- This property asserts that values survive after being serialized
-- and then deserialized.
prop_serialize_inverse
  :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a, Show a, Eq a)
  => Proxy a -> a -> QC.Property
prop_serialize_inverse _ value =
  let
    json1 = Aeson.toJSON value
    cannotParseMsg err = mconcat
      [ "Unable to parse JSON:\n"
      , "JSON: " , show json1 , "\n"
      , "Error: " , err
      ]
    valueNotStableMsg deserializedValue = mconcat
      [ "Value not stable after round-trip:\n"
      , "Original: " , show value , "\n"
      , "After:    " , show deserializedValue
      ]
  in case Aeson.parseEither Aeson.parseJSON json1 of
    Left err -> QC.counterexample (cannotParseMsg err) False
    Right value' -> case value == value' of
      False -> QC.counterexample (valueNotStableMsg value') False
      True  -> QC.property True

-- | serialize . deserialize . serialize == serialize
--
-- This property tests that if a JSON value was produced by serializing a reified
-- test case, then deserializing and serializing again gives the same JSON value.
-- This is weaker than saying that deserialize . serialize == id, but if that
-- test fails, whether or not this one passes can help with debugging.
prop_serialize_weak_inverse
  :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a)
  => Proxy a -> a -> QC.Property
prop_serialize_weak_inverse _ value =
  let
    json1 = Aeson.toJSON value
    cannotParseMsg err = mconcat
      [ "Unable to parse JSON:\n"
      , "JSON: " , show json1 , "\n"
      , "Error: " , err
      ]
    jsonNotStableMsg reserializedValue = mconcat
      [ "JSON not stable after round-trip:\n"
      , "Original: " , show json1 , "\n"
      , "After:    " , show reserializedValue
      ]
  in case Aeson.parseEither Aeson.parseJSON json1 of
    Left err -> QC.counterexample (cannotParseMsg err) False
    Right value' ->
      let json2 = Aeson.toJSON (value' :: a)
      in case json1 == json2 of
        False -> QC.counterexample (jsonNotStableMsg json2) False
        True  -> QC.property True

-- | fromReifiedBlockTree . toReifiedBlockTree == id
prop_reified_block_tree_conversion
  :: forall blk. (Show blk, Eq blk, HasHeader blk, IssueTestBlock blk)
  => BlockTree blk -> QC.Property
prop_reified_block_tree_conversion blockTree =
  let
    reified = toReifiedBlockTree blockTree
    cannotConvertMsg err = mconcat
      [ "Unable to convert from ReifiedBlockTree to BlockTree:\n"
      , "BlockTree: ", show blockTree, "\n"
      , "ReifiedBlockTree: ", show reified, "\n"
      , "Error: ", err
      ]
  in case fromReifiedBlockTree reified of
      Left err              -> QC.counterexample (cannotConvertMsg err) False
      Right (blockTree', _) -> eqBlockTree blockTree blockTree'



-- Test Cases for fromReifiedBlockTree --
-----------------------------------------

-- | Specific input output pairs for testing 'fromReifiedBlockTree'. These are
-- intended to demonstrate the expected semantics of slot number assignments.
test_fromReifiedBlockTree_cases :: TestTree
test_fromReifiedBlockTree_cases = localOption (QuickCheckTests 1) $
  testGroup "fromReifiedBlockTree cases"
  [ test_fromReifiedBlockTree_case
    "Empty trunk, no branches"
    ( AnchoredFork Nothing mempty 0
    , []
    , AF.fromOldestFirst AF.AnchorGenesis mempty
    , []
    )
  , test_fromReifiedBlockTree_case
    "Trunk with 1 block, no branches"
    ( AnchoredFork Nothing [BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}] 0
    , [] :: [AnchoredFork BlockRep]
    , AF.fromOldestFirst AF.AnchorGenesis
      [makeTestBlock [0] 0 Valid]
    , []
    )
  , test_fromReifiedBlockTree_case
    "Trunk with 2 blocks, no branches"
    ( AnchoredFork Nothing
      [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 2}
      ] 0
    , [] :: [AnchoredFork BlockRep]
    , AF.fromOldestFirst AF.AnchorGenesis
      [ makeTestBlock [0] 0 Valid
      , makeTestBlock [0,0] 1 Valid
      ]
    , []
    )
  , test_fromReifiedBlockTree_case
    "Trunk with 3 blocks, no branches"
    ( AnchoredFork Nothing
      [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 2}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 3}
      ] 0
    , [] :: [AnchoredFork BlockRep]
    , AF.fromOldestFirst AF.AnchorGenesis
      [ makeTestBlock [0] 0 Valid
      , makeTestBlock [0,0] 1 Valid
      , makeTestBlock [0,0,0] 2 Valid
      ]
    , []
    )
  , test_fromReifiedBlockTree_case
    "Trunk with 2 blocks, 1 branch at genesis"
    ( AnchoredFork Nothing
      [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 2}
      ] 0
    , [ AnchoredFork Nothing
        [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 3}
        ] 1
      ]
    , AF.fromOldestFirst AF.AnchorGenesis
      [ makeTestBlock [0] 0 Valid
      , makeTestBlock [0,0] 1 Valid
      ]
    , [ AF.fromOldestFirst AF.AnchorGenesis
        [makeTestBlock [1] 0 Valid]
      ]
    )
  , test_fromReifiedBlockTree_case
    "Trunk with 3 blocks, 1 branch from block"
    ( AnchoredFork Nothing
      [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 2}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 3}
      ] 0
    , [ AnchoredFork (Just (SlotNo 0, BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}))
        [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 3}
        ] 1
      ]
    , AF.fromOldestFirst AF.AnchorGenesis
      [ unsafeTestBlockWithPayload (testHashFromList [0]) (SlotNo 0) Valid ()
      , unsafeTestBlockWithPayload (testHashFromList [0,0]) (SlotNo 1) Valid ()
      , unsafeTestBlockWithPayload (testHashFromList [0,0,0]) (SlotNo 2) Valid ()
      ]
    , [ AF.fromOldestFirst (AF.Anchor (SlotNo 0) (testHashFromList [0]) (BlockNo 1))
        [ unsafeTestBlockWithPayload (testHashFromList [0,1]) (SlotNo 1) Valid ()
        ]
      ]
    )
  , test_fromReifiedBlockTree_case
    "Trunk with 3 blocks, 2 branches from genesis and block"
    ( AnchoredFork Nothing
      [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 2}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 3}
      ] 0
    , [ AnchoredFork Nothing
        [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 4}
        ] 1
      , AnchoredFork (Just (SlotNo 0, BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}))
        [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 5}
        ] 2
      ]
    , AF.fromOldestFirst AF.AnchorGenesis
      [ makeTestBlock [0] 0 Valid
      , makeTestBlock [0,0] 1 Valid
      , makeTestBlock [0,0,0] 2 Valid
      ]
    , [ AF.fromOldestFirst AF.AnchorGenesis
        [ makeTestBlock [1] 0 Valid
        ]
      , AF.fromOldestFirst (AF.Anchor (SlotNo 0) (testHashFromList [0]) (BlockNo 1))
        [ makeTestBlock [0,2] 1 Valid
        ]
      ]
    )
  , test_fromReifiedBlockTree_case
    "Trunk with 3 blocks, 2 branches from genesis and block (reverse branch order)"
    ( AnchoredFork Nothing
      [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 2}
      , BlockRep {brSlotGap = 0, brBlockNo = BlockNo 3}
      ] 0
    , [ AnchoredFork (Just (SlotNo 0, BlockRep {brSlotGap = 0, brBlockNo = BlockNo 1}))
        [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 5}
        ] 2
      , AnchoredFork Nothing
        [ BlockRep {brSlotGap = 0, brBlockNo = BlockNo 4}
        ] 1
      ]
    , AF.fromOldestFirst AF.AnchorGenesis
      [ makeTestBlock [0] 0 Valid
      , makeTestBlock [0,0] 1 Valid
      , makeTestBlock [0,0,0] 2 Valid
      ]
    , [ AF.fromOldestFirst (AF.Anchor (SlotNo 0) (testHashFromList [0]) (BlockNo 1))
        [ makeTestBlock [0,2] 1 Valid
        ]
      , AF.fromOldestFirst AF.AnchorGenesis
        [ makeTestBlock [1] 0 Valid
        ]
      ]
    )
  ]

-- | Helper for manually construcing a 'TestBlock' with a given hash, slot number,
-- and validity.
makeTestBlock :: [Word64] -> Word64 -> Validity -> TestBlock
makeTestBlock hash slot validity =
  unsafeTestBlockWithPayload (testHashFromList hash) (SlotNo slot) validity ()

-- | Convert a given reified block tree (as a trunk + branches) to a @BlockTree@,
-- and assert that the result is an expected value.
test_fromReifiedBlockTree_case
  :: String
  -> ( AnchoredFork BlockRep
     , [AnchoredFork BlockRep]
     , AF.AnchoredFragment TestBlock
     , [AF.AnchoredFragment TestBlock]
     )
  -> TestTree
test_fromReifiedBlockTree_case title parts =
  testProperty title $
    let
      (rTrunk, rBranches, bTrunk, bBranches) = parts
      actual = fmap fst $ fromReifiedBlockTree $ ReifiedBlockTree rTrunk rBranches
      expect = fromTrunkAndBranches bTrunk bBranches
      cannotConvertMsg err = mconcat
        [ "Failed to convert from ReifiedBlockTree to BlockTree:\n"
        , "Reified trunk: ", show rTrunk, "\n"
        , "Reified branches: ", show rBranches, "\n"
        , "Expected: ", show expect, "\n"
        , "Error: ", err
        ]
    in case (expect, actual) of
        (Just expectedBlockTree, Right actualBlockTree) ->
          eqBlockTree actualBlockTree expectedBlockTree
        (Nothing, _) -> QC.counterexample "Expected block tree is invalid" False
        (_, Left err) -> QC.counterexample (cannotConvertMsg err) False

-- Are two block trees equal?
eqBlockTree
  :: (StandardHash blk, Show blk, Eq blk)
  => BlockTree blk -- ^ Actual value
  -> BlockTree blk -- ^ Expected value
  -> QC.Property
eqBlockTree (BlockTree trunk1 branches1) (BlockTree trunk2 branches2) =
  QC.conjoin
    [ let
        msg = mconcat
          [ "Expected equal block trees, but the trunks do not match:\n"
          , "Expected: ", show trunk2, "\n"
          , "Actual: ", show trunk1, "\n"
          ]
      in QC.counterexample msg $ QC.property (trunk1 == trunk2)
    , let
        msg = mconcat
          [ "Expected equal block trees, but the branches do not match:\n"
          , "Expected: ", show branches2, "\n"
          , "Actual: ", show branches1, "\n"
          ]
      in QC.counterexample msg $ QC.property (branches1 == branches2)
    ]
