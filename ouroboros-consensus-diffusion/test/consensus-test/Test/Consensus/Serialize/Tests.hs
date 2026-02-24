{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Consensus.Serialize.Tests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.List (foldl')
import           Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo (..), HasHeader, SlotNo (..),
                     StandardHash)
import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup.GenChains (GenesisTest (..),
                     IssueTestBlock (..), genChains)
import           Test.Consensus.Genesis.ShrinkIndex
import           Test.Consensus.Genesis.Tests.CSJ (genDuplicatedHonestSchedule)
import           Test.Consensus.Genesis.Tests.Uniform
                     (genBlockFetchLeashingSchedule, genLeashingSchedule,
                     genTimeLimitedSchedule, genUniformSchedulePoints)
import qualified Test.Consensus.PointSchedule as Schedule
import           Test.Consensus.Serialize
import qualified Test.QuickCheck as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.TestBlock (TestBlock)



-- TODO: Currently using `()` as a dummy key type; this should be replaced
-- with a proper key type.
tests :: TestTree
tests = testGroup "JSON Serialization"
  [ testGroup "ReifiedTestCase () BlockRep"
    [ testProperty "toJSON . fromJSON . toJSON == toJSON" $
      QC.forAll (genReifiedTestCase branchFactor)
        (prop_serialize_weak_inverse (Proxy @(ReifiedTestCase () BlockRep)))
    , testProperty "fromJSON . toJSON == id" $
      QC.forAll (genReifiedTestCase branchFactor)
        (prop_deserialize_inverse (Proxy @(ReifiedTestCase () BlockRep)))
    ]
  , testGroup "ReifiedBlockTree"
    [ testProperty "fromReifiedBlockTree . toReifiedBlockTree == id" $
      QC.forAllShrink (genTestBlockTree branchFactor) shrinkBlockTree
        prop_fromReifiedBlockTree_inverse
    , testProperty "toReifiedBlockTree . fromReifiedBlockTree . toReifiedBlockTree == toReifiedBlockTree" $
      QC.forAllShrink (genTestBlockTree branchFactor) shrinkBlockTree
        prop_toReifiedBlockTree_weak_inverse
    , testProperty "branch anchors always resolve to trunk ids" $
      QC.forAllShrink (genTestBlockTree branchFactor) shrinkBlockTree
        prop_anchor_correctness_invariants
    ]
  , testGroup "PointSchedule"
    [ testProperty "toReifiedPointSchedule . fromReifiedPointSchedule . toReifiedPointSchedule == toReifiedPointSchedule" $
      QC.forAll (genTestBlockTreeWithPointSchedule branchFactor) $
        \(blockTree, pointSchedule) ->
          prop_toReifiedPointSchedule_weak_inverse blockTree pointSchedule
    ]
  ]
  where
    branchFactor = pure 5



-- Generators and Shrinkers --
------------------------------

genReifiedTestCase
  :: (QC.Arbitrary key)
  => QC.Gen Word -> QC.Gen (ReifiedTestCase key BlockRep)
genReifiedTestCase branchFactor = do
  (rtcBlockTree, rtcPointSchedule) <- genTestReifiedBlockTreeWithPointSchedule branchFactor
  rtcTestKey <- QC.arbitrary
  rtcTestVersion <- QC.arbitrary
  rtcShrinkIndex <- fmap (path . fmap QC.getNonNegative) QC.arbitrary
  rtcSeed <- fmap Seed QC.arbitrary
  pure ReifiedTestCase {..}

genTestReifiedBlockTreeWithPointSchedule
  :: QC.Gen Word
  -> QC.Gen (ReifiedBlockTree BlockRep, Schedule.PointSchedule (SlotNo, BlockNo))
genTestReifiedBlockTreeWithPointSchedule branchFactor = do
  (blockTree, pointSchedule) <- genTestBlockTreeWithPointSchedule branchFactor
  pure (toReifiedBlockTree blockTree, toReifiedPointSchedule pointSchedule)

genTestBlockTree :: QC.Gen Word -> QC.Gen (BlockTree TestBlock)
genTestBlockTree = fmap gtBlockTree . genChains

genTestBlockTreeWithPointSchedule
  :: QC.Gen Word
  -> QC.Gen (BlockTree TestBlock, Schedule.PointSchedule TestBlock)
genTestBlockTreeWithPointSchedule branchFactor = do
  genesisTest <- genChains branchFactor
  schedule <- QC.oneof $ fmap ($ genesisTest)
    [ genDuplicatedHonestSchedule
    , genLeashingSchedule
    , genTimeLimitedSchedule
    , genUniformSchedulePoints
    , genBlockFetchLeashingSchedule
    ]
  pure (gtBlockTree genesisTest, schedule)

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

-- | fromJSON . toJSON == id
--
-- This property asserts that values survive after being serialized
-- and then deserialized.
prop_deserialize_inverse
  :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a, Show a, Eq a)
  => Proxy a -> a -> QC.Property
prop_deserialize_inverse _ value =
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

-- | toJSON . fromJSON . toJSON == toJSON
--
-- This property tests that if a JSON value was produced by serializing a reified
-- test case, then deserializing and serializing again gives the same JSON value.
-- This is weaker than saying that fromJSON . toJSON == id, but if that
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
prop_fromReifiedBlockTree_inverse
  :: forall blk. (Show blk, Eq blk, HasHeader blk, IssueTestBlock blk)
  => BlockTree blk -> QC.Property
prop_fromReifiedBlockTree_inverse blockTree =
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

-- | toReifiedBlockTree . fromReifiedBlockTree . toReifiedBlockTree == toReifiedBlockTree
prop_toReifiedBlockTree_weak_inverse
  :: forall blk. (Show blk, HasHeader blk, IssueTestBlock blk)
  => BlockTree blk -> QC.Property
prop_toReifiedBlockTree_weak_inverse blockTree =
  let
    reified1 = toReifiedBlockTree blockTree
    cannotConvertMsg err = mconcat
      [ "Unable to decode initial reified tree:\n"
      , "Initial ReifiedBlockTree: ", show reified1, "\n"
      , "Error: ", err
      ]
    unstableMsg reified2 = mconcat
      [ "Reified tree not stable after round-trip:\n"
      , "Initial: ", show reified1, "\n"
      , "After:   ", show reified2
      ]
    fromReified1 :: Either String (BlockTree blk, KnownBlocks blk)
    fromReified1 = fromReifiedBlockTree reified1
  in case fromReified1 of
      Left err               -> QC.counterexample (cannotConvertMsg err) False
      Right (blockTree', _ ) ->
        let reified2 = toReifiedBlockTree blockTree'
        in QC.counterexample (unstableMsg reified2) $ QC.property (reified1 == reified2)

-- | Every non-genesis branch anchor in a reified tree must refer to a trunk block id.
prop_anchor_correctness_invariants
  :: forall blk. (HasHeader blk)
  => BlockTree blk -> QC.Property
prop_anchor_correctness_invariants blockTree =
  let
    reified@ReifiedBlockTree{rbtTrunk, rbtBranches} = toReifiedBlockTree blockTree
    trunkIds = Set.fromList (forkBlockIds rbtTrunk)
    missingAnchors = flip map (zip [0..] rbtBranches) $ \(ix :: Int, branch) -> do
      (slotNo, rep) <- forkAnchor branch
      let anchorId = BlockId slotNo (brBlockNo rep) (ForkNo 0)
      case Set.member anchorId trunkIds of
        True  -> Nothing
        False -> Just (ix, anchorId)
    failures = [ x | Just x <- missingAnchors ]
    msg = mconcat
      [ "Found branch anchors not present in trunk block ids:\n"
      , "Missing anchors: ", show failures, "\n"
      , "Trunk ids: ", show (Set.toList trunkIds), "\n"
      , "ReifiedBlockTree: ", show reified
      ]
  in QC.counterexample msg $ QC.property (null failures)

-- | toReifiedPointSchedule . fromReifiedPointSchedule . toReifiedPointSchedule == toReifiedPointSchedule
prop_toReifiedPointSchedule_weak_inverse
  :: forall blk.
     (Show blk, HasHeader blk, IssueTestBlock blk)
  => BlockTree blk -> Schedule.PointSchedule blk -> QC.Property
prop_toReifiedPointSchedule_weak_inverse blockTree pointSchedule =
  let
    reifiedTree = toReifiedBlockTree blockTree
    reifiedSchedule1 = toReifiedPointSchedule pointSchedule
    cannotConvertTreeMsg err = mconcat
      [ "Unable to decode reified block tree for point schedule test:\n"
      , "BlockTree: ", show blockTree, "\n"
      , "ReifiedBlockTree: ", show reifiedTree, "\n"
      , "Error: ", err
      ]
    cannotConvertScheduleMsg err = mconcat
      [ "Unable to decode reified point schedule:\n"
      , "PointSchedule: ", show pointSchedule, "\n"
      , "ReifiedPointSchedule: ", show reifiedSchedule1, "\n"
      , "Error: ", err
      ]
    notStableMsg reifiedSchedule2 = mconcat
      [ "Reified point schedule not stable after weak round-trip:\n"
      , "Original: ", show reifiedSchedule1, "\n"
      , "After:    ", show reifiedSchedule2
      ]
    fromReifiedTree :: Either String (BlockTree blk, KnownBlocks blk)
    fromReifiedTree = fromReifiedBlockTree reifiedTree
  in case fromReifiedTree of
      Left err -> QC.counterexample (cannotConvertTreeMsg err) False
      Right (_, knownBlocks) ->
        case fromReifiedPointSchedule knownBlocks reifiedSchedule1 of
          Left err -> QC.counterexample (cannotConvertScheduleMsg err) False
          Right pointSchedule' ->
            let reifiedSchedule2 = toReifiedPointSchedule pointSchedule'
            in QC.counterexample (notStableMsg reifiedSchedule2) $ QC.property (reifiedSchedule2 == reifiedSchedule1)

-- | Compute the set of block identifiers (slot and block number) for a fork.
forkBlockIds :: AnchoredFork BlockRep -> [BlockId]
forkBlockIds AnchoredFork{forkAnchor, forkBlocks, forkNumber} =
  let
    initialSlotNo = maybe (SlotNo 0) fst forkAnchor
    step (currentSlotNo, acc) BlockRep{brSlotGap, brBlockNo} =
      let nextSlotNo = currentSlotNo + fromIntegral brSlotGap
      in (nextSlotNo, BlockId nextSlotNo brBlockNo forkNumber : acc)
  in reverse $ snd $ foldl' step (initialSlotNo, []) forkBlocks
