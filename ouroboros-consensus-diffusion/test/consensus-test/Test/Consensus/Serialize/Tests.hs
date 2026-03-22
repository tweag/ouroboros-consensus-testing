{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Consensus.Serialize.Tests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as Aeson
import           Data.Foldable (toList)
import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Maybe (isJust)
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
    , testProperty "fromReifiedTestCase . toReifiedTestCase == id" $
      QC.forAll (genConcreteTestCase branchFactor)
        prop_fromReifiedTestCase_faithful_on_metadata
    , testProperty "toReifiedTestCase preserves key/version/shrinkIndex/seed" $
      QC.forAll (genConcreteTestCase branchFactor)
        prop_toReifiedTestCase_preserves_metadata
    , testProperty "serializeReifiedTestCase emits exactly expected top-level keys" $
      QC.forAll (genReifiedTestCase branchFactor)
        prop_serializeReifiedTestCase_emits_expected_keys
    , testProperty "deserializeReifiedTestCase rejects missing required fields" $
      QC.forAll (genReifiedTestCase branchFactor)
        prop_deserializeReifiedTestCase_rejects_missing_required_field
    , testProperty "deserializeReifiedTestCase rejects invalid required field types" $
      QC.forAll (genReifiedTestCase branchFactor)
        prop_deserializeReifiedTestCase_rejects_bad_field_type
    , testProperty "deserializeReifiedTestCase ignores unknown fields" $
      QC.forAll (genReifiedTestCase branchFactor)
        prop_deserializeReifiedTestCase_ignores_unknown_fields
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
    , testProperty "fromReifiedBlockTree populates KnownBlocks for every reified block id" $
      QC.forAllShrink (genTestBlockTree branchFactor) shrinkBlockTree
        prop_fromReifiedBlockTree_knownBlocks_complete
    , testProperty "fromReifiedBlockTree rejects dangling branch anchors" $
      QC.forAllShrink (genTestBlockTree branchFactor) shrinkBlockTree
        prop_fromReifiedBlockTree_rejects_dangling_anchor
    ]
  , testGroup "PointSchedule"
    [ testProperty "toReifiedPointSchedule . fromReifiedPointSchedule . toReifiedPointSchedule == toReifiedPointSchedule" $
      QC.forAll (genTestBlockTreeWithPointSchedule branchFactor) $
        \(blockTree, pointSchedule) ->
          prop_toReifiedPointSchedule_weak_inverse blockTree pointSchedule
    , testProperty "fromReifiedPointSchedule rejects unknown block ids" $
      QC.forAll (genTestBlockTreeWithPointSchedule branchFactor) $
        \(blockTree, pointSchedule) ->
          prop_fromReifiedPointSchedule_rejects_unknown_point blockTree pointSchedule
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

genConcreteTestCase
  :: QC.Gen Word
  -> QC.Gen ((), TestVersion, BlockTree TestBlock, Schedule.PointSchedule TestBlock, ShrinkIndex, Seed)
genConcreteTestCase branchFactor = do
  (blockTree, pointSchedule) <- genTestBlockTreeWithPointSchedule branchFactor
  testVersion <- QC.arbitrary
  shrinkIndex <- fmap (path . fmap QC.getNonNegative) QC.arbitrary
  seed <- fmap Seed QC.arbitrary
  pure ((), testVersion, blockTree, pointSchedule, shrinkIndex, seed)

genTestReifiedBlockTreeWithPointSchedule
  :: QC.Gen Word
  -> QC.Gen (ReifiedBlockTree BlockRep, Schedule.PointSchedule BlockId)
genTestReifiedBlockTreeWithPointSchedule branchFactor = do
  (blockTree, pointSchedule) <- genTestBlockTreeWithPointSchedule branchFactor
  let (reifiedTree, knownForks) = toReifiedBlockTree blockTree
  pure (reifiedTree, toReifiedPointSchedule knownForks pointSchedule)

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
prop_fromReifiedBlockTree_inverse blockTree = QC.ioProperty $ do
  ctx <- getTestBlockContext $ Proxy @blk
  let
    (reified, _) = toReifiedBlockTree blockTree
    cannotConvertMsg err = mconcat
      [ "Unable to convert from ReifiedBlockTree to BlockTree:\n"
      , "BlockTree: ", show blockTree, "\n"
      , "ReifiedBlockTree: ", show reified, "\n"
      , "Error: ", err
      ]
  pure $ case fromReifiedBlockTree ctx reified of
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
prop_toReifiedBlockTree_weak_inverse blockTree = QC.ioProperty $ do
  ctx <- getTestBlockContext $ Proxy @blk
  let
    (reified1, _) = toReifiedBlockTree blockTree
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
    fromReified1 = fromReifiedBlockTree ctx reified1
  pure $ case fromReified1 of
      Left err               -> QC.counterexample (cannotConvertMsg err) False
      Right (blockTree', _ ) ->
        let (reified2, _) = toReifiedBlockTree blockTree'
        in QC.counterexample (unstableMsg reified2) $ QC.property (reified1 == reified2)

-- | Every non-genesis branch anchor in a reified tree must refer to a trunk block id.
prop_anchor_correctness_invariants
  :: forall blk. (HasHeader blk)
  => BlockTree blk -> QC.Property
prop_anchor_correctness_invariants blockTree =
  let
    (reified@ReifiedBlockTree{rbtTrunk, rbtBranches}, _) = toReifiedBlockTree blockTree
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
prop_toReifiedPointSchedule_weak_inverse blockTree pointSchedule = QC.ioProperty $ do
  ctx <- getTestBlockContext $ Proxy @blk
  let
    (reifiedTree, knownForks) = toReifiedBlockTree blockTree
    reifiedSchedule1 = toReifiedPointSchedule knownForks pointSchedule
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
    fromReifiedTree = fromReifiedBlockTree ctx reifiedTree
  pure $ case fromReifiedTree of
      Left err -> QC.counterexample (cannotConvertTreeMsg err) False
      Right (_, knownBlocks) ->
        case fromReifiedPointSchedule knownBlocks reifiedSchedule1 of
          Left err -> QC.counterexample (cannotConvertScheduleMsg err) False
          Right pointSchedule' ->
            let (_, knownForks') = toReifiedBlockTree blockTree
                reifiedSchedule2 = toReifiedPointSchedule knownForks' pointSchedule'
            in QC.counterexample (notStableMsg reifiedSchedule2) $ QC.property (reifiedSchedule2 == reifiedSchedule1)

-- | The type of @fromReifiedTestCase@ takes a continuation that is used to construct
-- a concrete test case. Verify that the metadata passed to the continuation matches
-- that of the original test case.
prop_fromReifiedTestCase_faithful_on_metadata
  :: ((), TestVersion, BlockTree TestBlock, Schedule.PointSchedule TestBlock, ShrinkIndex, Seed)
  -> QC.Property
prop_fromReifiedTestCase_faithful_on_metadata (testKey, testVersion, blockTree, pointSchedule, shrinkIndex, seed) = QC.ioProperty $ do
  ctx <- getTestBlockContext $ Proxy @TestBlock
  let
    reified = toReifiedTestCase testKey testVersion blockTree pointSchedule shrinkIndex seed
    cannotConvertMsg err = mconcat
      [ "Unable to reconstruct concrete test case from reified representation:\n"
      , "Error: ", err, "\n"
      , "ReifiedTestCase: ", show reified
      ]
    fromReified = fromReifiedTestCase ctx (,,,,,) reified
  pure $ case fromReified of
      Left err -> QC.counterexample (cannotConvertMsg err) False
      Right (testKey', testVersion', blockTree', pointSchedule', shrinkIndex', seed') ->
        let
          (_, knownForks') = toReifiedBlockTree blockTree'
          reifiedSchedule' = toReifiedPointSchedule knownForks' pointSchedule'
        in QC.conjoin
          [ QC.counterexample "test key changed after to/from reified conversion" $
              QC.property (testKey == testKey')
          , QC.counterexample "test version changed after to/from reified conversion" $
              QC.property (testVersion == testVersion')
          , eqBlockTree blockTree' blockTree
          , QC.counterexample "reified point schedule changed after to/from reified conversion" $
            QC.property (reifiedSchedule' == rtcPointSchedule reified)
          , QC.counterexample "shrink index changed after to/from reified conversion" $
              QC.property (shrinkIndex == shrinkIndex')
          , QC.counterexample "seed changed after to/from reified conversion" $
              QC.property (seed == seed')
          ]

prop_toReifiedTestCase_preserves_metadata
  :: ((), TestVersion, BlockTree TestBlock, Schedule.PointSchedule TestBlock, ShrinkIndex, Seed)
  -> QC.Property
prop_toReifiedTestCase_preserves_metadata (testKey, testVersion, blockTree, pointSchedule, shrinkIndex, seed) =
  let
    reified = toReifiedTestCase testKey testVersion blockTree pointSchedule shrinkIndex seed
  in QC.conjoin
      [ QC.counterexample "rtcTestKey mismatch" $ QC.property (rtcTestKey reified == testKey)
      , QC.counterexample "rtcTestVersion mismatch" $ QC.property (rtcTestVersion reified == testVersion)
      , QC.counterexample "rtcShrinkIndex mismatch" $ QC.property (rtcShrinkIndex reified == shrinkIndex)
      , QC.counterexample "rtcSeed mismatch" $ QC.property (rtcSeed reified == seed)
      ]

-- | Serialized test cases have the expected keys.
prop_serializeReifiedTestCase_emits_expected_keys
  :: ReifiedTestCase () BlockRep -> QC.Property
prop_serializeReifiedTestCase_emits_expected_keys reified =
  case serializeReifiedTestCase FormatVersionOne reified of
    Aeson.Object obj ->
      let
        observed = Set.fromList (fmap AesonKey.toText $ AesonKeyMap.keys obj)
        expected = Set.fromList
          [ "formatVersion"
          , "key"
          , "testVersion"
          , "blockTree"
          , "pointSchedule"
          , "shrinkIndex"
          , "seed"
          ]
        msg = mconcat
          [ "Unexpected top-level JSON keys in serializeReifiedTestCase output.\n"
          , "Expected: ", show expected, "\n"
          , "Observed: ", show observed
          ]
      in QC.counterexample msg $ QC.property (observed == expected)
    value -> QC.counterexample ("Expected object JSON, got: " <> show value) False

-- | Deserializing a ReifiedTestCase with any required field missing should fail.
prop_deserializeReifiedTestCase_rejects_missing_required_field
  :: ReifiedTestCase () BlockRep -> QC.Property
prop_deserializeReifiedTestCase_rejects_missing_required_field reified =
  case serializeReifiedTestCase FormatVersionOne reified of
    Aeson.Object obj ->
      QC.conjoin $ fmap (fieldMustFailWhenMissing obj) requiredFields
    value -> QC.counterexample ("Expected object JSON, got: " <> show value) False
  where
    requiredFields :: [AesonKey.Key]
    requiredFields =
      [ "formatVersion"
      , "key"
      , "testVersion"
      , "blockTree"
      , "pointSchedule"
      , "shrinkIndex"
      , "seed"
      ]

    fieldMustFailWhenMissing
      :: Aeson.Object
      -> AesonKey.Key
      -> QC.Property
    fieldMustFailWhenMissing obj field =
      let mutated = Aeson.Object $ AesonKeyMap.delete field obj
      in case parseReifiedTestCaseValue mutated of
          Left _ -> QC.property True
          Right parsed -> QC.counterexample
            (mconcat
              [ "Expected parsing to fail when field is missing: "
              , show (AesonKey.toText field)
              , "\nParsed value: "
              , show parsed
              ])
            False

-- | Deserializing a ReifiedTestCase with an invalid type
-- for a required field should fail.
prop_deserializeReifiedTestCase_rejects_bad_field_type
  :: ReifiedTestCase () BlockRep -> QC.Property
prop_deserializeReifiedTestCase_rejects_bad_field_type reified =
  case serializeReifiedTestCase FormatVersionOne reified of
    Aeson.Object obj ->
      QC.conjoin $ fmap (mutationMustFail obj)
        [ ("formatVersion", Aeson.Bool True)
        , ("testVersion", Aeson.String "0.0")
        , ("blockTree", Aeson.String "not-an-object")
        , ("pointSchedule", Aeson.String "not-an-object")
        , ("shrinkIndex", Aeson.Bool True)
        , ("seed", Aeson.Number 0)
        ]
    value -> QC.counterexample ("Expected object JSON, got: " <> show value) False
  where
    mutationMustFail
      :: Aeson.Object
      -> (AesonKey.Key, Aeson.Value)
      -> QC.Property
    mutationMustFail obj (field, badValue) =
      let mutated = Aeson.Object $ AesonKeyMap.insert field badValue obj
      in case parseReifiedTestCaseValue mutated of
          Left _ -> QC.property True
          Right parsed -> QC.counterexample
            (mconcat
              [ "Expected parsing to fail for invalid field type.\n"
              , "Field: ", show (AesonKey.toText field), "\n"
              , "Bad value: ", show badValue, "\n"
              , "Parsed value: ", show parsed
              ])
            False

-- | Adding unknown JSON fields should not cause parsing to fail, and the
-- unknown fields should be ignored.
prop_deserializeReifiedTestCase_ignores_unknown_fields
  :: ReifiedTestCase () BlockRep -> QC.Property
prop_deserializeReifiedTestCase_ignores_unknown_fields reified =
  case serializeReifiedTestCase FormatVersionOne reified of
    Aeson.Object obj ->
      let
        mutated = Aeson.Object $ AesonKeyMap.insert "_unknownField" (Aeson.String "extra") obj
      in case parseReifiedTestCaseValue mutated of
          Left err -> QC.counterexample
            ("Expected parse success with unknown field, got error: " <> err)
            False
          Right parsed -> QC.counterexample
            (mconcat
              [ "Unknown fields should be ignored during parsing.\n"
              , "Expected: ", show reified, "\n"
              , "Parsed: ", show parsed
              ])
            (QC.property (parsed == reified))
    value -> QC.counterexample ("Expected object JSON, got: " <> show value) False

-- | The KnownBlocks map produced by fromReifiedBlockTree should contain entries
-- for every block ID present in the block tree.
prop_fromReifiedBlockTree_knownBlocks_complete
  :: forall blk. (HasHeader blk, IssueTestBlock blk, Show blk)
  => BlockTree blk -> QC.Property
prop_fromReifiedBlockTree_knownBlocks_complete blockTree = QC.ioProperty $ do
  ctx <- getTestBlockContext $ Proxy @blk
  let
    (reified, _) = toReifiedBlockTree blockTree
    expectedIds = Set.fromList $
      concatMap forkBlockIds (rbtTrunk reified : rbtBranches reified)
    cannotConvertMsg err = mconcat
      [ "Unable to decode ReifiedBlockTree:\n"
      , "ReifiedBlockTree: ", show reified, "\n"
      , "Error: ", err
      ]
    fromReified :: Either String (BlockTree blk, KnownBlocks blk)
    fromReified = fromReifiedBlockTree ctx reified
  pure $ case fromReified of
      Left err -> QC.counterexample (cannotConvertMsg err) False
      Right (_, knownBlocks) ->
        let
          observedIds = Set.fromList (M.keys (unKnownBlocks knownBlocks))
          missing = Set.toList (expectedIds Set.\\ observedIds)
          msg = mconcat
            [ "KnownBlocks is missing ids after reconstruction.\n"
            , "Missing ids: ", show missing, "\n"
            , "Expected ids: ", show (Set.toList expectedIds), "\n"
            , "Observed ids: ", show (Set.toList observedIds)
            ]
        in QC.counterexample msg $ QC.property (Set.null (expectedIds Set.\\ observedIds))

-- | A dangling anchor refers to an anchor node that does not exist in the trunk.
-- fromReifiedBlockTree should reject forks with dangling anchors.
prop_fromReifiedBlockTree_rejects_dangling_anchor
  :: forall blk. (HasHeader blk, IssueTestBlock blk, Show blk)
  => BlockTree blk -> QC.Property
prop_fromReifiedBlockTree_rejects_dangling_anchor blockTree = QC.ioProperty $ do
  ctx <- getTestBlockContext $ Proxy @blk
  let
    (reified, _) = toReifiedBlockTree blockTree
    mMutated = mutateDanglingAnchor reified
  pure $ case mMutated of
      Nothing -> QC.property True
      Just mutated ->
        let
          msgSuccess = mconcat
            [ "Expected fromReifiedBlockTree to reject dangling branch anchor, but it succeeded.\n"
            , "Original reified tree: ", show reified, "\n"
            , "Mutated reified tree: ", show mutated
            ]
        in case fromReifiedBlockTree ctx mutated :: Either String (BlockTree blk, KnownBlocks blk) of
            Left _  -> QC.property True
            Right _ -> QC.counterexample msgSuccess False

-- | Reified point schedules should only contain points that are present
-- in the block tree.
prop_fromReifiedPointSchedule_rejects_unknown_point
  :: forall blk. (HasHeader blk, IssueTestBlock blk, Show blk)
  => BlockTree blk -> Schedule.PointSchedule blk -> QC.Property
prop_fromReifiedPointSchedule_rejects_unknown_point blockTree pointSchedule = QC.ioProperty $ do
  ctx <- getTestBlockContext $ Proxy @blk
  let
    (reifiedTree, knownForks) = toReifiedBlockTree blockTree
    reifiedSchedule = toReifiedPointSchedule knownForks pointSchedule
    hasSchedulePoints = not (null (toList reifiedSchedule))
    cannotConvertTreeMsg err = mconcat
      [ "Unable to decode reified block tree for point schedule test:\n"
      , "ReifiedBlockTree: ", show reifiedTree, "\n"
      , "Error: ", err
      ]
    fromReifiedTree :: Either String (BlockTree blk, KnownBlocks blk)
    fromReifiedTree = fromReifiedBlockTree ctx reifiedTree
  pure $ case fromReifiedTree of
      Left err -> QC.counterexample (cannotConvertTreeMsg err) False
      Right (_, knownBlocks) ->
        let
          badPoint = freshBlockId knownBlocks
          mutatedSchedule = fmap (const badPoint) reifiedSchedule
          msgSuccess = mconcat
            [ "Expected fromReifiedPointSchedule to fail on unknown block ids, but it succeeded.\n"
            , "Bad block id used: ", show badPoint, "\n"
            , "Mutated schedule: ", show mutatedSchedule
            ]
        in hasSchedulePoints QC.==>
            case fromReifiedPointSchedule knownBlocks mutatedSchedule of
              Left _  -> QC.property True
              Right _ -> QC.counterexample msgSuccess False

parseReifiedTestCaseValue :: Aeson.Value -> Either String (ReifiedTestCase () BlockRep)
parseReifiedTestCaseValue = Aeson.parseEither deserializeReifiedTestCase

-- | Invent a block id that is not present in the given KnownBlocks map.
freshBlockId :: KnownBlocks blk -> BlockId
freshBlockId knownBlocks =
  let
    ids = Set.fromList (M.keys (unKnownBlocks knownBlocks))
    maxSlotNo = maximum (SlotNo 0 : fmap bidSlotNo (Set.toList ids))
    maxBlockNo = maximum (BlockNo 0 : fmap bidBlockNo (Set.toList ids))
    maxForkNo = maximum (ForkNo 0 : fmap bidForkNo (Set.toList ids))
    SlotNo s = maxSlotNo
    BlockNo b = maxBlockNo
    ForkNo f = maxForkNo
    candidate = BlockId (SlotNo (s + 1)) (BlockNo (b + 1)) (ForkNo (f + 1))
  in if Set.member candidate ids
      then BlockId (SlotNo (s + 2)) (BlockNo (b + 2)) (ForkNo (f + 2))
      else candidate

-- | Mutate one branch anchor to refer to a slot and block number that are not
-- present in the tree.
mutateDanglingAnchor
  :: ReifiedBlockTree BlockRep
  -> Maybe (ReifiedBlockTree BlockRep)
mutateDanglingAnchor reified@ReifiedBlockTree{rbtBranches} =
  let hasAnchor = isJust . forkAnchor
  -- Find the first branch with an anchor and mutate it.
  in case break hasAnchor rbtBranches of
    (_, []) -> Nothing
    (prefix, branch:suffix) ->
      case forkAnchor branch of
        Nothing -> Nothing
        Just (slotNo, rep) ->
          let
            SlotNo s = slotNo
            BlockNo n = brBlockNo rep
            badSlotNo = SlotNo (s + 1000000)
            badRep = rep { brBlockNo = BlockNo (n + 1000000) }
            mutatedBranch = branch { forkAnchor = Just (badSlotNo, badRep) }
          in Just reified { rbtBranches = prefix ++ (mutatedBranch : suffix) }

-- | Compute the set of block identifiers (slot and block number) for a fork.
forkBlockIds :: AnchoredFork BlockRep -> [BlockId]
forkBlockIds AnchoredFork{forkAnchor, forkBlocks, forkNumber} =
  let
    initialSlotNo = maybe (SlotNo 0) fst forkAnchor
    step (currentSlotNo, acc) BlockRep{brSlotGap, brBlockNo} =
      let nextSlotNo = currentSlotNo + fromIntegral brSlotGap
      in (nextSlotNo, BlockId nextSlotNo brBlockNo forkNumber : acc)
  in reverse $ snd $ foldl' step (initialSlotNo, []) forkBlocks
