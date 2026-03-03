{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Indexing the shrinking tree
module Test.Consensus.Genesis.ShrinkIndex (
    ShrinkIndex
  , ShrinkTree
  , arbitraryShrinkIndexWithin
  , arbitraryShrinkTree
  , child
  , lookup
  , makeShrinkTree
  , narrowShrinkTree
  , next
  , parent
  , path
  , stretch
  , succ
  ) where

import           Control.Comonad (Comonad (..))
import           Control.Monad ((>=>))
import qualified Data.Aeson as Aeson
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Maybe (isJust, listToMaybe)
import           Data.Sequence (Seq (..), fromList)
import qualified Data.Text as T
import           Prelude hiding (lookup, succ)
import           Test.QuickCheck (Arbitrary (..), Gen, Testable (property),
                     frequency, listOf, suchThat)
import           Test.QuickCheck.Checkers (EqProp (..), eq)
import           Text.Read (readMaybe)

-- | Each 'ShrinkIndex' represents a unique path along a 'ShrinkTree'. Its monoidal
-- operation corresponds to concatenating one path onto the other, and its
-- neutral (empty) element corresponds to the current 'ShrinkTree' top node
-- (representing a test counterexample). See [NOTE: shrink-index-properties]
newtype ShrinkIndex = Ix {getIndex :: Seq Int} deriving (Eq, Ord, Semigroup, Monoid)

instance Show ShrinkIndex where
  show (Ix s) = "path " <> show (toList s)

instance Arbitrary ShrinkIndex where
  arbitrary = fmap mconcat $ listOf $
    frequency [(4, child <$> arbitrary), (1, pure mempty)]

  shrink (Ix s) = Ix <$> shrink s

instance Aeson.ToJSON ShrinkIndex where
  toJSON (Ix s) = Aeson.toJSON $ fmap show (toList s)

instance Aeson.FromJSON ShrinkIndex where
  parseJSON = Aeson.withArray "ShrinkIndex" $ \arr -> do
    let parseElement = Aeson.withText "ShrinkIndex element" $ \txt ->
          case readMaybe (T.unpack txt) of
            Just v  -> pure v
            Nothing -> fail $ "Invalid ShrinkIndex element: " ++ T.unpack txt
    Ix . fromList <$> mapM parseElement (toList arr)

data ShrinkTree a = Node a [ShrinkTree a] deriving stock (Functor, Foldable, Traversable)

instance Comonad ShrinkTree where
  extract = node
  extend f tree@(Node _ bs) = Node (f tree) (fmap (extend f) bs)

instance Arbitrary a => Arbitrary (ShrinkTree a) where
  arbitrary = fmap arbitraryShrinkTree arbitrary

  -- Note that a 'ShrinkTree' build by @arbitraryShrinkTree@
  -- shrinks to its own child branches i.e.
  -- @shrink tree == branches tree@
  shrink = fmap arbitraryShrinkTree . shrink . extract

-- | Build a path out of an integer list.
path :: [Int] -> ShrinkIndex
path = foldMap child

-- | The top node of a 'ShrinkTree'.
node :: ShrinkTree a -> a
node (Node x _) = x

-- | Child branches of a 'ShrinkTree'.
branches :: ShrinkTree a -> [ShrinkTree a]
branches (Node _ bs) = bs

-- | Unfold a 'ShrinkTree' using the given shirking function.
makeShrinkTree :: (a -> [a]) -> a -> ShrinkTree a
makeShrinkTree f x = Node x $ fmap (makeShrinkTree f) $ f x

-- | Unfold a 'ShrinkTree' by recursively shrinking a value.
arbitraryShrinkTree :: Arbitrary a => a -> ShrinkTree a
arbitraryShrinkTree = makeShrinkTree shrink

-- | Generates an arbitrary 'ShrinkIndex' within the given 'ShrinkTree'.
arbitraryShrinkIndexWithin :: ShrinkTree a -> Gen ShrinkIndex
arbitraryShrinkIndexWithin tree =
  suchThat arbitrary (isJust . flip lookup tree)

-- | Find the value that a 'ShrinkIndex' points to. It returns the
-- root note of the tree when passsed the empty index.
lookup :: ShrinkIndex -> ShrinkTree a -> Maybe a
lookup ix tree = fmap extract $ runKleisli (narrowShrinkTree ix) tree

-- | A 'ShrinkTree' traversal by the given index's path. It is a monoid
-- homomorphism; this property is fundamental for the specification of 'lookup'.
-- See [NOTE: shrink-index-properties]
narrowShrinkTree :: ShrinkIndex -> Kleisli Maybe (ShrinkTree a) (ShrinkTree a)
narrowShrinkTree = foldMap (\n -> Kleisli (listToMaybe . drop n . branches)) . getIndex

-- | A local definition of 'Control.Arrow.Kleisli' to provide a non-orphan
-- 'Monoid' instance. With this, the algebraic structure of the tree path
-- traversal by `narrowShrinkTree` is made explicit.
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Semigroup (Kleisli m a a) where
  Kleisli f <> Kleisli g = Kleisli $ f >=> g

instance Monad m => Monoid (Kleisli m a a) where
  mempty = Kleisli pure

-- | The testing notion of 'ShrinkTree' path equality is given by the observation
-- of the current (top) node.
instance (Arbitrary a, Eq b) => EqProp (Kleisli Maybe (ShrinkTree a) (ShrinkTree b)) where
    Kleisli f =-= Kleisli g = property $ do
      x <- arbitrary
      pure $ on eq (fmap extract) (f x) (g x)

-- | Confines an index transformation /within/ a 'ShrinkTree'
withinTree :: (ShrinkIndex -> ShrinkIndex) -> ShrinkTree a -> ShrinkIndex -> Maybe ShrinkIndex
withinTree f tree ix = f ix <$ lookup (f ix) tree

-- | Stretch the 'ShrinkIndex' into the first child 'ShrinkTree'.
stretch :: ShrinkTree a -> ShrinkIndex -> Maybe ShrinkIndex
stretch = withinTree (<> child 0)

-- | Move the 'ShrinkIndex' tip to the next sibling 'ShrinkTree' branch.
succ :: ShrinkTree a -> ShrinkIndex -> Maybe ShrinkIndex
succ = withinTree next

-- | The immediate nth child node index.
child :: Int -> ShrinkIndex
child n = Ix $ fromList [n]

-- | The index of the next sibling node, or 'mempty' if the index contains no calls to 'child'.
next :: ShrinkIndex -> ShrinkIndex
next (Ix Empty)      = mempty
next (Ix (xs :|> x)) = Ix (xs :|> (x + 1))

-- | The index of the parent node.
parent :: ShrinkIndex -> Maybe ShrinkIndex
parent (Ix Empty)      = Nothing
parent (Ix (xs :|> _)) = Just $ Ix xs
