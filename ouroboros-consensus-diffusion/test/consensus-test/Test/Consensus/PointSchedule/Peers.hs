{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Simulating the protocol requires modeling the /peers/ in the network. This
-- module defines peer related types and helpers to manipulate them. Peers are
-- represented opaquely by a 'PeerId'.

module Test.Consensus.PointSchedule.Peers (
    -- * 'Peer'
    Peer (..)
  , PeerId (..)
  , enumerateAdversaries
  , isAdversarialPeerId
  , isHonestPeerId
    -- * 'Peers'
  , Peers (..)
    -- ** Constructors
  , peersOnlyAdversary
  , peersOnlyHonest
    -- ** Combinators
  , unionWithKey
    -- ** Queries
  , adversarialPeers'
  , adversarialPeers''
  , getPeer
  , getPeerIds
  , honestPeers'
  , honestPeers''
    -- ** Modifiers
  , deletePeer
  , updatePeer
    -- ** Conversions
  , fromMap
  , fromMap'
  , peers'
  , peersFromPeerIdList
  , peersFromPeerIdList'
  , peersFromPeerList
  , peersList
  , toMap
  , toMap'
  ) where

import           Control.Monad ((>=>))
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Hashable (Hashable)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString (fromString))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Util.Condense (Condense (..),
                     CondenseList (..), PaddingDirection (..),
                     condenseListWithPadding)
import qualified Test.QuickCheck as QC

-- | Identifier used to index maps and specify which peer is active during a tick.
data PeerId
  = HonestPeer Int
  | AdversarialPeer Int
  deriving (Eq, Generic, Show, Ord, NoThunks)

instance IsString PeerId where
  fromString s = case words s of
    ["honest"]       -> HonestPeer 1
    ["honest", n]    -> HonestPeer (read n)
    ["adversary"]    -> AdversarialPeer 1
    ["adversary", n] -> AdversarialPeer (read n)
    _                -> error $ "fromString: invalid PeerId: " ++ s

instance Condense PeerId where
  condense = \case
    HonestPeer 1 -> "honest"
    HonestPeer n -> "honest " ++ show n
    AdversarialPeer 1 -> "adversary"
    AdversarialPeer n -> "adversary " ++ show n

instance CondenseList PeerId where
  condenseList = condenseListWithPadding PadRight

instance Hashable PeerId

instance Aeson.ToJSON PeerId where
  toJSON peerId = Aeson.object
    [ "peerType" .= case peerId of
        HonestPeer _      -> "honest" :: String
        AdversarialPeer _ -> "adversarial" :: String
    , "peerIndex" .= case peerId of
        HonestPeer n      -> n
        AdversarialPeer n -> n
    ]

instance Aeson.FromJSON PeerId where
  parseJSON = Aeson.withObject "PeerId" $ \v -> do
    peerType <- v Aeson..: "peerType"
    peerIndex <- v Aeson..: "peerIndex"
    case peerType of
      "honest"      -> pure $ HonestPeer peerIndex
      "adversarial" -> pure $ AdversarialPeer peerIndex
      (_ :: String) -> fail $ "Unknown peerType: " ++ peerType

instance QC.Arbitrary PeerId where
  arbitrary = QC.oneof
    [ fmap (HonestPeer . QC.getNonNegative) QC.arbitrary
    , fmap (AdversarialPeer . QC.getNonNegative) QC.arbitrary
    ]

-- | General-purpose functor for associating data to a peer.
data Peer a =
  Peer {
    name  :: PeerId,
    value :: a
  }
  deriving (Eq, Show, Generic)

instance Functor Peer where
  fmap f Peer {name, value} = Peer {name, value = f value}

instance Foldable Peer where
  foldr step z (Peer _ a) = step a z

instance Traversable Peer where
  sequenceA (Peer name fa) =
    Peer name <$> fa

instance Condense a => Condense (Peer a) where
  condense Peer {name, value} = condense name ++ ": " ++ condense value

instance CondenseList a => CondenseList (Peer a) where
  condenseList peers =
    zipWith
      (\name value -> name ++ ": " ++ value)
      (condenseList $ name <$> peers)
      (condenseList $ value <$> peers)

instance (QC.Arbitrary a) => QC.Arbitrary (Peer a) where
  arbitrary = do
    name <- QC.arbitrary
    value <- QC.arbitrary
    return Peer {..}

-- | General-purpose functor for associating data to each of a set of peers.
data Peers a = Peers
  { honestPeers      :: Map Int a,
    adversarialPeers :: Map Int a
  }
  deriving (Eq, Show, Generic, Traversable)

-- | Variant of 'honestPeers' that returns a map with 'PeerId's as keys.
honestPeers' :: Peers a -> Map PeerId a
honestPeers' = Map.mapKeysMonotonic HonestPeer . honestPeers

-- | Variant of 'honestPeers' that returns a map with 'PeerId's as keys and
-- values as 'Peer's.
honestPeers'' :: Peers a -> Map PeerId (Peer a)
honestPeers'' = Map.mapWithKey Peer . honestPeers'

-- | Variant of 'adversarialPeers' that returns a map with 'PeerId's as keys.
adversarialPeers' :: Peers a -> Map PeerId a
adversarialPeers' peers = Map.mapKeysMonotonic AdversarialPeer $ adversarialPeers peers

-- | Variant of 'adversarialPeers' that returns a map with 'PeerId's as keys and
-- values as 'Peer's.
adversarialPeers'' :: Peers a -> Map PeerId (Peer a)
adversarialPeers'' = Map.mapWithKey Peer . adversarialPeers'

instance Functor Peers where
  fmap f Peers {honestPeers, adversarialPeers} =
    Peers
      { honestPeers = f <$> honestPeers,
        adversarialPeers = f <$> adversarialPeers
      }

instance Foldable Peers where
  foldMap f Peers {honestPeers, adversarialPeers} =
    foldMap f honestPeers <> foldMap f adversarialPeers

instance (QC.Arbitrary a) => QC.Arbitrary (Peers a) where
  arbitrary = do
    honestPeers <- QC.arbitrary
    adversarialPeers <- QC.arbitrary
    return Peers {..}

peersToJSON :: Peers Aeson.Value -> Aeson.Value
peersToJSON Peers {honestPeers, adversarialPeers} =
  let pairToJSON (peerId, value) = Aeson.object
        [ "peerId" .= peerId
        , "value" .= value
        ]
  in Aeson.object
    [ "honestPeers" .= Aeson.listValue pairToJSON (Map.toList honestPeers)
    , "adversarialPeers" .= Aeson.listValue pairToJSON (Map.toList adversarialPeers)
    ]

peersFromJSON
  :: Aeson.Value -> Aeson.Parser (Peers Aeson.Value)
peersFromJSON = Aeson.withObject "Peers" $ \v -> do
  let
    pairFromJSON :: Aeson.Value -> Aeson.Parser (Int, Aeson.Value)
    pairFromJSON = Aeson.withObject "Peer" $ \obj -> do
      peerId <- obj .: "peerId"
      value <- obj .: "value"
      pure (peerId, value)
  honestPeersList <- v .: "honestPeers"
  adversarialPeersList <- v .: "adversarialPeers"
  let parseFromList = fmap Map.fromList . Aeson.listParser pairFromJSON
  honestPeers <- parseFromList honestPeersList
  adversarialPeers <- parseFromList adversarialPeersList
  pure Peers {..}

instance Aeson.ToJSON a => Aeson.ToJSON (Peers a) where
  toJSON = peersToJSON . fmap Aeson.toJSON
instance Aeson.FromJSON a => Aeson.FromJSON (Peers a) where
  parseJSON = peersFromJSON >=> traverse Aeson.parseJSON

-- | A set of peers with only one honest peer carrying the given value.
peersOnlyHonest :: a -> Peers a
peersOnlyHonest value =
  Peers
    { honestPeers = Map.singleton 1 value,
      adversarialPeers = Map.empty
    }

peersOnlyAdversary :: a -> Peers a
peersOnlyAdversary value =
  Peers
    { adversarialPeers = Map.singleton 1 value,
      honestPeers = Map.empty
    }

-- | Extract all 'PeerId's.
getPeerIds :: Peers a -> [PeerId]
getPeerIds Peers {honestPeers, adversarialPeers} =
  (HonestPeer <$> Map.keys honestPeers) ++ (AdversarialPeer <$> Map.keys adversarialPeers)

getPeer :: PeerId -> Peers a -> Peer a
getPeer (HonestPeer n) Peers {honestPeers} = Peer (HonestPeer n) (honestPeers Map.! n)
getPeer (AdversarialPeer n) Peers {adversarialPeers} = Peer (AdversarialPeer n) (adversarialPeers Map.! n)

-- | Apply a function to the value at a specific peer ID, returning the
-- updated 'Peers' structure and the result of the function.
updatePeer :: (a -> (a, b)) -> PeerId -> Peers a -> (Peers a, b)
updatePeer f (HonestPeer n) Peers {honestPeers, adversarialPeers} =
  let (a, b) = f (honestPeers Map.! n)
   in (Peers {honestPeers = Map.insert n a honestPeers, adversarialPeers}, b)
updatePeer f (AdversarialPeer n) Peers {honestPeers, adversarialPeers} =
  let (a, b) = f (adversarialPeers Map.! n)
   in (Peers {honestPeers, adversarialPeers = Map.insert n a adversarialPeers}, b)

-- | Convert 'Peers' to a list of 'Peer'.
peersList :: Peers a -> [Peer a]
peersList Peers {honestPeers, adversarialPeers} =
  Map.foldrWithKey
    (\k v -> (Peer (HonestPeer k) v :))
    ( Map.foldrWithKey
        (\k v -> (Peer (AdversarialPeer k) v :))
        []
        adversarialPeers
    )
    honestPeers

-- | Generate the list of all adversarial peer IDs.
--
-- > enumerateAdversaries = fmap AdversarialPeer [1 ..]
enumerateAdversaries :: [PeerId]
enumerateAdversaries = AdversarialPeer <$> [1 ..]

-- | Construct 'Peers' from values, adding adversary names based on the default schema.
peers'
  :: [a]  -- ^ Honest values
  -> [a]  -- ^ Adversarial values
  -> Peers a
peers' hs as =
  Peers
    { honestPeers = Map.fromList $ zip [1 ..] hs,
      adversarialPeers = Map.fromList $ zip [1 ..] as
    }

-- | Make a 'Peers' structure from individual 'Peer's.
peersFromPeerList :: [Peer a] -> Peers a
peersFromPeerList peers =
  let (hs, as) = partitionPeers peers
   in Peers
        { honestPeers = Map.fromList hs,
          adversarialPeers = Map.fromList as
        }
  where
    partitionPeers :: [Peer a] -> ([(Int, a)], [(Int, a)])
    partitionPeers =
      foldl
        ( \(hs, as) (Peer pid v) -> case pid of
            HonestPeer n      -> ((n, v) : hs, as)
            AdversarialPeer n -> (hs, (n, v) : as)
        )
        ([], [])

unionWithKey :: (PeerId -> a -> a -> a) -> Peers a -> Peers a -> Peers a
unionWithKey f peers1 peers2 =
  Peers
    { honestPeers = Map.unionWithKey (f . HonestPeer) (honestPeers peers1) (honestPeers peers2),
      adversarialPeers = Map.unionWithKey (f . AdversarialPeer) (adversarialPeers peers1) (adversarialPeers peers2)
    }

-- | Make a 'Peers' structure from a list of peer ids and a default value.
peersFromPeerIdList :: [PeerId] -> a -> Peers a
peersFromPeerIdList = flip $ \val -> peersFromPeerList . fmap (flip Peer val)

-- | Like 'peersFromPeerIdList' with @()@.
peersFromPeerIdList' :: [PeerId] -> Peers ()
peersFromPeerIdList' = flip peersFromPeerIdList ()

-- | Same as 'toMap' but the map contains unwrapped values.
toMap' :: Peers a -> Map PeerId a
toMap' Peers {honestPeers, adversarialPeers} =
  Map.union
    (Map.mapKeysMonotonic HonestPeer honestPeers)
    (Map.mapKeysMonotonic AdversarialPeer adversarialPeers)

-- | Convert 'Peers' to an explicit map from 'PeerId's to wrapped values.
--
-- Returns a coherent map: the peer ID at each entry matches the entry's index. This
-- is witnessed by the following invariant:
-- INVARIANT: and $ zipWith (==) $ fmap (mapSnd name) $ Map.toList (toMap peers) == True
toMap :: Peers a -> Map PeerId (Peer a)
toMap = Map.mapWithKey Peer . toMap'

-- | Same as 'fromMap' but the map contains unwrapped values.
fromMap' :: Map PeerId a -> Peers a
fromMap' peers =
  let (honestPeers, adversarialPeers) =
        Map.mapEitherWithKey
          ( \case
              HonestPeer _ -> Left
              AdversarialPeer _ -> Right
          )
          peers
   in Peers
        { honestPeers = Map.mapKeysMonotonic unHonestPeer honestPeers,
          adversarialPeers = Map.mapKeysMonotonic unAdversarialPeer adversarialPeers
        }
  where
    unHonestPeer (HonestPeer n) = n
    unHonestPeer _              = error "unHonestPeer: not a honest peer"
    unAdversarialPeer (AdversarialPeer n) = n
    unAdversarialPeer _ = error "unAdversarialPeer: not an adversarial peer"

fromMap :: Map PeerId (Peer a) -> Peers a
fromMap = fromMap' . Map.map value

-- | Remove a peer.
deletePeer :: PeerId -> Peers a -> Peers a
deletePeer (HonestPeer n) Peers {honestPeers, adversarialPeers} =
  Peers {honestPeers = Map.delete n honestPeers, adversarialPeers}
deletePeer (AdversarialPeer n) Peers {honestPeers, adversarialPeers} =
  Peers {honestPeers, adversarialPeers = Map.delete n adversarialPeers}

isHonestPeerId :: PeerId -> Bool
isHonestPeerId (HonestPeer _) = True
isHonestPeerId _              = False

isAdversarialPeerId :: PeerId -> Bool
isAdversarialPeerId (AdversarialPeer _) = True
isAdversarialPeerId _                   = False
