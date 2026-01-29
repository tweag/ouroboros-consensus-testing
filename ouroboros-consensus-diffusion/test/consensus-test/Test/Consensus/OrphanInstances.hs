{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains (orphan) instances to serialize a 'GenesisTest' for
-- the @consensus-test-runner@ input test file.
--
-- Types with 'Generic' instances can derive 'ToJSON' and 'FromJSON'
-- automatically; in turn, to standalone derive a 'Generic' instance, all of
-- its constructors must in scope. Because of this, opaque data type instances
-- need to be written manually.
--
-- Note that most of the constraints in this instance declarations are caused
-- by the abstraction of the block type parameter.
module Test.Consensus.OrphanInstances () where

import Cardano.Slotting.Time (SlotLength)
import Control.Monad.Class.MonadTime.SI (Time)
import Data.Aeson
  ( FromJSON (parseJSON)
  , FromJSONKey ()
  , KeyValue ((.=))
  , ToJSON (toEncoding, toJSON)
  , ToJSONKey ()
  , defaultOptions
  , genericToEncoding
  , object
  , withObject
  , (.:)
  )
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block.Abstract (GenesisWindow (..))
import Ouroboros.Consensus.Config (SecurityParam (..))
import Ouroboros.Network.AnchoredFragment (Anchor)
import Ouroboros.Network.AnchoredSeq
  ( Anchorable (..)
  , AnchoredSeq (..)
  , fromOldestFirst
  , toOldestFirst
  )
import Ouroboros.Network.Block (HasHeader, HeaderHash)
import Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import Test.Consensus.PointSchedule
  ( BlockFetchTimeout (..)
  , CSJParams (..)
  , ForecastRange (..)
  , GenesisTest (..)
  , LoPBucketParams (..)
  , PointSchedule (..)
  )

import           Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout (..))
import Test.Consensus.PointSchedule.Peers (PeerId, Peers (..))
import Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..))
import Test.Ouroboros.Consensus.ChainGenerator.Params (Delta (..))

-- * Target instances

deriving instance Generic (GenesisTest blk schedule)
instance
  (ToJSON (HeaderHash blk), HasHeader blk, ToJSON schedule, ToJSON blk, ToJSONKey (HeaderHash blk)) =>
  ToJSON (GenesisTest blk schedule)
  where
  toEncoding = genericToEncoding defaultOptions
instance
  (FromJSON (HeaderHash blk), HasHeader blk, FromJSON schedule, FromJSON blk, FromJSONKey (HeaderHash blk)) =>
  FromJSON (GenesisTest blk schedule)

-- ** 'GenesisTest' field instances

instance ToJSON SecurityParam
instance FromJSON SecurityParam

deriving instance Generic GenesisWindow
instance ToJSON GenesisWindow
instance FromJSON GenesisWindow

deriving instance Generic ForecastRange
instance ToJSON ForecastRange
instance FromJSON ForecastRange

deriving instance Generic Delta
instance ToJSON Delta
instance FromJSON Delta

deriving instance Generic ChainSyncTimeout
instance ToJSON ChainSyncTimeout
instance FromJSON ChainSyncTimeout

deriving instance Generic BlockFetchTimeout
instance ToJSON BlockFetchTimeout
instance FromJSON BlockFetchTimeout

deriving instance Generic LoPBucketParams
instance ToJSON LoPBucketParams
instance FromJSON LoPBucketParams

deriving instance Generic CSJParams
instance ToJSON CSJParams
instance FromJSON CSJParams

instance ToJSON SlotLength
instance FromJSON SlotLength

deriving instance Generic (PointSchedule blk)
instance ToJSON blk => ToJSON (PointSchedule blk)
instance FromJSON blk => FromJSON (PointSchedule blk)

instance (ToJSON a, ToJSON b) => ToJSON (AnchoredSeq v a b) where
  toJSON anchoredSeq =
    object
      [ "anchor" .= anchor anchoredSeq
      , "sequence" .= toOldestFirst anchoredSeq
      ]

instance (Anchorable v a b, FromJSON a, FromJSON b) => FromJSON (AnchoredSeq v a b) where
  parseJSON = withObject "AnchoredSeq" $ \obj -> do
    a <- obj .: "anchor"
    s <- obj .: "sequence"
    pure $ fromOldestFirst a s

instance ToJSON (HeaderHash blk) => ToJSON (Anchor blk)
instance FromJSON (HeaderHash blk) => FromJSON (Anchor blk)

-- *** 'PointSchedule' field related instances

instance ToJSON Time
instance FromJSON Time

instance ToJSON PeerId
instance FromJSON PeerId

deriving instance Generic (Peers a)
instance ToJSON a => ToJSON (Peers a)
instance FromJSON a => FromJSON (Peers a)

deriving instance Generic (SchedulePoint blk)
instance ToJSON blk => ToJSON (SchedulePoint blk)
instance FromJSON blk => FromJSON (SchedulePoint blk)
