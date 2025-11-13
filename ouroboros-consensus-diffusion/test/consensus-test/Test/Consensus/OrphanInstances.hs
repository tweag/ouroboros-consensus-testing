{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.OrphanInstances () where

import Control.Monad.Class.MonadTime.SI (Time)
import Data.Aeson
  ( FromJSON
  , ToJSON (toEncoding)
  , defaultOptions
  , genericToEncoding
  )
import GHC.Generics
import Test.Consensus.PointSchedule (PointSchedule (..))
import Test.Consensus.PointSchedule.Peers (PeerId, Peers (..))
import Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..))

deriving instance Generic (PointSchedule blk)
instance ToJSON blk => ToJSON (PointSchedule blk) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON blk => FromJSON (PointSchedule blk)

instance ToJSON Time where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Time

instance ToJSON PeerId where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON PeerId

deriving instance Generic (Peers a)
instance ToJSON a => ToJSON (Peers a) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (Peers a)

deriving instance Generic (SchedulePoint blk)
instance ToJSON blk => ToJSON (SchedulePoint blk) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON blk => FromJSON (SchedulePoint blk)
