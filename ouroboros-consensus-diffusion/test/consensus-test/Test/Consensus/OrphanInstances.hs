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
  , KeyValue ((.=))
  , ToJSON (toJSON)
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
import Ouroboros.Network.Block (HeaderHash)

import           Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout (..))
import Test.Ouroboros.Consensus.ChainGenerator.Params (Delta (..))



-- ** 'GenesisTest' field instances

instance ToJSON SecurityParam
instance FromJSON SecurityParam

deriving instance Generic GenesisWindow
instance ToJSON GenesisWindow
instance FromJSON GenesisWindow

deriving instance Generic Delta
instance ToJSON Delta
instance FromJSON Delta

deriving instance Generic ChainSyncTimeout
instance ToJSON ChainSyncTimeout
instance FromJSON ChainSyncTimeout

instance ToJSON SlotLength
instance FromJSON SlotLength

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
