{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans () where

import           Codec.Serialise (Serialise (decode, encode))
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId,
                     TxId)
import           Ouroboros.Consensus.Node.Run
                     (SerialiseNodeToNodeConstraints (..))
import           Ouroboros.Consensus.Node.Serialisation (SerialiseNodeToNode)
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader,
                     decodeTrivialSerialisedHeader,
                     encodeTrivialSerialisedHeader)
import           Ouroboros.Network.Block (Serialised)
import           Test.Util.TestBlock (TestBlock)

-- * Target instances

instance SerialiseNodeToNodeConstraints TestBlock where
  estimateBlockSize = const 0

-- ** Instances needed to fulfill the constrainst for @SerializeNodeToNodeContraints TestBlock@

instance SerialiseNodeToNode TestBlock TestBlock
instance SerialiseNodeToNode TestBlock (Header TestBlock)
instance SerialiseNodeToNode TestBlock (Serialised TestBlock)
instance SerialiseNodeToNode TestBlock (SerialisedHeader TestBlock)
instance SerialiseNodeToNode TestBlock (GenTx TestBlock)
instance SerialiseNodeToNode TestBlock (GenTxId TestBlock)

data instance GenTx TestBlock
data instance TxId (GenTx TestBlock)
deriving instance Generic (GenTx TestBlock)

instance Serialise (GenTx TestBlock)

deriving instance Generic (GenTxId TestBlock)
instance Serialise (GenTxId TestBlock)

instance Serialise (SerialisedHeader TestBlock) where
  encode = encodeTrivialSerialisedHeader
  decode = decodeTrivialSerialisedHeader
