{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Orphans where

import Codec.Serialise
import Data.Typeable (Typeable)
import GHC.Generics
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Node.InitStorage
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Node.Serialisation
import Ouroboros.Consensus.Storage.ChainDB
  ( ImmutableDbSerialiseConstraints
  , SerialiseDiskConstraints
  , VolatileDbSerialiseConstraints
  )
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Network.Block (Serialised)
import Test.Util.TestBlock

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

{- SerialiseNodeToNode class methods
  encodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> a -> Encoding
  decodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> forall s. Decoder s a

  -- When the config is not needed, we provide a default, unversioned
  -- implementation using 'Serialise'

  default encodeNodeToNode ::
    Serialise a =>
    CodecConfig blk -> BlockNodeToNodeVersion blk -> a -> Encoding
  encodeNodeToNode _ccfg _version = encode

  default decodeNodeToNode ::
    Serialise a =>
    CodecConfig blk -> BlockNodeToNodeVersion blk -> forall s. Decoder s a
  decodeNodeToNode _ccfg _version = decode
-}

data instance GenTx TestBlock
data instance TxId (GenTx TestBlock)
deriving instance Generic (GenTx TestBlock)

instance Serialise (GenTx TestBlock)

deriving instance Generic (GenTxId TestBlock)
instance Serialise (GenTxId TestBlock)

deriving instance Generic (SerialisedHeader TestBlock)
instance Serialise (SerialisedHeader TestBlock)

{-
instance Serialise (GenDepPair Serialised (NestedCtxt Header TestBlock)) where
  encode = undefined
  decode = undefined
-}
