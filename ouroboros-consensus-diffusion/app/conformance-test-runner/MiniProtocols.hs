{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implement ChainSync and BlockFetch servers on top of just the immutable DB.
module MiniProtocols (immDBServer) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad (forever)
import Control.ResourceRegistry
import Control.Tracer
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Network.Mux as Mux
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
  ( blockFetchServer'
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( chainSyncServerForFollower
  )
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import Ouroboros.Consensus.Node (stdVersionDataNTN)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Driver (runPeer)
import Ouroboros.Network.KeepAlive (keepAliveServer)
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , MiniProtocolCb (..)
  , OuroborosApplication (..)
  , OuroborosApplicationWithMinimalCtx
  , RunMiniProtocol (..)
  )
import Ouroboros.Network.NodeToNode
  ( NodeToNodeVersionData (..)
  , Versions (..)
  )
import qualified Ouroboros.Network.NodeToNode as N2N
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.Handshake.Version (Version (..))
import Ouroboros.Network.Protocol.KeepAlive.Server
  ( keepAliveServerPeer
  )

immDBServer ::
  forall m blk addr.
  ( IOLike m
  , HasHeader blk
  , ShowProxy blk
  , SerialiseNodeToNodeConstraints blk
  , SupportedNetworkProtocolVersion blk
  ) =>
  CodecConfig blk ->
  (NodeToNodeVersion -> addr -> CBOR.Encoding) ->
  (NodeToNodeVersion -> forall s. CBOR.Decoder s addr) ->
  NetworkMagic ->
  Versions
    NodeToNodeVersion
    NodeToNodeVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode addr BL.ByteString m Void ())
immDBServer codecCfg encAddr decAddr networkMagic = do
  forAllVersions application
 where
  forAllVersions ::
    (NodeToNodeVersion -> BlockNodeToNodeVersion blk -> r) ->
    Versions NodeToNodeVersion NodeToNodeVersionData r
  forAllVersions mkR =
    Versions $
      Map.mapWithKey mkVersion $
        supportedNodeToNodeVersions (Proxy @blk)
   where
    mkVersion version blockVersion =
      Version
        { versionApplication = const $ mkR version blockVersion
        , versionData =
            stdVersionDataNTN
              networkMagic
              N2N.InitiatorOnlyDiffusionMode
              PeerSharingDisabled
        }

  application ::
    NodeToNodeVersion ->
    BlockNodeToNodeVersion blk ->
    OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode addr BL.ByteString m Void ()
  application version blockVersion =
    OuroborosApplication miniprotocols
   where
    miniprotocols =
      [ mkMiniProtocol
          Mux.StartOnDemandAny
          N2N.keepAliveMiniProtocolNum
          N2N.keepAliveProtocolLimits
          keepAliveProt
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.chainSyncMiniProtocolNum
          N2N.chainSyncProtocolLimits
          chainSyncProt
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.blockFetchMiniProtocolNum
          N2N.blockFetchProtocolLimits
          blockFetchProt
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.txSubmissionMiniProtocolNum
          N2N.txSubmissionProtocolLimits
          txSubmissionProt
      ]
     where
      Consensus.N2N.Codecs
        { cKeepAliveCodec
        , cChainSyncCodecSerialised
        , cBlockFetchCodecSerialised
        } =
          Consensus.N2N.defaultCodecs codecCfg blockVersion encAddr decAddr version

      keepAliveProt =
        MiniProtocolCb $ \_ctx channel ->
          runPeer nullTracer cKeepAliveCodec channel $
            keepAliveServerPeer keepAliveServer
      chainSyncProt =
        MiniProtocolCb $ \_ctx channel ->
          undefined
      -- withRegistry $
      --   runPeer nullTracer cChainSyncCodecSerialised channel
      --     . chainSyncServerPeer
      --     . chainSyncServer immDB ChainDB.getSerialisedHeaderWithPoint
      blockFetchProt =
        MiniProtocolCb $ \_ctx channel ->
          undefined
      -- withRegistry $
      --   runPeer nullTracer cBlockFetchCodecSerialised channel
      --     . blockFetchServerPeer
      --     . blockFetchServer immDB ChainDB.getSerialisedBlockWithPoint
      txSubmissionProt =
        -- never reply, there is no timeout
        MiniProtocolCb $ \_ctx _channel -> forever $ threadDelay 10

    mkMiniProtocol miniProtocolStart miniProtocolNum limits proto =
      MiniProtocol
        { miniProtocolNum
        , miniProtocolLimits = limits N2N.defaultMiniProtocolParameters
        , miniProtocolRun = ResponderProtocolOnly proto
        , miniProtocolStart
        }

-- | The ChainSync specification requires sending a rollback instruction to the
-- intersection point right after an intersection has been negotiated. (Opening
-- a connection implicitly negotiates the Genesis point as the intersection.)
data ChainSyncIntersection blk
  = JustNegotiatedIntersection !(Point blk)
  | AlreadySentRollbackToIntersection
  deriving stock Generic
  deriving anyclass NoThunks
