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

-- | Implements a server that waits for an incoming connection to ChainSync or
-- BlockFetch, and forwards the resulting channels to a TMVar so they can be
-- picked up by the peer simulator.
module MiniProtocols (peerSimServer) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad (forever)
import Control.Monad.Class.MonadSay
import Control.Tracer
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Network.Mux as Mux
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import Ouroboros.Consensus.Node (stdVersionDataNTN)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
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

peerSimServer ::
  forall m blk addr.
  ( IOLike m
  , SerialiseNodeToNodeConstraints blk
  , SupportedNetworkProtocolVersion blk
  , MonadSay m
  ) =>
  StrictTMVar m (Mux.Channel m BL.ByteString) ->
  StrictTMVar m (Mux.Channel m BL.ByteString) ->
  CodecConfig blk ->
  (NodeToNodeVersion -> addr -> CBOR.Encoding) ->
  (NodeToNodeVersion -> forall s. CBOR.Decoder s addr) ->
  NetworkMagic ->
  Versions
    NodeToNodeVersion
    NodeToNodeVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode addr BL.ByteString m Void ())
peerSimServer csChanTMV bfChanTMV codecCfg encAddr decAddr networkMagic = do
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
          $ MiniProtocolCb
          $ \_ctx channel ->
            runPeer nullTracer cKeepAliveCodec channel $
              keepAliveServerPeer keepAliveServer
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.chainSyncMiniProtocolNum
          N2N.chainSyncProtocolLimits
          $ MiniProtocolCb
          $ \_ctx channel -> do
            say "hello from cs"
            atomically $
              putTMVar csChanTMV channel
            pure ((), Nothing)
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.blockFetchMiniProtocolNum
          N2N.blockFetchProtocolLimits
          $ MiniProtocolCb
          $ \_ctx channel -> do
            say "hello from bf"
            atomically $
              putTMVar bfChanTMV channel
            pure ((), Nothing)
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.txSubmissionMiniProtocolNum
          N2N.txSubmissionProtocolLimits
          $ MiniProtocolCb
          $ \_ctx _channel -> forever $ threadDelay 10
      ]
     where
      Consensus.N2N.Codecs
        { cKeepAliveCodec
        -- , cChainSyncCodecSerialised
        -- , cBlockFetchCodecSerialised
        } =
          Consensus.N2N.defaultCodecs codecCfg blockVersion encAddr decAddr version

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
