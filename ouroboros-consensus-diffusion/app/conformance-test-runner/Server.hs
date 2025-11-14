{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server (run) where

import Test.Consensus.PeerSimulator.Resources (PeerResources)
import Control.ResourceRegistry
import Control.Tracer
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Contravariant ((>$<))
import Data.Void (Void)
import MiniProtocols (peerSimServer)
import qualified Network.Mux as Mux
import Network.Socket (SockAddr (..))
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Mock.Ledger
import Ouroboros.Consensus.Mock.Node ()
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import Orphans ()
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Mux
import qualified Ouroboros.Network.NodeToNode as N2N
import Ouroboros.Network.PeerSelection.PeerSharing.Codec
  ( decodeRemoteAddress
  , encodeRemoteAddress
  )
import Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import qualified Ouroboros.Network.Protocol.Handshake as Handshake
import qualified Ouroboros.Network.Server.Simple as Server
import qualified Ouroboros.Network.Snocket as Snocket
import Ouroboros.Network.Socket (SomeResponderApplication (..), configureSocket)

-- | Glue code for using just the bits from the Diffusion Layer that we need in
-- this context.
serve ::
  SockAddr ->
  N2N.Versions
    N2N.NodeToNodeVersion
    N2N.NodeToNodeVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode SockAddr BL.ByteString IO Void ()) ->
  IO Void
serve sockAddr application = withIOManager \iocp ->
  Server.with
    (Snocket.socketSnocket iocp)
    Snocket.makeSocketBearer
    (\sock addr -> configureSocket sock (Just addr))
    sockAddr
    HandshakeArguments
      { haHandshakeTracer = show >$< stdoutTracer
      , haBearerTracer = show >$< stdoutTracer
      , haHandshakeCodec = Handshake.nodeToNodeHandshakeCodec
      , haVersionDataCodec = Handshake.cborTermVersionDataCodec N2N.nodeToNodeCodecCBORTerm
      , haAcceptVersion = Handshake.acceptableVersion
      , haQueryVersion = Handshake.queryVersion
      , haTimeLimits = Handshake.timeLimitsHandshake
      }
    (SomeResponderApplication <$> application)
    (\_ serverAsync -> wait serverAsync)

run ::
  forall blk.
  ( SupportedNetworkProtocolVersion blk
  , SerialiseNodeToNodeConstraints blk
  , ConfigSupportsNode blk
  , blk ~ SimpleBlock SimpleMockCrypto SimplePraosRuleExt
  ) =>
  PeerResources IO blk ->
  -- | A TMVar for the chainsync channel that we will fill in once the node connects.
  StrictTVar IO Bool ->
  -- | A TMVar for the blockfetch channel that we will fill in once the node connects.
  StrictTVar IO Bool ->
  SockAddr ->
  IO Void
run res csChanTMV bfChanTMV sockAddr = withRegistry \_registry ->
  serve sockAddr
    $ peerSimServer @_ @(SimpleBlock SimpleMockCrypto SimplePraosRuleExt)
      res
      csChanTMV
      bfChanTMV
      SimpleCodecConfig
      encodeRemoteAddress
      decodeRemoteAddress
    $ getNetworkMagic @(SimpleBlock SimpleMockCrypto SimplePraosRuleExt) SimpleBlockConfig
