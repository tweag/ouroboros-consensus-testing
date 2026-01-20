{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server (run) where

import           Control.ResourceRegistry
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Contravariant ((>$<))
import           Data.Void (Void)
import           MiniProtocols (peerSimServer)
import qualified Network.Mux as Mux
import           Network.Socket (SockAddr (..))
import           Orphans ()
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux
import qualified Ouroboros.Network.NodeToNode as N2N
import           Ouroboros.Network.PeerSelection.PeerSharing.Codec
                     (decodeRemoteAddress, encodeRemoteAddress)
import           Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import qualified Ouroboros.Network.Protocol.Handshake as Handshake
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Socket (SomeResponderApplication (..),
                     configureSocket)
import           Test.Consensus.PeerSimulator.Resources (PeerResources)
import qualified Test.Util.TestBlock as TB
import           Test.Util.TestBlock (TestBlock)

-- | Glue code for using just the bits from the Diffusion Layer that we need in
-- this context.
serve ::
  StrictTMVar IO SockAddr ->
  SockAddr ->
  N2N.Versions
    N2N.NodeToNodeVersion
    N2N.NodeToNodeVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode SockAddr BL.ByteString IO Void ()) ->
  IO Void
serve _ sockAddr application = withIOManager \iocp -> do
  let sn = Snocket.socketSnocket iocp
      family = Snocket.addrFamily sn sockAddr
  bracket (Snocket.open sn family) (Snocket.close sn) \socket -> do
    networkMutableState <- N2N.newNetworkMutableState
    configureSocket socket (Just sockAddr)
    Snocket.bind sn socket sockAddr
    Snocket.listen sn socket
    N2N.withServer
      sn
      N2N.nullNetworkServerTracers
        { N2N.nstHandshakeTracer = show >$< stdoutTracer
        , N2N.nstErrorPolicyTracer = show >$< stdoutTracer
        }
      networkMutableState
      acceptedConnectionsLimit
      socket
      application
      nullErrorPolicies
 where
  acceptedConnectionsLimit =
    N2N.AcceptedConnectionsLimit
      { N2N.acceptedConnectionsHardLimit = maxBound
      , N2N.acceptedConnectionsSoftLimit = maxBound
      , N2N.acceptedConnectionsDelay = 0
      }

run ::
  forall blk.
  ( SupportedNetworkProtocolVersion blk
  , SerialiseNodeToNodeConstraints blk
  , ConfigSupportsNode blk
  , blk ~ TestBlock
  ) =>
  PeerResources IO blk ->
  -- | A TMVar for the connecting peer
  StrictTMVar IO SockAddr ->
  -- | A TMVar for the chainsync channel that we will fill in once the node connects.
  StrictTVar IO Bool ->
  -- | A TMVar for the blockfetch channel that we will fill in once the node connects.
  StrictTVar IO Bool ->
  SockAddr ->
  IO Void
run res incomingTV csChanTMV bfChanTMV sockAddr = withRegistry \_registry ->
  serve incomingTV sockAddr
    $ peerSimServer @_ @TestBlock
      res
      csChanTMV
      bfChanTMV
      TB.TestBlockCodecConfig
      encodeRemoteAddress
      decodeRemoteAddress
    $ getNetworkMagic @TestBlock $ TB.TestBlockConfig $ NumCoreNodes 0
