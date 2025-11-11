{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import qualified Cardano.Tools.ImmDBServer.Diffusion as ImmDBServer
import Control.ResourceRegistry
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Coerce
import Data.Foldable
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable
import qualified Network.Socket as Socket
import Options
  ( Options (..)
  , execParser
  , options
  )
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Diffusion.Topology
  ( LocalRootPeersGroup (..)
  , LocalRootPeersGroups (..)
  , NetworkTopology (..)
  , RootConfig (..)
  )
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection (PeerAdvertise (..), PortNumber)
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..), WarmValency (..))
import Server (run)
import Test.Consensus.PeerSimulator.Resources
import Test.Consensus.PeerSimulator.Run
import Test.Consensus.PointSchedule
import Test.Consensus.PointSchedule.Peers (PeerId (..), Peers (Peers), getPeerIds)

testPointSchedule :: PointSchedule blk
testPointSchedule =
  PointSchedule
    { psSchedule =
        Peers
          (M.fromList [(1, undefined), (2, undefined)])
          (M.fromList [(1, undefined)])
    , psStartOrder = []
    , psMinEndTime = undefined
    }

buildPeerMap :: PortNumber -> PointSchedule blk -> Map PeerId PortNumber
buildPeerMap firstPort = M.fromList . flip zip [firstPort ..] . getPeerIds . psSchedule

toRelayAP :: PortNumber -> RelayAccessPoint
toRelayAP = RelayAccessAddress (read "127.0.0.1")

makeTopology :: Foldable t => t PortNumber -> NetworkTopology () ()
makeTopology ports =
  NetworkTopology
    { localRootPeersGroups =
        LocalRootPeersGroups $
          pure $
            LocalRootPeersGroup
              { localRoots =
                  RootConfig
                    { rootAccessPoints = fmap toRelayAP $ toList ports
                    , rootAdvertise = DoAdvertisePeer -- is this the right value?
                    }
              , hotValency = coerce num_peers
              , warmValency = coerce $ num_peers + 1
              , rootDiffusionMode = InitiatorOnlyDiffusionMode -- is this the right value?
              , extraFlags = ()
              }
    , publicRootPeers = []
    , useLedgerPeers = DontUseLedgerPeers -- is this the right value?
    , peerSnapshotPath = Nothing
    , extraConfig = ()
    }
 where
  num_peers = length ports

main :: IO ()
main = do
  opts <- execParser options
  contents <- BSL8.readFile (optTestFile opts)
  pointSchedule <- throwDecode contents :: IO (PointSchedule Bool)
  let simPeerMap = buildPeerMap (optPort opts) pointSchedule
  BSL8.writeFile (optOutputTopologyFile opts) (encode $ makeTopology simPeerMap)

runServer :: IO ()
runServer = do
  let peerMap = testMap

  peerServers <-
    for peerMap $ \port -> do
      csChannelTMV <- newEmptyTMVarIO
      bfChannelTMV <- newEmptyTMVarIO

      putStrLn $ "starting server on " <> show port
      let sockAddr = Socket.SockAddrInet port $ Socket.tupleToHostAddress (127, 0, 0, 1)
      thread <- async $ run sockAddr csChannelTMV bfChannelTMV
      pure ((csChannelTMV, bfChannelTMV), thread)

  peerChannels <- atomically $ do
    for peerServers $ \((csChanTMV, bfChanTMV), _thread) -> do
      csChan <- takeTMVar csChanTMV
      bfChan <- takeTMVar bfChanTMV
      pure (csChan, bfChan)

  pure ()
