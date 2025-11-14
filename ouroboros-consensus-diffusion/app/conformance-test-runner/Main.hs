module Main (main) where

import qualified Data.Map.Merge.Lazy as M
import Test.Consensus.PeerSimulator.Resources (PeerSimulatorResources(..), makePeerSimulatorResources)
import Control.Tracer (nullTracer)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Aeson (encode, throwDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Coerce
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable
import qualified Network.Socket as Socket
import Options (Options (..), parseOptions)
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
import Ouroboros.Network.PeerSelection.LedgerPeers (RelayAccessPoint (..), UseLedgerPeers (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..), WarmValency (..))
import Server (run)
import System.Environment (getArgs)
import Test.Consensus.PointSchedule (PointSchedule (..))
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
  args <- getArgs
  opts <- parseOptions args
  contents <- BSL8.readFile (optTestFile opts)
  pointSchedule <- throwDecode contents :: IO (PointSchedule Bool)
  let simPeerMap = buildPeerMap (optPort opts) pointSchedule
  BSL8.writeFile (optOutputTopologyFile opts) (encode $ makeTopology simPeerMap)

zipMaps :: Ord k => Map k a -> Map k b -> Map k (a, b)
zipMaps = M.merge M.dropMissing M.dropMissing $ M.zipWithMatched $ const (,)

runServer :: IO ()
runServer = do
  let peerMap = buildPeerMap 6001 testPointSchedule

  peerSim <- makePeerSimulatorResources nullTracer undefined $ NonEmpty.fromList $ M.keys peerMap

  peerServers <-
    for (zipMaps peerMap $ psrPeers peerSim) $ \(port, res) -> do
      -- Make a TMVar for the chainsync and blockfetch channels exposed through
      -- the miniprotocols. These get threaded into the server, which will fill
      -- them once the NUT has connected.
      csChannelTMV <- newTVarIO False
      bfChannelTMV <- newTVarIO False

      putStrLn $ "starting server on " <> show port
      let sockAddr = Socket.SockAddrInet port $ Socket.tupleToHostAddress (127, 0, 0, 1)
      thread <- async $ run res csChannelTMV bfChannelTMV sockAddr
      pure ((csChannelTMV, bfChannelTMV), thread)

  -- Now, take each of the resulting TMVars. This effectively blocks until the
  -- NUT has connected.
  _peerChannels <- atomically $ do
    for peerServers $ \((csChanTMV, bfChanTMV), _thread) -> do
      csChan <- readTVar csChanTMV
      bfChan <- readTVar bfChanTMV
      pure (csChan, bfChan)

  for_ peerServers $ uninterruptibleCancel . snd

  putStrLn "took everything"

  pure ()
