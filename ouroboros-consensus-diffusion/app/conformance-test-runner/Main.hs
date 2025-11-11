module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Coerce
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
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
import Test.Consensus.PointSchedule
import Test.Consensus.PointSchedule.Peers (PeerId (..), getPeerIds)

testMap :: Map PeerId PortNumber
testMap =
  M.fromList
    [ (HonestPeer 1, 6001)
    , (HonestPeer 2, 6002)
    , (AdversarialPeer 1, 6003)
    ]

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
main = BSL8.putStrLn $ encode $ makeTopology testMap
