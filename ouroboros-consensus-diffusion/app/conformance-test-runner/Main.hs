{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.Aeson (encode, throwDecode)
import Data.Bits (Ior (..))
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Coerce
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
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
import Ouroboros.Network.PeerSelection.LedgerPeers (RelayAccessPoint (..), UseLedgerPeers (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..), WarmValency (..))
import Server (run)
import System.Exit (ExitCode (..))
import Test.Consensus.PointSchedule (PointSchedule (..))
import Test.Consensus.PointSchedule.Peers (PeerId (..), Peers (Peers), getPeerIds)

-- | Exit statuses for the test runner. 'Success' is represented
-- by an empty set of 'StatusFlags'.
data ExitStatus = InternalError | BadUsage | Flags (Set StatusFlag)

pattern Success :: ExitStatus
pattern Success <- Flags (null -> True)
  where
    Success = Flags mempty

-- | A 'ContinueShrinking' flag is returned whenever the 'TestFailed' or got
-- 'Success' with a non-empty shrink index as input, unless no more shrinking
-- on the input is possible. It is intended to signal the user to manually
-- pump the shrinker.
data StatusFlag = TestFailed | ContinueShrinking deriving (Eq, Ord)

exitStatusToCode :: ExitStatus -> ExitCode
exitStatusToCode = \case
  Success -> ExitSuccess
  InternalError -> ExitFailure 1
  BadUsage -> ExitFailure 2
  -- Flags are combined using bit-wise OR.
  Flags flags -> ExitFailure $ getIor $ foldMap flagToCode flags
 where
  flagToCode :: StatusFlag -> Ior Int
  flagToCode = \case
    TestFailed -> Ior 4
    ContinueShrinking -> Ior 8

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
  let peerMap = buildPeerMap 6001 testPointSchedule

  peerServers <-
    for peerMap $ \port -> do
      -- Make a TMVar for the chainsync and blockfetch channels exposed through
      -- the miniprotocols. These get threaded into the server, which will fill
      -- them once the NUT has connected.
      csChannelTMV <- newEmptyTMVarIO
      bfChannelTMV <- newEmptyTMVarIO

      putStrLn $ "starting server on " <> show port
      let sockAddr = Socket.SockAddrInet port $ Socket.tupleToHostAddress (127, 0, 0, 1)
      thread <- async $ run csChannelTMV bfChannelTMV sockAddr
      pure ((csChannelTMV, bfChannelTMV), thread)

  -- Now, take each of the resulting TMVars. This effectively blocks until the
  -- NUT has connected.
  _peerChannels <- atomically $ do
    for peerServers $ \((csChanTMV, bfChanTMV), _thread) -> do
      csChan <- takeTMVar csChanTMV
      bfChan <- takeTMVar bfChanTMV
      pure (csChan, bfChan)

  pure ()
