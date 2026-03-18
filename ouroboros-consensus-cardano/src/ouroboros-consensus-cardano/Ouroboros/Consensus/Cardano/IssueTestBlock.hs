{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Cardano.IssueTestBlock () where


import           Cardano.Crypto.Hash as Hash
import           Cardano.Crypto.KES as KES
import           Cardano.Crypto.VRF.Class (deriveVerKeyVRF)
import           Cardano.Ledger.Alonzo.Tx
import           Cardano.Ledger.Alonzo.TxAuxData (mkAlonzoTxAuxData)
import           Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Keys hiding (hashVerKeyVRF)
import           Cardano.Ledger.Shelley.API hiding (hashVerKeyVRF)
import           Cardano.Ledger.Shelley.Core
import           Cardano.Protocol.Crypto
import           Cardano.Protocol.TPraos.BHeader
import qualified Data.Map as M
import qualified Data.Sequence.Strict as StrictSeq
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock,
                     pattern BlockConway)
import           Ouroboros.Consensus.Protocol.Praos.Common
import           Ouroboros.Consensus.Protocol.Praos.Header
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..),
                     ShelleyHash (..))
import           Ouroboros.Consensus.Shelley.Node.Common
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import           Test.Cardano.Ledger.Conway.Examples.Consensus
import           Test.Cardano.Ledger.Core.KeyPair (mkWitnessesVKey)
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE
import           Test.Cardano.Ledger.Shelley.Utils hiding (mkVRFKeyPair)
import           Test.Consensus.Genesis.Setup.GenChains (IssueTestBlock (..))


credentials :: ShelleyLeaderCredentials StandardCrypto
credentials = undefined

instance IssueTestBlock (CardanoBlock StandardCrypto) where
  issueFirstBlock fork slot = makeCardanoBlock (Just fork) 0 slot Nothing
  issueSuccessorBlock fork slot (BlockConway (ShelleyBlock
      (Block (Header (HeaderBody {hbBlockNo, hbSlotNo}) _) _)
      (ShelleyHash hh))) =
    makeCardanoBlock fork
      (hbBlockNo + 1)
      (hbSlotNo + slot) $ Just $ HashHeader hh
  issueSuccessorBlock _ _ _ =
    -- Impossible because we only ever produce 'BlockConway' in
    -- 'makeCardanoBlock'.
    error "issueSuccessorBlock: impossible"


-- | Construct a fake 'CardanoBlock' with all of its crypto intact.
makeCardanoBlock
  :: Maybe Int
  -> BlockNo
  -> SlotNo
  -> Maybe HashHeader
  -> CardanoBlock StandardCrypto
makeCardanoBlock fork blockNo slot mhash = BlockConway $
  let blk =
        conwayLedgerBlock slot blockNo mhash $
          AlonzoTx
            exampleTxBodyConway
            ( AlonzoTxWits
                (mkWitnessesVKey (hashAnnotated exampleTxBodyConway) [asWitness SLE.examplePayKey]) -- vkey
                mempty -- bootstrap
                mempty -- txscripts
                mempty -- txdats
                mempty -- redeemers
            )
            (IsValid True)
            ( SJust $ forkToAuxData fork
            )
      Block (Header bhb _) _ = blk
    in ShelleyBlock blk $ ShelleyHash $ castHash $ hbBodyHash bhb


-- | Construct a made-up (but believable) cardano block for the Conway era.
conwayLedgerBlock ::
  SlotNo ->
  BlockNo ->
  Maybe HashHeader ->
  -- ^ The parent hash, if there is one.
  Tx ConwayEra ->
  -- ^ Some transaction to stick in the block.
  Block (Header StandardCrypto) ConwayEra
conwayLedgerBlock slot blockNo prev tx = Block blockHeader blockBody
  where
    PraosCanBeLeader
        { praosCanBeLeaderSignKeyVRF
        , praosCanBeLeaderColdVerKey
        , praosCanBeLeaderOpCert
        } = shelleyLeaderCredentialsCanBeLeader credentials

    blockHeader :: Header StandardCrypto
    blockHeader =
        Header blockHeaderBody $
          unsoundPureSignedKES () 0 blockHeaderBody $
            shelleyLeaderCredentialsInitSignKey credentials

    blockHeaderBody :: HeaderBody StandardCrypto
    blockHeaderBody =
      HeaderBody
        { hbBlockNo = blockNo
        , hbSlotNo = slot
        , hbPrev = maybe GenesisHash BlockHash prev
        , hbVk = coerceKeyRole praosCanBeLeaderColdVerKey
        , hbVrfVk = deriveVerKeyVRF praosCanBeLeaderSignKeyVRF
        , hbVrfRes = mkCertifiedVRF (mkBytes 0) praosCanBeLeaderSignKeyVRF
        , hbBodySize = 2345
        , hbBodyHash = hashTxSeq blockBody
        , hbOCert = praosCanBeLeaderOpCert
        , hbProtVer = ProtVer (natVersion @2) 0
        }

    blockBody = toTxSeq @ConwayEra (StrictSeq.fromList [tx])

    mkBytes :: Int -> Cardano.Ledger.BaseTypes.Seed
    mkBytes = Seed . mkDummyHash @Blake2b_256


-- | We need to do something with our fork number to futz the resulting hash.
-- So we stick it into the auxilliary data that gets attached to the block.
forkToAuxData :: Maybe Int -> TxAuxData ConwayEra
forkToAuxData fk = mkAlonzoTxAuxData @_ @ConwayEra (foldMap (M.singleton 1 . I . fromIntegral) fk) []

