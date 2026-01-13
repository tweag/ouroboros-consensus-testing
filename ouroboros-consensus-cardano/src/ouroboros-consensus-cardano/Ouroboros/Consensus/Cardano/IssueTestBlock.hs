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
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Keys hiding (hashVerKeyVRF)
import           Cardano.Ledger.Shelley.API hiding (hashVerKeyVRF)
import           Cardano.Ledger.Shelley.Core
import           Cardano.Protocol.Crypto
import           Cardano.Protocol.TPraos.BHeader
import           Cardano.Protocol.TPraos.OCert
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock,
                     pattern BlockShelley)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..),
                     ShelleyHash (..))
import           Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import           Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import           Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import           Test.Cardano.Ledger.Shelley.Examples.Consensus
import           Test.Cardano.Ledger.Shelley.Generator.Core
import           Test.Cardano.Ledger.Shelley.Utils hiding (mkVRFKeyPair)
import           Test.Consensus.Genesis.Setup.GenChains (IssueTestBlock (..))


instance IssueTestBlock (CardanoBlock StandardCrypto) where
  issueFirstBlock fork slot = makeCardanoBlock (Just fork) 0 slot Nothing
  issueSuccessorBlock fork slot (BlockShelley (ShelleyBlock (Block (BHeader (BHBody {bheaderBlockNo}) _) _) (ShelleyHash hh))) =
    makeCardanoBlock fork (bheaderBlockNo + 1) slot $ Just $ HashHeader hh
  issueSuccessorBlock _ _ _ =
    -- Impossible because we only ever produce 'BlockShelley' in
    -- 'makeCardanoBlock'.
    error "issueSuccessorBlock: impossible"


-- | Construct a fake 'CardanoBlock' with all of its crypto intact.
makeCardanoBlock
  :: Maybe Int
  -> BlockNo
  -> SlotNo
  -> Maybe HashHeader
  -> CardanoBlock StandardCrypto
makeCardanoBlock fork blockNo slot mhash = BlockShelley $
  let blk =
        shelleyLedgerBlock slot blockNo mhash
          (exampleTx
            (mkWitnessesPreAlonzo (Proxy @ShelleyEra))
              exampleTxBodyShelley
              $ forkToAuxData fork)
    in ShelleyBlock blk $ ShelleyHash $ unHashHeader $ bhHash $ bheader blk


-- | Construct a made-up (but believable) cardano block for the Shelley era.
shelleyLedgerBlock ::
  SlotNo ->
  BlockNo ->
  Maybe HashHeader ->
  -- ^ The parent hash, if there is one.
  Tx ShelleyEra ->
  -- ^ Some transaction to stick in the block.
  Block (BHeader StandardCrypto) ShelleyEra
shelleyLedgerBlock slot blockNo prev tx = Block blockHeader blockBody
  where
    keys :: AllIssuerKeys StandardCrypto 'StakePool
    keys = exampleKeys

    hotKey = kesSignKey $ snd $ NE.head $ aikHot keys
    KeyPair vKeyCold _ = aikCold keys

    blockHeader :: BHeader StandardCrypto
    blockHeader = BHeader blockHeaderBody (unsoundPureSignedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: BHBody StandardCrypto
    blockHeaderBody =
      BHBody
        { bheaderBlockNo = blockNo
        , bheaderSlotNo = slot
        , bheaderPrev = maybe GenesisHash BlockHash prev
        , bheaderVk = coerceKeyRole vKeyCold
        , bheaderVrfVk = vrfVerKey $ aikVrf keys
        , bheaderEta = mkCertifiedVRF (mkBytes 0) (vrfSignKey $ aikVrf keys)
        , bheaderL = mkCertifiedVRF (mkBytes 1) (vrfSignKey $ aikVrf keys)
        , bsize = 2345
        , bhash = hashTxSeq blockBody
        , bheaderOCert = mkOCert keys 0 (KESPeriod 0)
        , bprotver = ProtVer (natVersion @2) 0
        }

    blockBody = toTxSeq @ShelleyEra (StrictSeq.fromList [tx])

    mkBytes :: Int -> Cardano.Ledger.BaseTypes.Seed
    mkBytes = Seed . mkDummyHash @Blake2b_256


-- | We need to do something with our fork number to futz the resulting hash.
-- So we stick it into the auxilliary data that gets attached to the block.
forkToAuxData :: Maybe Int -> TxAuxData ShelleyEra
forkToAuxData
  = ShelleyTxAuxData
  . foldMap (\i -> M.singleton 1 $ I $ fromIntegral i)

