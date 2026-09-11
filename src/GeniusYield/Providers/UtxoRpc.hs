module GeniusYield.Providers.UtxoRpc (
  UtxoRpcConfig (..),
  UtxoRpc,
  mkUtxoRpc,
  utxoRpcSlotActions,
  utxoRpcGetParameters,
  utxoRpcQueryUtxo,
  utxoRpcGetSlotOfCurrentBlock,
  withUtxoRpcConnection
) where

import Cardano.Api qualified as Api
import Cardano.Slotting.Time (SystemStart)
import Data.Time (NominalDiffTime)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS

import GeniusYield.Types

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common
import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map

import Proto.Utxorpc.V1alpha.Query.Query
import Proto.Utxorpc.V1alpha.Query.Query_Fields (hash, index , keys, maybe'parsedState, maybe'txoRef, maybe'params, maybe'values)
import qualified Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields as Cardano_Fields

import Proto.Utxorpc.V1alpha.Sync.Sync
import Proto.Utxorpc.V1alpha.Sync.Sync_Fields qualified as Sync_Fields (maybe'tip, slot)
import Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields (address, maybe'bigInt, scripts, maybe'script, maybe'nativeScript,
                                                    maybe'bigInt, assets, coin, name, outputCoin, policyId, k)
import Proto.Utxorpc.V1alpha.Cardano.Cardano (Asset, BigInt, BigInt'BigInt (..), Multiasset, TxOutput, NativeScript,
                                             Datum, ScriptNOfK, Script'Script (..), NativeScript'NativeScript (..),
                                             Script, NativeScriptList)

import Cardano.Api.Ledger qualified as Api.L
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Conway.PParams
  ( ConwayPParams (..)
  , THKD (..)
  )
import Data.Ratio ((%))

import Proto.Utxorpc.V1alpha.Cardano.Cardano qualified as ProtoCardano

import Cardano.Ledger.Plutus qualified as LedgerPlutus
import Cardano.Slotting.Time qualified as CTime
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Proto.Utxorpc.V1alpha.Query.Query_Fields qualified as Query_Fields
import Proto.Utxorpc.V1alpha.Cardano.Cardano (Genesis)
import Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields (startTime)

type instance RequestMetadata (Protobuf SyncService "readTip") = NoMetadata
type instance ResponseInitialMetadata (Protobuf SyncService "readTip") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf SyncService "readTip") = NoMetadata

type instance RequestMetadata (Protobuf QueryService "readUtxos") = NoMetadata
type instance ResponseInitialMetadata (Protobuf QueryService "readUtxos") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf QueryService "readUtxos") = NoMetadata

type instance RequestMetadata (Protobuf QueryService "readParams") = NoMetadata
type instance ResponseInitialMetadata (Protobuf QueryService "readParams") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf QueryService "readParams") = NoMetadata

type instance RequestMetadata (Protobuf QueryService "readGenesis") = NoMetadata
type instance ResponseInitialMetadata (Protobuf QueryService "readGenesis") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf QueryService "readGenesis") = NoMetadata

type instance RequestMetadata (Protobuf QueryService "readEraSummary") = NoMetadata
type instance ResponseInitialMetadata (Protobuf QueryService "readEraSummary") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf QueryService "readEraSummary") = NoMetadata

type instance RequestMetadata (Protobuf QueryService "searchUtxos") = NoMetadata
type instance ResponseInitialMetadata (Protobuf QueryService "searchUtxos") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf QueryService "searchUtxos") = NoMetadata

utxoRpcGetSlotOfCurrentBlock :: Connection -> IO GYSlot
utxoRpcGetSlotOfCurrentBlock conn = do
  response <-
    nonStreaming
      conn
      (rpcWith @(Protobuf SyncService "readTip") def)
      (Proto (defMessage :: ReadTipRequest))

  case getProto response ^. Sync_Fields.maybe'tip of
    Nothing ->
      fail "UTxO-RPC ReadTipResponse did not contain a tip"
    Just blockRef ->
      pure $ slotFromWord64 (blockRef ^. Sync_Fields.slot)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

withUtxoRpcConnection :: UtxoRpcConfig -> (Connection -> IO a) -> IO a
withUtxoRpcConnection config action =
  withConnection def server action
  where
    address' =
      Address
        (utxoRpcHost config)
        (fromIntegral $ utxoRpcPort config)
        Nothing

    server
      | utxoRpcUseTls config =
          ServerSecure
            (ValidateServer certStoreFromSystem)
            SslKeyLogNone
            address'
      | otherwise =
          ServerInsecure address'

-- | Configuration for an UTxO-RPC endpoint.
--
-- The endpoint is deliberately provider-neutral.  Dolos is one possible
-- implementation of the UTxO-RPC server, but this provider is written
-- against the UTxO-RPC protocol rather than against Dolos itself.
data UtxoRpcConfig = UtxoRpcConfig
  { utxoRpcHost :: !String
  , utxoRpcPort :: !Int
  , utxoRpcUseTls :: !Bool
  , utxoRpcSlotCacheTime :: !NominalDiffTime
  }

--------------------------------------------------------------------------------
-- Provider
--------------------------------------------------------------------------------

-- | Connection/client state for the UTxO-RPC provider.
--
-- The actual gRPC client is kept behind this type so the rest of Atlas does
-- not depend on the transport implementation.
data UtxoRpc = UtxoRpc
  { utxoRpcConfig :: !UtxoRpcConfig
  }

mkUtxoRpc :: UtxoRpcConfig -> IO UtxoRpc
mkUtxoRpc config =
  pure UtxoRpc
    { utxoRpcConfig = config
    }

--------------------------------------------------------------------------------
-- Genesis
--------------------------------------------------------------------------------

utxoRpcReadGenesis :: Connection -> IO Genesis
utxoRpcReadGenesis conn = do
  response <-
    nonStreaming
      conn
      (rpcWith @(Protobuf QueryService "readGenesis") def)
      (Proto (defMessage :: ReadGenesisRequest))

  case getProto response ^. Query_Fields.maybe'config of
    Nothing ->
      fail "UTxO-RPC ReadGenesisResponse has no config"

    Just (ReadGenesisResponse'Cardano genesis) ->
      pure genesis

--------------------------------------------------------------------------------
-- SystemStart
--------------------------------------------------------------------------------

utxoRpcSystemStart :: UtxoRpc -> IO SystemStart
utxoRpcSystemStart provider =
  withUtxoRpcConnection (utxoRpcConfig provider) $ \conn -> do
    genesis <- utxoRpcReadGenesis conn
    pure $
      CTime.SystemStart $
        posixSecondsToUTCTime $
          fromIntegral (genesis ^. startTime)

{--------------------------------------------------------------------------------
-- EraHistory
--------------------------------------------------------------------------------

 UTxO-RPC's (Dolos 1.6.0) ReadEraSummary only reflects
 whatever era-boundary records the backing node (Dolos) has itself locally
 processed since it started tracking chain state -- it does not
 reconstruct the full historical era table the way a full node or
 Blockfrost's /network/eras endpoint does. Dolos's own miniBF
 /network/eras route already does this padding (see
 crates/minibf/src/routes/network.rs in the Dolos repo, "Special,
 hardcoded stuff" block); its gRPC/UTxO-RPC (src/serve/grpc/v1alpha/query.rs
 read_era_summary) and Ogmios (src/serve/o7s_unix/statequery.rs) interfaces
 do not. Atlas' era interpreter is a fixed 7-slot (Byron..Conway) structure
 with no partial form, so a live response with fewer summaries can never
 be parsed into one.

 Era history for the UtxoRpc provider is, for now, supplied by the caller
 instead (see 'GeniusYield.GYConfig.utxoRpcNetworkEraHistory', a hardcoded
 per-network table) and threaded into 'utxoRpcGetParameters' below.

 The code below is kept, commented out, as the long-term replacement: once
 Dolos's read_era_summary (gRPC) is patched to reuse miniBF's padding
 logic, restore this and go back to threading 'utxoRpcEraHistory provider'
 into 'utxoRpcGetParameters' instead of a hardcoded 'Api.EraHistory'.

 import Data.Word (Word64)
 import Data.Maybe (fromMaybe)
 import Ouroboros.Consensus.Block.Abstract (GenesisWindow (..))
 import Cardano.Slotting.Slot qualified as CSlot
 import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
 import GeniusYield.Providers.Common (parseEraHist)
 import Proto.Utxorpc.V1alpha.Cardano.Cardano (EraSummary, EraBoundary)
 import Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields (securityParam, maybe'activeSlotsCoeff)

 utxoRpcEraHistory :: UtxoRpc -> IO Api.EraHistory
 utxoRpcEraHistory provider =
   withUtxoRpcConnection (utxoRpcConfig provider) $ \conn -> do
     genesis <- utxoRpcReadGenesis conn
     genesisWin <- either fail pure (computeGenesisWindow genesis)

     response <-
       nonStreaming
         conn
         (rpcWith @(Protobuf QueryService "readEraSummary") def)
         (Proto (defMessage :: ReadEraSummaryRequest))

     case getProto response ^. Query_Fields.maybe'summary of
       Nothing ->
         fail "UTxO-RPC ReadEraSummaryResponse has no summary"

       Just (ReadEraSummaryResponse'Cardano eraSummaries) ->
         let summs = eraSummaries ^. Cardano_Fields.summaries
         in maybe
              (fail "UTxO-RPC returned an unexpected number of era summaries")
              pure
              (parseEraHist (mkEra genesis genesisWin) summs)

   where
     mkBound :: EraBoundary -> Ouroboros.Bound
     mkBound b =
       Ouroboros.Bound
         { Ouroboros.boundTime = CTime.RelativeTime (fromIntegral (b ^. Cardano_Fields.time))
         , Ouroboros.boundSlot = CSlot.SlotNo (b ^. Cardano_Fields.slot)
         , Ouroboros.boundEpoch = CSlot.EpochNo (fromIntegral (b ^. Cardano_Fields.epoch))
         }

     -- | The spec doesn't hand us epoch/slot length per era, so: for the
     -- final (unbounded) era use Genesis' own current-era params, and for
     -- bounded historical eras derive them from that era's own boundaries.
     mkEraParams :: Genesis -> Word64 -> EraSummary -> Ouroboros.EraParams
     mkEraParams genesis genesisWin s =
       case s ^. Cardano_Fields.maybe'end of
         Nothing ->
           Ouroboros.EraParams
             { Ouroboros.eraEpochSize = CSlot.EpochSize (fromIntegral (genesis ^. Cardano_Fields.epochLength))
             , Ouroboros.eraSlotLength = CTime.mkSlotLength (fromIntegral (genesis ^. Cardano_Fields.slotLength))
             , Ouroboros.eraSafeZone = Ouroboros.StandardSafeZone genesisWin
             , Ouroboros.eraGenesisWin = GenesisWindow genesisWin
             }
         Just end ->
           let start' =
                 fromMaybe
                   (error "UTxO-RPC bounded era summary has no start boundary")
                   (s ^. Cardano_Fields.maybe'start)

               slotDelta = end ^. Cardano_Fields.slot - start' ^. Cardano_Fields.slot
               epochDelta = end ^. Cardano_Fields.epoch - start' ^. Cardano_Fields.epoch
               timeDelta = end ^. Cardano_Fields.time - start' ^. Cardano_Fields.time

               epochSizeSlots =
                 if epochDelta == 0
                   then error "UTxO-RPC era summary has zero epoch delta"
                   else slotDelta `div` epochDelta

               slotLenSecs =
                 if slotDelta == 0
                   then error "UTxO-RPC era summary has zero slot delta"
                   else fromIntegral timeDelta / fromIntegral slotDelta :: Double
           in
             Ouroboros.EraParams
               { Ouroboros.eraEpochSize = CSlot.EpochSize epochSizeSlots
               , Ouroboros.eraSlotLength = CTime.mkSlotLength (realToFrac slotLenSecs)
               , Ouroboros.eraSafeZone = Ouroboros.StandardSafeZone genesisWin
               , Ouroboros.eraGenesisWin = GenesisWindow genesisWin
               }

     mkEra :: Genesis -> Word64 -> EraSummary -> Ouroboros.EraSummary
     mkEra genesis genesisWin s =
       Ouroboros.EraSummary
         { Ouroboros.eraStart =
             mkBound $
               fromMaybe
                 (error "UTxO-RPC era summary has no start boundary")
                 (s ^. Cardano_Fields.maybe'start)
         , Ouroboros.eraEnd =
             maybe Ouroboros.EraUnbounded (Ouroboros.EraEnd . mkBound) (s ^. Cardano_Fields.maybe'end)
         , Ouroboros.eraParams = mkEraParams genesis genesisWin s
         }

 -- | Safe zone / genesis window, computed as (ceil) 3k/f from the security
 -- parameter and active slot coefficient, since UTxO-RPC's genesis doesn't
 -- expose it directly (same approach/TODO as Atlas' own Maestro provider).
 computeGenesisWindow :: Genesis -> Either String Word64
 computeGenesisWindow genesis = do
   activeSlotsRat <-
     maybe
       (Left "UTxO-RPC genesis has no active_slots_coeff")
       (convertRational "active_slots_coeff")
       (genesis ^. maybe'activeSlotsCoeff)

   let k' = toInteger (genesis ^. securityParam)

   if activeSlotsRat <= 0
     then Left "UTxO-RPC genesis active_slots_coeff is non-positive"
     else pure $ ceiling (3 * fromInteger k' / activeSlotsRat)
-}

--------------------------------------------------------------------------------
-- Slot actions
--------------------------------------------------------------------------------

-- | Build Atlas' slot actions around the UTxO-RPC ReadTip operation.
--
-- The Grapesy connection is deliberately supplied by the caller.  Its
-- lifetime must cover the Atlas provider callback.
utxoRpcSlotActions ::
  UtxoRpc ->
  Connection ->
  IO GYSlotActions
utxoRpcSlotActions provider conn =
  makeSlotActions
    (utxoRpcSlotCacheTime $ utxoRpcConfig provider)
    (utxoRpcGetSlotOfCurrentBlock conn)

--------------------------------------------------------------------------------
-- Protocol parameters
--------------------------------------------------------------------------------

-- | Build Atlas' parameter provider around UTxO-RPC ReadParams.
--
-- Atlas caches system start and era history for the lifetime of the provider
-- and refreshes protocol parameters at epoch boundaries.
--
-- Era history is not sourced from UTxO-RPC (see the note above
-- 'utxoRpcSlotActions' -- Dolos's gRPC interface does not return the full
-- historical era table); the caller supplies it instead.
utxoRpcGetParameters ::
  UtxoRpc ->
  Api.EraHistory ->
  IO GYGetParameters
utxoRpcGetParameters provider eraHistory =
  makeGetParameters
    (utxoRpcReadParams provider)
    (utxoRpcSystemStart provider)
    (pure eraHistory)
    (withUtxoRpcConnection (utxoRpcConfig provider) utxoRpcGetSlotOfCurrentBlock)

utxoRpcReadParams :: UtxoRpc -> IO ApiProtocolParameters
utxoRpcReadParams provider =
  withUtxoRpcConnection
    (utxoRpcConfig provider)
    $ \conn -> do
      response <-
        nonStreaming
          conn
          (rpcWith @(Protobuf QueryService "readParams") def)
          (Proto (defMessage :: ReadParamsRequest))

      let chainParams =
            getProto response ^. maybe'values

      case chainParams of
        Nothing ->
          fail "UTxO-RPC ReadParamsResponse has no values"

        Just chainParams' ->
          case chainParams' ^. maybe'params of
            Nothing ->
              fail "UTxO-RPC ReadParamsResponse values has no params"

            Just params ->
              case params of
                AnyChainParams'Cardano pparams ->
                  case convertPParams pparams of
                    Left err ->
                      fail $
                        "UTxO-RPC protocol parameters conversion failed: "
                          <> err

                    Right result ->
                      pure result

convertPParams ::
  ProtoCardano.PParams ->
  Either String ApiProtocolParameters
convertPParams pparams = do
  minFeeA <-
    convertCoin
      "min_fee_coefficient"
      (pparams ^. Cardano_Fields.maybe'minFeeCoefficient)

  minFeeB <-
    convertCoin
      "min_fee_constant"
      (pparams ^. Cardano_Fields.maybe'minFeeConstant)

  keyDeposit <-
    convertCoin
      "stake_key_deposit"
      (pparams ^. Cardano_Fields.maybe'stakeKeyDeposit)

  poolDeposit <-
    convertCoin
      "pool_deposit"
      (pparams ^. Cardano_Fields.maybe'poolDeposit)

  minPoolCost <-
    convertCoin
      "min_pool_cost"
      (pparams ^. Cardano_Fields.maybe'minPoolCost)

  coinsPerUtxoByte <-
    convertCoin
      "coins_per_utxo_byte"
      (pparams ^. Cardano_Fields.maybe'coinsPerUtxoByte)

  protocolVersion <-
    convertProtocolVersion
      (pparams ^. Cardano_Fields.maybe'protocolVersion)

  poolInfluence <-
    convertBoundedRational @Ledger.NonNegativeInterval
      "pool_influence"
      (pparams ^. Cardano_Fields.maybe'poolInfluence)

  monetaryExpansion <-
    convertBoundedRational @Ledger.UnitInterval
      "monetary_expansion"
      (pparams ^. Cardano_Fields.maybe'monetaryExpansion)

  treasuryExpansion <-
    convertBoundedRational @Ledger.UnitInterval
      "treasury_expansion"
      (pparams ^. Cardano_Fields.maybe'treasuryExpansion)

  costModels <-
    convertCostModels
      (pparams ^. Cardano_Fields.maybe'costModels)

  prices <-
    convertPrices
      (pparams ^. Cardano_Fields.maybe'prices)

  maxTxExUnits <-
    convertExUnits
      "max_execution_units_per_transaction"
      (pparams ^. Cardano_Fields.maybe'maxExecutionUnitsPerTransaction)

  maxBlockExUnits <-
    convertExUnits
      "max_execution_units_per_block"
      (pparams ^. Cardano_Fields.maybe'maxExecutionUnitsPerBlock)

  govActionDeposit <-
    convertCoin
      "governance_action_deposit"
      (pparams ^. Cardano_Fields.maybe'governanceActionDeposit)

  drepDeposit <-
    convertCoin
      "drep_deposit"
      (pparams ^. Cardano_Fields.maybe'drepDeposit)

  minFeeRefScriptCostPerByte <-
    convertBoundedRational @Ledger.NonNegativeInterval
      "min_fee_script_ref_cost_per_byte"
      (pparams ^. Cardano_Fields.maybe'minFeeScriptRefCostPerByte)

  poolVotingThresholds <-
    convertPoolVotingThresholds
      (pparams ^. Cardano_Fields.maybe'poolVotingThresholds)

  drepVotingThresholds <-
    convertDRepVotingThresholds
      (pparams ^. Cardano_Fields.maybe'drepVotingThresholds)

  pure $
    Ledger.PParams $
      ConwayPParams
        { cppMinFeeA =
            THKD minFeeA
        , cppMinFeeB =
            THKD minFeeB
        , cppMaxBBSize =
            THKD $
              fromIntegral $
                pparams ^. Cardano_Fields.maxBlockBodySize
        , cppMaxTxSize =
            THKD $
              fromIntegral $
                pparams ^. Cardano_Fields.maxTxSize
        , cppMaxBHSize =
            THKD $
              fromIntegral $
                pparams ^. Cardano_Fields.maxBlockHeaderSize
        , cppKeyDeposit =
            THKD keyDeposit
        , cppPoolDeposit =
            THKD poolDeposit
        , cppEMax =
            THKD $
              Ledger.EpochInterval $
                fromIntegral $
                  pparams ^. Cardano_Fields.poolRetirementEpochBound
        , cppNOpt =
            THKD $
              fromIntegral $
                pparams ^. Cardano_Fields.desiredNumberOfPools
        , cppA0 =
            THKD poolInfluence
        , cppRho =
            THKD monetaryExpansion
        , cppTau =
            THKD treasuryExpansion
        , cppProtocolVersion =
            protocolVersion
        , cppMinPoolCost =
            THKD minPoolCost
        , cppCoinsPerUTxOByte =
            THKD $
              Api.L.CoinPerByte coinsPerUtxoByte
        , cppCostModels =
            THKD costModels
        , cppPrices =
            THKD prices
        , cppMaxTxExUnits =
            THKD maxTxExUnits
        , cppMaxBlockExUnits =
            THKD maxBlockExUnits
        , cppMaxValSize =
            THKD $
              fromIntegral $
                pparams ^. Cardano_Fields.maxValueSize
        , cppCollateralPercentage =
            THKD $
              fromIntegral $
                pparams ^. Cardano_Fields.collateralPercentage
        , cppMaxCollateralInputs =
            THKD $
              fromIntegral $
                pparams ^. Cardano_Fields.maxCollateralInputs
        , cppPoolVotingThresholds =
            THKD poolVotingThresholds
        , cppDRepVotingThresholds =
            THKD drepVotingThresholds
        , cppCommitteeMinSize =
            THKD $
              fromIntegral $
                pparams ^. Cardano_Fields.minCommitteeSize
        , cppCommitteeMaxTermLength =
            THKD $
              Ledger.EpochInterval $
                fromIntegral $
                  pparams ^. Cardano_Fields.committeeTermLimit
        , cppGovActionLifetime =
            THKD $
              Ledger.EpochInterval $
                fromIntegral $
                  pparams ^. Cardano_Fields.governanceActionValidityPeriod
        , cppGovActionDeposit =
            THKD govActionDeposit
        , cppDRepDeposit =
            THKD drepDeposit
        , cppDRepActivity =
            THKD $
              Ledger.EpochInterval $
                fromIntegral $
                  pparams ^. Cardano_Fields.drepInactivityPeriod
        , cppMinFeeRefScriptCostPerByte =
            THKD minFeeRefScriptCostPerByte
        }

convertCoin ::
  String ->
  Maybe ProtoCardano.BigInt ->
  Either String Ledger.Coin
convertCoin fieldName value = do
  amount <-
    convertBigIntField fieldName value

  if amount < 0
    then
      Left $
        "UTxO-RPC protocol parameter "
          <> fieldName
          <> " is negative"
    else
      pure $ Ledger.Coin amount

convertBigIntField ::
  String ->
  Maybe ProtoCardano.BigInt ->
  Either String Integer
convertBigIntField fieldName value =
  case value of
    Nothing ->
      Left $
        "UTxO-RPC protocol parameter "
          <> fieldName
          <> " is missing"

    Just bigInt ->
      convertBigInt bigInt

convertProtocolVersion ::
  Maybe ProtoCardano.ProtocolVersion ->
  Either String Ledger.ProtVer
convertProtocolVersion value =
  case value of
    Nothing ->
      Left "UTxO-RPC protocol_version is missing"

    Just version -> do
      major <-
        maybe
          (Left "UTxO-RPC protocol_version major is out of bounds")
          Right
          (Ledger.mkVersion $ version ^. Cardano_Fields.major)

      pure $
        Ledger.ProtVer
          { Ledger.pvMajor = major
          , Ledger.pvMinor =
              fromIntegral $
                version ^. Cardano_Fields.minor
          }

convertRational ::
  String ->
  ProtoCardano.RationalNumber ->
  Either String Rational
convertRational fieldName value =
  let numerator =
        toInteger $
          value ^. Cardano_Fields.numerator

      denominator =
        toInteger $
          value ^. Cardano_Fields.denominator
  in
    if denominator == 0
      then
        Left $
          "UTxO-RPC protocol parameter "
            <> fieldName
            <> " has zero denominator"
      else
        pure $
          numerator % denominator

convertBoundedRational ::
  forall a.
  BoundedRational a =>
  String ->
  Maybe ProtoCardano.RationalNumber ->
  Either String a
convertBoundedRational fieldName value = do
  rational <-
    case value of
      Nothing ->
        Left $
          "UTxO-RPC protocol parameter "
            <> fieldName
            <> " is missing"

      Just value' ->
        convertRational fieldName value'

  maybe
    (Left $
      "UTxO-RPC protocol parameter "
        <> fieldName
        <> " is outside its valid range")
    Right
    (Ledger.boundRational @a rational)

-- 'VotingThresholds' is a flat 'repeated RationalNumber' in the utxorpc proto
-- (no named sub-fields at this call site) — index order is not documented in
-- the .proto, but matches how Dolos's grpc/v1alpha/query.rs constructs it
-- (github.com/txpipe/dolos, src/serve/grpc/v1alpha/query.rs).
convertPoolVotingThresholds ::
  Maybe ProtoCardano.VotingThresholds ->
  Either String Ledger.PoolVotingThresholds
convertPoolVotingThresholds Nothing =
  Left "UTxO-RPC protocol parameter pool_voting_thresholds is missing"
convertPoolVotingThresholds (Just vt) =
  case vt ^. Cardano_Fields.thresholds of
    [motionNoConfidence', committeeNormal', committeeNoConfidence', hardForkInitiation', ppSecurityGroup'] -> do
      motionNoConfidence <-
        convertBoundedRational @Ledger.UnitInterval
          "pool_voting_thresholds[0] (motion_no_confidence)"
          (Just motionNoConfidence')

      committeeNormal <-
        convertBoundedRational @Ledger.UnitInterval
          "pool_voting_thresholds[1] (committee_normal)"
          (Just committeeNormal')

      committeeNoConfidence <-
        convertBoundedRational @Ledger.UnitInterval
          "pool_voting_thresholds[2] (committee_no_confidence)"
          (Just committeeNoConfidence')

      hardForkInitiation <-
        convertBoundedRational @Ledger.UnitInterval
          "pool_voting_thresholds[3] (hard_fork_initiation)"
          (Just hardForkInitiation')

      ppSecurityGroup <-
        convertBoundedRational @Ledger.UnitInterval
          "pool_voting_thresholds[4] (pp_security_group)"
          (Just ppSecurityGroup')

      pure
        Ledger.PoolVotingThresholds
          { pvtPPSecurityGroup = ppSecurityGroup
          , pvtMotionNoConfidence = motionNoConfidence
          , pvtHardForkInitiation = hardForkInitiation
          , pvtCommitteeNormal = committeeNormal
          , pvtCommitteeNoConfidence = committeeNoConfidence
          }
    other ->
      Left $
        "UTxO-RPC protocol parameter pool_voting_thresholds has "
          <> show (length other)
          <> " values, expected 5"

convertDRepVotingThresholds ::
  Maybe ProtoCardano.VotingThresholds ->
  Either String Ledger.DRepVotingThresholds
convertDRepVotingThresholds Nothing =
  Left "UTxO-RPC protocol parameter drep_voting_thresholds is missing"
convertDRepVotingThresholds (Just vt) =
  case vt ^. Cardano_Fields.thresholds of
    [ motionNoConfidence'
      , committeeNormal'
      , committeeNoConfidence'
      , updateToConstitution'
      , hardForkInitiation'
      , ppNetworkGroup'
      , ppEconomicGroup'
      , ppTechnicalGroup'
      , ppGovGroup'
      , treasuryWithdrawal'
      ] -> do
        motionNoConfidence <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[0] (motion_no_confidence)"
            (Just motionNoConfidence')

        committeeNormal <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[1] (committee_normal)"
            (Just committeeNormal')

        committeeNoConfidence <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[2] (committee_no_confidence)"
            (Just committeeNoConfidence')

        updateToConstitution <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[3] (update_to_constitution)"
            (Just updateToConstitution')

        hardForkInitiation <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[4] (hard_fork_initiation)"
            (Just hardForkInitiation')

        ppNetworkGroup <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[5] (pp_network_group)"
            (Just ppNetworkGroup')

        ppEconomicGroup <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[6] (pp_economic_group)"
            (Just ppEconomicGroup')

        ppTechnicalGroup <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[7] (pp_technical_group)"
            (Just ppTechnicalGroup')

        ppGovGroup <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[8] (pp_gov_group)"
            (Just ppGovGroup')

        treasuryWithdrawal <-
          convertBoundedRational @Ledger.UnitInterval
            "drep_voting_thresholds[9] (treasury_withdrawal)"
            (Just treasuryWithdrawal')

        pure
          Ledger.DRepVotingThresholds
            { dvtUpdateToConstitution = updateToConstitution
            , dvtTreasuryWithdrawal = treasuryWithdrawal
            , dvtPPTechnicalGroup = ppTechnicalGroup
            , dvtPPNetworkGroup = ppNetworkGroup
            , dvtPPGovGroup = ppGovGroup
            , dvtPPEconomicGroup = ppEconomicGroup
            , dvtMotionNoConfidence = motionNoConfidence
            , dvtHardForkInitiation = hardForkInitiation
            , dvtCommitteeNormal = committeeNormal
            , dvtCommitteeNoConfidence = committeeNoConfidence
            }
    other ->
      Left $
        "UTxO-RPC protocol parameter drep_voting_thresholds has "
          <> show (length other)
          <> " values, expected 10"

convertExUnits ::
  String ->
  Maybe ProtoCardano.ExUnits ->
  Either String Ledger.OrdExUnits
convertExUnits fieldName value =
  case value of
    Nothing ->
      Left $
        "UTxO-RPC protocol parameter "
          <> fieldName
          <> " is missing"

    Just exUnits ->
      pure $
        Ledger.OrdExUnits $
          Ledger.ExUnits
            { Ledger.exUnitsSteps =
                fromIntegral $
                  exUnits ^. Cardano_Fields.steps
            , Ledger.exUnitsMem =
                fromIntegral $
                  exUnits ^. Cardano_Fields.memory
            }

convertPrices ::
  Maybe ProtoCardano.ExPrices ->
  Either String LedgerPlutus.Prices
convertPrices value =
  case value of
    Nothing ->
      Left "UTxO-RPC protocol parameter prices is missing"

    Just prices -> do
      steps <-
        convertBoundedRational @Ledger.NonNegativeInterval
          "prices.steps"
          (Just $ prices ^. Cardano_Fields.steps)

      memory <-
        convertBoundedRational @Ledger.NonNegativeInterval
          "prices.memory"
          (Just $ prices ^. Cardano_Fields.memory)

      pure $
        LedgerPlutus.Prices
          { Ledger.prSteps = steps
          , Ledger.prMem = memory
          }

convertCostModels ::
  Maybe ProtoCardano.CostModels ->
  Either String LedgerPlutus.CostModels
convertCostModels value =
  case value of
    Nothing ->
      Left "UTxO-RPC protocol parameter cost_models is missing"

    Just models -> do
      v1 <-
        requiredCostModel
          "plutus_v1"
          LedgerPlutus.PlutusV1
          (models ^. Cardano_Fields.maybe'plutusV1)

      v2 <-
        requiredCostModel
          "plutus_v2"
          LedgerPlutus.PlutusV2
          (models ^. Cardano_Fields.maybe'plutusV2)

      v3 <-
        requiredCostModel
          "plutus_v3"
          LedgerPlutus.PlutusV3
          (models ^. Cardano_Fields.maybe'plutusV3)

      pure $
        LedgerPlutus.mkCostModels $
          Map.fromList
            [ (LedgerPlutus.PlutusV1, v1)
            , (LedgerPlutus.PlutusV2, v2)
            , (LedgerPlutus.PlutusV3, v3)
            ]

requiredCostModel ::
  String ->
  LedgerPlutus.Language ->
  Maybe ProtoCardano.CostModel ->
  Either String LedgerPlutus.CostModel
requiredCostModel fieldName language value =
  case value of
    Nothing ->
      Left $
        "UTxO-RPC protocol parameter "
          <> fieldName
          <> " is missing"

    Just model ->
      first show $
        LedgerPlutus.mkCostModel
          language
          (model ^. Cardano_Fields.values)

--------------------------------------------------------------------------------
-- UTxO queries
--------------------------------------------------------------------------------

-- | Atlas UTxO query implementation.
--
-- We intentionally provide only the non-datum operations for the MVP.
--
-- The *_WithDatums fields remain 'Nothing'.  Atlas' default implementations
-- would otherwise attempt to use gyLookupDatum, which is deliberately not
-- part of the initial UTxO-RPC provider.
utxoRpcQueryUtxo :: UtxoRpc -> Connection -> GYQueryUTxO
utxoRpcQueryUtxo provider conn =
  GYQueryUTxO
    { gyQueryUtxosAtTxOutRefs' =
        utxoRpcReadUtxos provider conn

    , gyQueryUtxosAtTxOutRefsWithDatums' =
        Nothing

    , gyQueryUtxoAtTxOutRef' =
        utxoRpcReadUtxoAtTxOutRef provider conn

    , gyQueryUtxoRefsAtAddress' =
        \address' ->
          utxosRefs
            <$> utxoRpcQueryAddress provider address' Nothing

    , gyQueryUtxosAtAddress' =
        utxoRpcQueryAddress provider

    , gyQueryUtxosWithAsset' =
        utxoRpcQueryAsset provider

    , gyQueryUtxosAtAddressWithDatums' =
        Nothing

    , gyQueryUtxosAtAddresses' =
        \addresses ->
          mconcat
            <$> traverse
              (\address' ->
                utxoRpcQueryAddress provider address' Nothing)
              addresses

    , gyQueryUtxosAtAddressesWithDatums' =
        Nothing

    , gyQueryUtxosAtPaymentCredential' =
        utxoRpcQueryPaymentCredential provider

    , gyQueryUtxosAtPaymentCredWithDatums' =
        Nothing

    , gyQueryUtxosAtPaymentCredentials' =
        \credentials ->
          mconcat
            <$> traverse
              (\credential ->
                utxoRpcQueryPaymentCredential provider credential Nothing)
              credentials

    , gyQueryUtxosAtPaymentCredsWithDatums' =
        Nothing
    }

--------------------------------------------------------------------------------
-- ReadUtxos
--------------------------------------------------------------------------------

utxoRpcReadUtxos :: UtxoRpc -> Connection -> [GYTxOutRef] -> IO GYUTxOs
utxoRpcReadUtxos _provider conn refs = do
  response <-
    nonStreaming
      conn
      (rpcWith @(Protobuf QueryService "readUtxos") def)
      (Proto request)

  let responseItems =
        getProto response ^. Query_Fields.items

  case traverse convertItem responseItems of
    Left err ->
      fail $ "UTxO-RPC readUtxos conversion failed: " <> err

    Right converted ->
      pure $ utxosFromList converted

  where
    request :: ReadUtxosRequest
    request =
      defMessage
        & keys .~ map convertTxOutRef refs

    convertItem :: AnyUtxoData -> Either String GYUTxO
    convertItem item = do
      txoRef <-
        maybe
          (Left "UTxO-RPC response item has no txo_ref")
          Right
          (item ^. maybe'txoRef)

      ref <-
        convertTxoRef txoRef

      parsedState <-
        maybe
          (Left "UTxO-RPC response item has no parsed_state")
          Right
          (item ^. maybe'parsedState)

      case parsedState of
        AnyUtxoData'Cardano txOutput ->
          convertTxOutput ref txOutput

utxoRpcReadUtxoAtTxOutRef ::
  UtxoRpc ->
  Connection ->
  GYTxOutRef ->
  IO (Maybe GYUTxO)
utxoRpcReadUtxoAtTxOutRef provider conn ref = do
  utxos <- utxoRpcReadUtxos provider conn [ref]
  pure $
    case utxosToList utxos of
      [utxo] -> Just utxo
      _ -> Nothing

--------------------------------------------------------------------------------
-- Address conversion
--------------------------------------------------------------------------------

convertAddress :: ByteString -> Either String GYAddress
convertAddress bs = do
  addressAny <-
    first show $
      Api.deserialiseFromRawBytes
        Api.AsAddressAny
        bs

  pure $ addressFromApi addressAny

convertAddressToBytes :: GYAddress -> ByteString
convertAddressToBytes =
  Api.serialiseToRawBytes . addressToApi

--------------------------------------------------------------------------------
-- BigInt conversion
--------------------------------------------------------------------------------

convertBigInt :: BigInt -> Either String Integer
convertBigInt value =
  case value ^. maybe'bigInt of
    Nothing ->
      Left "UTxO-RPC BigInt has no value"

    Just bigInt ->
      case bigInt of
        BigInt'Int n ->
          pure $ fromIntegral n

        BigInt'BigUInt bytes ->
          pure $ bytesToInteger bytes

        BigInt'BigNInt bytes ->
          pure $ negate (bytesToInteger bytes)

bytesToInteger :: ByteString -> Integer
bytesToInteger =
  BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

--------------------------------------------------------------------------------
-- Native asset conversion
--------------------------------------------------------------------------------

convertMultiasset ::
  Multiasset ->
  Either String [(GYAssetClass, Integer)]
convertMultiasset multiasset = do
  policyIdBytes <- pure $ multiasset ^. policyId

  policyId' <-
    first show $
      Api.deserialiseFromRawBytes
        Api.AsPolicyId
        policyIdBytes

  let mintingPolicyId' = mintingPolicyIdFromApi policyId'

  traverse
    (convertAsset mintingPolicyId')
    (multiasset ^. assets)

convertAsset ::
  GYMintingPolicyId ->
  Asset ->
  Either String (GYAssetClass, Integer)
convertAsset mintingPolicyId' asset = do
  quantity <-
    convertBigInt (asset ^. outputCoin)

  tokenName <-
    maybe
      (Left "UTxO-RPC token name exceeds 32 bytes")
      Right
      (tokenNameFromBS (asset ^. name))

  pure
    ( GYToken mintingPolicyId' tokenName
    , quantity
    )

--------------------------------------------------------------------------------
-- Value conversion
--------------------------------------------------------------------------------

convertTxOutputValue ::
  TxOutput ->
  Either String GYValue
convertTxOutputValue txOutput = do
  lovelace <- convertBigInt (txOutput ^. coin)

  nativeAssets <-
    concat
      <$> traverse
        convertMultiasset
        (txOutput ^. assets)

  pure $
    valueFromList
      ( (GYLovelace, lovelace)
          : nativeAssets
      )

--------------------------------------------------------------------------------
-- TxOutRef conversion
--------------------------------------------------------------------------------

convertTxOutRef :: GYTxOutRef -> TxoRef
convertTxOutRef ref =
  let Api.TxIn txId (Api.TxIx ix) = txOutRefToApi ref
  in
    defMessage
      & hash .~ Api.serialiseToRawBytes txId
      & index .~ fromIntegral ix

convertTxoRef :: TxoRef -> Either String GYTxOutRef
convertTxoRef ref = do
  txId <-
    first show $
      Api.deserialiseFromRawBytes
        Api.AsTxId
        (ref ^. hash)

  pure $
    txOutRefFromApiTxIdIx
      txId
      (Api.TxIx $ fromIntegral (ref ^. index))

--------------------------------------------------------------------------------
-- TxOutput conversion
--------------------------------------------------------------------------------

convertTxOutput ::
  GYTxOutRef ->
  TxOutput ->
  Either String GYUTxO
convertTxOutput ref txOutput = do
  address' <- convertAddress (txOutput ^. address)

  value <- convertTxOutputValue txOutput

  outDatum <-
    maybe
      (Right GYOutDatumNone)
      convertDatum
      (txOutput ^. Cardano_Fields.maybe'datum)

  refScript <-
    maybe
      (Right Nothing)
      (fmap Just . convertScript)
      (txOutput ^. maybe'script)

  pure $
    GYUTxO
      { utxoRef = ref
      , utxoAddress = address'
      , utxoValue = value
      , utxoOutDatum = outDatum
      , utxoRefScript = refScript
      }

--------------------------------------------------------------------------------
-- Datum conversion
--------------------------------------------------------------------------------

convertDatum :: Datum -> Either String GYOutDatum
convertDatum datum =
  case datum ^. Cardano_Fields.maybe'payload of
    Nothing -> do
      datumHash <-
        first
          (\err -> "UTxO-RPC datum hash decode failed: " <> show err)
          (Api.deserialiseFromRawBytes
            (Api.AsHash Api.AsScriptData)
            (datum ^. Cardano_Fields.hash))

      pure $ GYOutDatumHash (datumHashFromApi datumHash)

    Just _payload -> do
      hashableScriptData <-
        first
          (\err -> "UTxO-RPC datum CBOR decode failed: " <> show err)
          (Api.deserialiseFromCBOR
            Api.AsHashableScriptData
            (datum ^. Cardano_Fields.originalCbor))

      pure $
        GYOutDatumInline $
          datumFromApi' hashableScriptData

--------------------------------------------------------------------------------
-- Script conversion
--------------------------------------------------------------------------------

convertScript :: Script -> Either String GYAnyScript
convertScript script = do
  script' <-
    maybe
      (Left "UTxO-RPC script has no script value")
      Right
      (script ^. maybe'script)

  case script' of
    Script'PlutusV1 bytes ->
      pure $
        GYPlutusScript $
          scriptFromSerialisedScript @'PlutusV1 $
            SBS.toShort bytes

    Script'PlutusV2 bytes ->
      pure $
        GYPlutusScript $
          scriptFromSerialisedScript @'PlutusV2 $
            SBS.toShort bytes

    Script'PlutusV3 bytes ->
      pure $
        GYPlutusScript $
          scriptFromSerialisedScript @'PlutusV3 $
            SBS.toShort bytes

    Script'PlutusV4 _ ->
      Left "UTxO-RPC Plutus V4 reference script is not supported by Atlas"

    Script'Native nativeScript ->
      GYSimpleScript <$> convertNativeScript nativeScript

convertNativeScript :: NativeScript -> Either String GYSimpleScript
convertNativeScript nativeScript = do
  nativeScript' <-
    maybe
      (Left "UTxO-RPC native script has no native_script value")
      Right
      (nativeScript ^. maybe'nativeScript)

  case nativeScript' of
    NativeScript'ScriptPubkey bytes -> do
      keyHash <-
        first
          show
          (Api.deserialiseFromRawBytes (Api.AsHash Api.AsPaymentKey) bytes)

      pure $ RequireSignature $ paymentKeyHashFromApi keyHash

    NativeScript'ScriptAll scriptList ->
      RequireAllOf <$> convertNativeScriptList scriptList

    NativeScript'ScriptAny scriptList ->
      RequireAnyOf <$> convertNativeScriptList scriptList

    NativeScript'ScriptNOfK scriptNOfK ->
      convertScriptNOfK scriptNOfK

    NativeScript'InvalidBefore slot' ->
      pure $ RequireTimeAfter (slotFromWord64 slot')

    NativeScript'InvalidHereafter slot' ->
      pure $ RequireTimeBefore (slotFromWord64 slot')
  
convertNativeScriptList :: NativeScriptList -> Either String [GYSimpleScript]
convertNativeScriptList scriptList =
  traverse convertNativeScript (scriptList ^. Cardano_Fields.items)

convertScriptNOfK :: ScriptNOfK -> Either String GYSimpleScript
convertScriptNOfK scriptNOfK =
  RequireMOf
    (fromIntegral $ scriptNOfK ^. k)
    <$> traverse convertNativeScript
      (scriptNOfK ^. scripts)

--------------------------------------------------------------------------------
-- SearchUtxos
--------------------------------------------------------------------------------

utxoRpcSearchUtxos ::
  UtxoRpc ->
  Connection ->
  ProtoCardano.TxOutputPattern ->
  IO GYUTxOs
utxoRpcSearchUtxos _provider conn pattern' = do
  response <-
    nonStreaming
      conn
      (rpcWith @(Protobuf QueryService "searchUtxos") def)
      (Proto request)

  let responseItems =
        getProto response ^. Query_Fields.items

  case traverse convertItem responseItems of
    Left err ->
      fail $
        "UTxO-RPC searchUtxos conversion failed: "
          <> err

    Right converted ->
      pure $ utxosFromList converted

  where
    request :: SearchUtxosRequest
    request =
      defMessage
        & Query_Fields.maybe'predicate .~ Just
            ( defMessage
                & Query_Fields.maybe'match .~ Just
                    ( defMessage
                        & Query_Fields.maybe'utxoPattern .~ Just
                            (AnyUtxoPattern'Cardano pattern')
                    )
            )

    convertItem :: AnyUtxoData -> Either String GYUTxO
    convertItem item = do
      txoRef <-
        maybe
          (Left "UTxO-RPC searchUtxos item has no txo_ref")
          Right
          (item ^. maybe'txoRef)

      ref <-
        convertTxoRef txoRef

      parsedState <-
        maybe
          (Left "UTxO-RPC searchUtxos item has no parsed_state")
          Right
          (item ^. maybe'parsedState)

      case parsedState of
        AnyUtxoData'Cardano txOutput ->
          convertTxOutput ref txOutput

utxoRpcQueryAddress ::
  UtxoRpc ->
  GYAddress ->
  Maybe GYAssetClass ->
  IO GYUTxOs
utxoRpcQueryAddress provider address' assetClass = do
  pattern' <-
    case assetClass of
      Nothing ->
        pure $
          defMessage
            & Cardano_Fields.maybe'address .~ Just
                ( defMessage
                    & Cardano_Fields.exactAddress .~ convertAddressToBytes address'
                )

      Just assetClass' ->
        pure $
          defMessage
            & Cardano_Fields.maybe'address .~ Just
                ( defMessage
                    & Cardano_Fields.exactAddress .~ convertAddressToBytes address'
            )
            & Cardano_Fields.maybe'asset .~ Just
                (convertAssetPattern assetClass')

  withUtxoRpcConnection
    (utxoRpcConfig provider)
    $ \conn ->
      utxoRpcSearchUtxos provider conn pattern'

utxoRpcQueryAsset :: UtxoRpc -> GYNonAdaToken -> IO GYUTxOs 
utxoRpcQueryAsset provider (GYNonAdaToken policyId' tokenName) = 
  withUtxoRpcConnection (utxoRpcConfig provider) $ \conn -> 
    utxoRpcSearchUtxos provider conn 
      ( defMessage & Cardano_Fields.maybe'asset .~ Just 
        ( defMessage & Cardano_Fields.policyId .~ Api.serialiseToRawBytes (mintingPolicyIdToApi policyId') & 
              Cardano_Fields.assetName .~ Api.serialiseToRawBytes (tokenNameToApi tokenName) ) )

utxoRpcQueryPaymentCredential ::
  UtxoRpc ->
  GYPaymentCredential ->
  Maybe GYAssetClass ->
  IO GYUTxOs
utxoRpcQueryPaymentCredential provider credential assetClass = do
  paymentPart' <-
    case convertPaymentCredential credential of
      Left err ->
        fail err

      Right value ->
        pure value

  let addressPattern =
        defMessage
          & Cardano_Fields.paymentPart .~ paymentPart'

      pattern' =
        case assetClass of
          Nothing ->
            defMessage
              & Cardano_Fields.maybe'address .~ Just addressPattern

          Just assetClass' ->
            defMessage
              & Cardano_Fields.maybe'address .~ Just addressPattern
              & Cardano_Fields.maybe'asset .~ Just
                  (convertAssetPattern assetClass')

  withUtxoRpcConnection
    (utxoRpcConfig provider)
    $ \conn ->
      utxoRpcSearchUtxos provider conn pattern'

convertAssetPattern ::
  GYAssetClass ->
  ProtoCardano.AssetPattern
convertAssetPattern assetClass =
  case assetClass of
    GYLovelace ->
      error "UTxO-RPC: AssetPattern cannot represent lovelace"

    GYToken policyId' tokenName ->
      defMessage
        & Cardano_Fields.policyId .~
            Api.serialiseToRawBytes
              (mintingPolicyIdToApi policyId')
        & Cardano_Fields.assetName .~
            ( Api.serialiseToRawBytes
                (tokenNameToApi tokenName)
            )

convertPaymentCredential ::
  GYPaymentCredential ->
  Either String ByteString
convertPaymentCredential credential =
  case credential of
    GYPaymentCredentialByKey keyHash ->
      pure $
        Api.serialiseToRawBytes
          (paymentKeyHashToApi keyHash)

    GYPaymentCredentialByScript scriptHash' ->
      pure $
        Api.serialiseToRawBytes
          (scriptHashToApi scriptHash')