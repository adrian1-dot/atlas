module GeniusYield.Providers.UtxoRpc (
  UtxoRpcConfig (..),
  defaultUtxoRpcConfig,
  exponentialBackoff,
  UtxoRpc,
  mkUtxoRpc,
  utxoRpcSlotActions,
  utxoRpcGetParameters,
  utxoRpcQueryUtxo,
  utxoRpcGetSlotOfCurrentBlock,
  utxoRpcLookupDatum,
  utxoRpcSubmitTx,
  utxoRpcAwaitTxConfirmed,
  utxoRpcGetMempoolTxs,
  utxoRpcGetConstitution,
  withUtxoRpcConnection,
  UtxoRpcConn,

  -- * Pure conversion helpers (exposed for unit testing)
  convertAddress,
  convertAddressToBytes,
  convertBigInt,
  convertMultiasset,
  convertAsset,
  convertTxOutputValue,
  convertTxOutRef,
  convertTxoRef,
  convertTxOutput,
  convertDatum,
  convertScript,
  convertNativeScript,
  convertNativeScriptList,
  convertScriptNOfK,
  convertPParams,
  convertPoolVotingThresholds,
  convertDRepVotingThresholds
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
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Control.Concurrent (threadDelay, MVar, newMVar, readMVar, takeMVar, putMVar)
import Control.Exception (throwIO, catch, finally)

import Proto.Utxorpc.V1alpha.Query.Query
import Proto.Utxorpc.V1alpha.Query.Query_Fields (hash, index , keys, maybe'parsedState, maybe'txoRef, maybe'params, maybe'values, nativeBytes, values)
import qualified Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields as Cardano_Fields

import Proto.Utxorpc.V1alpha.Sync.Sync
import Proto.Utxorpc.V1alpha.Sync.Sync_Fields qualified as Sync_Fields (maybe'tip, slot)
import Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields (address, maybe'bigInt, scripts, maybe'script, maybe'nativeScript,
                                                    maybe'bigInt, assets, coin, name, outputCoin, policyId, k,
                                                    maybe'anchor, url, contentHash, constitution)
import Proto.Utxorpc.V1alpha.Cardano.Cardano (Asset, BigInt, BigInt'BigInt (..), Multiasset, TxOutput, NativeScript,
                                             Datum, ScriptNOfK, Script'Script (..), NativeScript'NativeScript (..),
                                             Script, NativeScriptList)

-- Imported qualified: 'Proto.Utxorpc.V1alpha.Query.Query' (open above) also
-- defines an unrelated 'AnyChainTx' (a fetched tx + its block reference), so
-- an open import here would make the type name ambiguous at every use site.
import Proto.Utxorpc.V1alpha.Submit.Submit qualified as ProtoSubmit
import Proto.Utxorpc.V1alpha.Submit.Submit_Fields qualified as Submit_Fields

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

type instance RequestMetadata (Protobuf QueryService "readData") = NoMetadata
type instance ResponseInitialMetadata (Protobuf QueryService "readData") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf QueryService "readData") = NoMetadata

type instance RequestMetadata (Protobuf ProtoSubmit.SubmitService "submitTx") = NoMetadata
type instance ResponseInitialMetadata (Protobuf ProtoSubmit.SubmitService "submitTx") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf ProtoSubmit.SubmitService "submitTx") = NoMetadata

type instance RequestMetadata (Protobuf ProtoSubmit.SubmitService "waitForTx") = NoMetadata
type instance ResponseInitialMetadata (Protobuf ProtoSubmit.SubmitService "waitForTx") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf ProtoSubmit.SubmitService "waitForTx") = NoMetadata

type instance RequestMetadata (Protobuf ProtoSubmit.SubmitService "readMempool") = NoMetadata
type instance ResponseInitialMetadata (Protobuf ProtoSubmit.SubmitService "readMempool") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf ProtoSubmit.SubmitService "readMempool") = NoMetadata

utxoRpcGetSlotOfCurrentBlock :: Maybe Timeout -> UtxoRpcConn -> IO GYSlot
utxoRpcGetSlotOfCurrentBlock timeout uc = do
  response <-
    utxoRpcCallWithReconnect uc $ \conn ->
      nonStreaming
        conn
        (rpcWith @(Protobuf SyncService "readTip") def{ callTimeout = timeout })
        (Proto (defMessage :: ReadTipRequest))

  case getProto response ^. Sync_Fields.maybe'tip of
    Nothing ->
      fail "UTxO-RPC ReadTipResponse did not contain a tip"
    Just blockRef ->
      pure $ slotFromWord64 (blockRef ^. Sync_Fields.slot)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | A UTxO-RPC connection that can rebuild itself.
--
-- Wraps grapesy's 'Connection' behind an 'MVar' instead of handing out the
-- raw immutable value. grapesy's own reconnect machinery
-- ('connReconnectPolicy'\/'stayConnected') only ever re-triggers when a
-- /new/ connection attempt fails -- it never notices an established
-- connection dying silently between calls (no keepalive/ping exists in
-- grapesy as of 1.2.0; see
-- <https://github.com/well-typed/grapesy/issues/228>). When that happens,
-- the wedged call fails fast via 'callTimeout'
-- ('GrpcException' with 'GrpcDeadlineExceeded'), and
-- 'utxoRpcCallWithReconnect' below explicitly closes the dead connection
-- and opens a fresh one, so the /next/ call gets a working connection
-- instead of hanging forever on the same dead one. The failing call itself
-- is not retried -- see 'utxoRpcCallWithReconnect'.
data UtxoRpcConn = UtxoRpcConn
  { ucConnVar :: !(MVar Connection)
  , ucConnParams :: !ConnParams
  , ucServer :: !Server
  }

-- | Run a UTxO-RPC call against the current connection, rebuilding it if the
-- call fails with 'GrpcDeadlineExceeded'.
--
-- __NOTE:__ the /failing/ call is not retried against the rebuilt
-- connection -- it still fails, exactly as it did before this fix, just
-- without leaving the connection wedged for good. Only the /next/ call
-- benefits from the fresh connection. This is a deliberate scope decision,
-- not an oversight: transparent retry would need every call site to be
-- safely re-driveable (submitTx in particular is not obviously safe to
-- silently retry), so it is left for a future pass if needed.
--
-- Concurrent failures on the same dead connection are not deduplicated:
-- two calls failing at nearly the same time may both close the old
-- connection and open a new one, wasting one redial. Accepted for now --
-- see the 'MVar' comment on 'ucConnVar' if this needs tightening later.
utxoRpcCallWithReconnect :: UtxoRpcConn -> (Connection -> IO a) -> IO a
utxoRpcCallWithReconnect uc action = do
  conn <- readMVar (ucConnVar uc)
  action conn `catch` \e -> do
    case e of
      GrpcException{grpcError = GrpcDeadlineExceeded} ->
        rebuildUtxoRpcConn uc
      _ ->
        pure ()
    throwIO e

-- | Close the current (dead) connection and open a fresh one in its place.
--
-- Uses the 'MVar' itself as the single-flight lock: whoever is between
-- 'takeMVar' and 'putMVar' here is the only thread doing the rebuild;
-- everyone else (a concurrent failing call, or a normal 'readMVar' caller)
-- just blocks until it is done.
rebuildUtxoRpcConn :: UtxoRpcConn -> IO ()
rebuildUtxoRpcConn uc = do
  old <- takeMVar (ucConnVar uc)
  closeConnection old
  new <- openConnection (ucConnParams uc) (ucServer uc)
  putMVar (ucConnVar uc) new

withUtxoRpcConnection :: UtxoRpcConfig -> (UtxoRpcConn -> IO a) -> IO a
withUtxoRpcConnection config action = do
  initial <- openConnection connParams server
  connVar <- newMVar initial
  let uc = UtxoRpcConn { ucConnVar = connVar, ucConnParams = connParams, ucServer = server }
  action uc `finally` (readMVar connVar >>= closeConnection)
  where
    connParams =
      def
        { connReconnectPolicy = utxoRpcReconnectPolicy config
        , connDefaultTimeout = utxoRpcDefaultTimeout config
        , connHTTP2Settings = utxoRpcHTTP2Settings config
        }

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
-- The endpoint is deliberately provider-neutral. Dolos is one possible
-- implementation of the UTxO-RPC server, but this provider targets the
-- UTxO-RPC protocol, not Dolos specifically.
--
-- The gRPC connection fields ('utxoRpcReconnectPolicy',
-- 'utxoRpcDefaultTimeout', 'utxoRpcHTTP2Settings') are required, not
-- defaulted, so callers must consciously decide them. Use
-- 'defaultUtxoRpcConfig' to fall back to grapesy's own defaults (no
-- reconnect, no timeout, default HTTP/2 tuning).
data UtxoRpcConfig = UtxoRpcConfig
  { utxoRpcHost :: !String
  , utxoRpcPort :: !Int
  , utxoRpcUseTls :: !Bool
  , utxoRpcSlotCacheTime :: !NominalDiffTime
  , utxoRpcReconnectPolicy :: !ReconnectPolicy
  -- ^ Reconnect behaviour on a lost/failed connection. Use
  -- 'exponentialBackoff' for retries, or build a custom 'ReconnectPolicy'
  -- via @grapesy@ (@Network.GRPC.Client@) directly -- not re-exported here.
  , utxoRpcDefaultTimeout :: !(Maybe Timeout)
  -- ^ Per-call timeout. 'Nothing' means calls can hang forever on a wedged
  -- connection, which also blocks 'utxoRpcReconnectPolicy' from ever
  -- triggering. Build a 'Timeout' via @grapesy@ directly.
  , utxoRpcHTTP2Settings :: !HTTP2Settings
  -- ^ HTTP/2 tuning (window sizes, @TCP_NODELAY@, frame rate limits).
  -- Build via @grapesy@ (@Network.GRPC.Common@) directly.
  }

-- | 'UtxoRpcConfig' with grapesy's defaults: no reconnect, no timeout,
-- default HTTP/2 tuning. Override via record update, e.g.:
--
-- > (defaultUtxoRpcConfig host port useTls slotCacheTime)
-- >   { utxoRpcReconnectPolicy = exponentialBackoff threadDelay 1.5 (0.5, 2.0) 10 }
defaultUtxoRpcConfig :: String -> Int -> Bool -> NominalDiffTime -> UtxoRpcConfig
defaultUtxoRpcConfig host port useTls slotCacheTime =
  UtxoRpcConfig
    { utxoRpcHost = host
    , utxoRpcPort = port
    , utxoRpcUseTls = useTls
    , utxoRpcSlotCacheTime = slotCacheTime
    , utxoRpcReconnectPolicy = def
    , utxoRpcDefaultTimeout = Nothing
    , utxoRpcHTTP2Settings = def
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

utxoRpcReadGenesis :: Maybe Timeout -> UtxoRpcConn -> IO Genesis
utxoRpcReadGenesis timeout uc = do
  response <-
    utxoRpcCallWithReconnect uc $ \conn ->
      nonStreaming
        conn
        (rpcWith @(Protobuf QueryService "readGenesis") def{ callTimeout = timeout })
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
    genesis <- utxoRpcReadGenesis (utxoRpcDefaultTimeout $ utxoRpcConfig provider) conn
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
     genesis <- utxoRpcReadGenesis (utxoRpcDefaultTimeout $ utxoRpcConfig provider) conn
     genesisWin <- either fail pure (computeGenesisWindow genesis)

     response <-
       nonStreaming
         conn
         (rpcWith @(Protobuf QueryService "readEraSummary") def{ callTimeout = utxoRpcDefaultTimeout (utxoRpcConfig provider) })
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
  UtxoRpcConn ->
  IO GYSlotActions
utxoRpcSlotActions provider conn =
  makeSlotActions
    (utxoRpcSlotCacheTime $ utxoRpcConfig provider)
    (utxoRpcGetSlotOfCurrentBlock (utxoRpcDefaultTimeout $ utxoRpcConfig provider) conn)

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
--
-- The PlutusV3 cost model is likewise not sourced from UTxO-RPC's live
-- response -- Dolos derives "effective" cost models for the current epoch
-- from the wrong protocol state on both its minibf REST and gRPC
-- interfaces (see 'convertCostModels' below and
-- <https://github.com/txpipe/dolos/issues/1274 dolos#1274>); the caller
-- supplies the correct current-network value instead
-- ('GeniusYield.GYConfig.utxoRpcNetworkPlutusV3CostModel').
utxoRpcGetParameters ::
  UtxoRpc ->
  Api.EraHistory ->
  [Integer] ->
  IO GYGetParameters
utxoRpcGetParameters provider eraHistory plutusV3CostModel =
  makeGetParameters
    (utxoRpcReadParams provider plutusV3CostModel)
    (utxoRpcSystemStart provider)
    (pure eraHistory)
    (withUtxoRpcConnection (utxoRpcConfig provider) (utxoRpcGetSlotOfCurrentBlock (utxoRpcDefaultTimeout $ utxoRpcConfig provider)))

utxoRpcReadParams :: UtxoRpc -> [Integer] -> IO ApiProtocolParameters
utxoRpcReadParams provider plutusV3CostModel =
  withUtxoRpcConnection
    (utxoRpcConfig provider)
    $ \conn -> do
      response <-
        utxoRpcCallWithReconnect conn $ \c ->
          nonStreaming
            c
            (rpcWith @(Protobuf QueryService "readParams") def{ callTimeout = utxoRpcDefaultTimeout (utxoRpcConfig provider) })
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
                  case convertPParams plutusV3CostModel pparams of
                    Left err ->
                      fail $
                        "UTxO-RPC protocol parameters conversion failed: "
                          <> err

                    Right result ->
                      pure result

convertPParams ::
  [Integer] ->
  ProtoCardano.PParams ->
  Either String ApiProtocolParameters
convertPParams plutusV3CostModel pparams = do
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
      plutusV3CostModel
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

-- | 'Maybe ProtoCardano.CostModels' -> the PlutusV1/V2 fields are still
-- sourced live from Dolos's response; PlutusV3 is overridden by the caller
-- (see 'utxoRpcGetParameters') since Dolos's derivation of it is wrong on
-- both its minibf and gRPC interfaces -- <https://github.com/txpipe/dolos/issues/1274 dolos#1274>.
convertCostModels ::
  [Integer] ->
  Maybe ProtoCardano.CostModels ->
  Either String LedgerPlutus.CostModels
convertCostModels plutusV3CostModel value =
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
        first show $
          LedgerPlutus.mkCostModel
            LedgerPlutus.PlutusV3
            (fromIntegral <$> plutusV3CostModel)

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
utxoRpcQueryUtxo :: UtxoRpc -> UtxoRpcConn -> GYQueryUTxO
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
            <$> utxoRpcQueryAddress provider conn address' Nothing

    , gyQueryUtxosAtAddress' =
        utxoRpcQueryAddress provider conn

    , gyQueryUtxosWithAsset' =
        utxoRpcQueryAsset provider conn

    , gyQueryUtxosAtAddressWithDatums' =
        Nothing

    , gyQueryUtxosAtAddresses' =
        \addresses ->
          mconcat
            <$> traverse
              (\address' ->
                utxoRpcQueryAddress provider conn address' Nothing)
              addresses

    , gyQueryUtxosAtAddressesWithDatums' =
        Nothing

    , gyQueryUtxosAtPaymentCredential' =
        utxoRpcQueryPaymentCredential provider conn

    , gyQueryUtxosAtPaymentCredWithDatums' =
        Nothing

    , gyQueryUtxosAtPaymentCredentials' =
        \credentials ->
          mconcat
            <$> traverse
              (\credential ->
                utxoRpcQueryPaymentCredential provider conn credential Nothing)
              credentials

    , gyQueryUtxosAtPaymentCredsWithDatums' =
        Nothing
    }

--------------------------------------------------------------------------------
-- ReadUtxos
--------------------------------------------------------------------------------

utxoRpcReadUtxos :: UtxoRpc -> UtxoRpcConn -> [GYTxOutRef] -> IO GYUTxOs
utxoRpcReadUtxos provider uc refs = do
  response <-
    utxoRpcCallWithReconnect uc $ \conn ->
      nonStreaming
        conn
        (rpcWith @(Protobuf QueryService "readUtxos") def{ callTimeout = utxoRpcDefaultTimeout (utxoRpcConfig provider) })
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
  UtxoRpcConn ->
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
    Nothing
      | BS.null (datum ^. Cardano_Fields.hash) ->
          -- Dolos always emits a Datum submessage, even for outputs with no
          -- datum at all (pallas-utxorpc's map_tx_datum sets hash=[], payload=None
          -- in that case). Empty hash + no payload means "no datum", not a genuine
          -- hash-only datum (a real hash is always 32 bytes) -- see TODO.md for the
          -- upstream Dolos fix to stop emitting this degenerate message.
          pure GYOutDatumNone
      | otherwise -> do
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
  UtxoRpcConn ->
  ProtoCardano.TxOutputPattern ->
  IO GYUTxOs
utxoRpcSearchUtxos provider uc pattern' = do
  response <-
    utxoRpcCallWithReconnect uc $ \conn ->
      nonStreaming
        conn
        (rpcWith @(Protobuf QueryService "searchUtxos") def{ callTimeout = utxoRpcDefaultTimeout (utxoRpcConfig provider) })
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
  UtxoRpcConn ->
  GYAddress ->
  Maybe GYAssetClass ->
  IO GYUTxOs
utxoRpcQueryAddress provider conn address' assetClass = do
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

  utxoRpcSearchUtxos provider conn pattern'

utxoRpcQueryAsset :: UtxoRpc -> UtxoRpcConn -> GYNonAdaToken -> IO GYUTxOs
utxoRpcQueryAsset provider conn (GYNonAdaToken policyId' tokenName) =
  utxoRpcSearchUtxos provider conn
    ( defMessage & Cardano_Fields.maybe'asset .~ Just
      ( defMessage & Cardano_Fields.policyId .~ Api.serialiseToRawBytes (mintingPolicyIdToApi policyId') &
            Cardano_Fields.assetName .~ Api.serialiseToRawBytes (tokenNameToApi tokenName) ) )

utxoRpcQueryPaymentCredential ::
  UtxoRpc ->
  UtxoRpcConn ->
  GYPaymentCredential ->
  Maybe GYAssetClass ->
  IO GYUTxOs
utxoRpcQueryPaymentCredential provider conn credential assetClass = do
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

--------------------------------------------------------------------------------
-- Datum lookup
--------------------------------------------------------------------------------

-- | Look up a datum by its hash via UTxO-RPC's ReadData.
utxoRpcLookupDatum :: UtxoRpc -> UtxoRpcConn -> GYLookupDatum
utxoRpcLookupDatum provider uc dh = do
  response <-
    utxoRpcCallWithReconnect uc $ \conn ->
      nonStreaming
        conn
        (rpcWith @(Protobuf QueryService "readData") def{ callTimeout = utxoRpcDefaultTimeout (utxoRpcConfig provider) })
        (Proto request)

  case getProto response ^. values of
    [] ->
      pure Nothing

    item : _ ->
      case convertAnyChainDatum item of
        Left err ->
          fail $ "UTxO-RPC readData conversion failed: " <> err

        Right d ->
          pure $ Just d

  where
    request :: ReadDataRequest
    request =
      defMessage
        & keys .~ [Api.serialiseToRawBytes (datumHashToApi dh)]

    convertAnyChainDatum :: AnyChainDatum -> Either String GYDatum
    convertAnyChainDatum item =
      first
        (\err -> "UTxO-RPC datum CBOR decode failed: " <> show err)
        (datumFromApi' <$> Api.deserialiseFromCBOR Api.AsHashableScriptData (item ^. nativeBytes))

--------------------------------------------------------------------------------
-- Submit tx
--------------------------------------------------------------------------------

-- | Submit a signed 'GYTx' via UTxO-RPC's SubmitTx.
utxoRpcSubmitTx :: UtxoRpc -> UtxoRpcConn -> GYSubmitTx
utxoRpcSubmitTx provider uc tx = do
  response <-
    utxoRpcCallWithReconnect uc $ \conn ->
      nonStreaming
        conn
        (rpcWith @(Protobuf ProtoSubmit.SubmitService "submitTx") def{ callTimeout = utxoRpcDefaultTimeout (utxoRpcConfig provider) })
        (Proto request)

  either
    (\err -> fail $ "UTxO-RPC submitTx returned an unparseable tx id: " <> err)
    pure
    (first show $ Api.deserialiseFromRawBytes Api.AsTxId (getProto response ^. Submit_Fields.ref))
    <&> txIdFromApi

  where
    request :: ProtoSubmit.SubmitTxRequest
    request =
      defMessage
        & Submit_Fields.tx .~
            ( defMessage
                & Submit_Fields.raw .~ Api.serialiseToCBOR (txToApi tx)
            )

--------------------------------------------------------------------------------
-- Await tx confirmation
--------------------------------------------------------------------------------

-- | Await confirmation of a submitted 'GYTxId' via UTxO-RPC's WaitForTx.
--
-- __NOTE:__ UTxO-RPC's 'WaitForTxResponse' only reports a coarse-grained
-- 'Stage' (acknowledged\/mempool\/network\/confirmed), not a block-depth
-- confirmation count. This provider treats 'STAGE_CONFIRMED' as satisfying
-- any requested 'confirmations' depth -- there is no way to distinguish "1
-- confirmation" from "N confirmations" over UTxO-RPC as it stands.
utxoRpcAwaitTxConfirmed :: UtxoRpc -> UtxoRpcConn -> GYAwaitTx
utxoRpcAwaitTxConfirmed _provider uc params@GYAwaitTxParameters {..} txId = do
  -- NOTE: deliberately not routed through 'utxoRpcCallWithReconnect' -- this
  -- call has no 'callTimeout' (see the note at its call site), so it can
  -- never observe 'GrpcDeadlineExceeded' to rebuild on. Parked pending a
  -- decision on a separate, longer timeout for this specifically
  -- long-running wait; see andamio-atlas-api-v2#81.
  conn <- readMVar (ucConnVar uc)
  serverStreaming
    conn
    (rpcWith @(Protobuf ProtoSubmit.SubmitService "waitForTx") def)
    (Proto request)
    (go 0)

  where
    request :: ProtoSubmit.WaitForTxRequest
    request =
      defMessage
        & Submit_Fields.ref .~ [Api.serialiseToRawBytes (txIdToApi txId)]

    go :: Int -> IO (NextElem (Proto ProtoSubmit.WaitForTxResponse)) -> IO ()
    go attempt recv
      | maxAttempts <= attempt =
          throwIO $ GYAwaitTxException params
      | otherwise = do
          next <- recv
          case next of
            NoNextElem ->
              threadDelay checkInterval >> go (attempt + 1) recv

            NextElem response
              | getProto response ^. Submit_Fields.stage == ProtoSubmit.STAGE_CONFIRMED ->
                  pure ()
              | otherwise ->
                  threadDelay checkInterval >> go (attempt + 1) recv

--------------------------------------------------------------------------------
-- Mempool
--------------------------------------------------------------------------------

-- | List the transactions currently sitting in the mempool, via UTxO-RPC's
-- ReadMempool.
utxoRpcGetMempoolTxs :: UtxoRpc -> UtxoRpcConn -> IO [GYTx]
utxoRpcGetMempoolTxs provider uc = do
  response <-
    utxoRpcCallWithReconnect uc $ \conn ->
      nonStreaming
        conn
        (rpcWith @(Protobuf ProtoSubmit.SubmitService "readMempool") def{ callTimeout = utxoRpcDefaultTimeout (utxoRpcConfig provider) })
        (Proto (defMessage :: ProtoSubmit.ReadMempoolRequest))

  let items =
        getProto response ^. Submit_Fields.items

  case traverse convertTxInMempool items of
    Left err ->
      fail $ "UTxO-RPC readMempool conversion failed: " <> err

    Right txs ->
      pure txs

  where
    convertTxInMempool :: ProtoSubmit.TxInMempool -> Either String GYTx
    convertTxInMempool item =
      first show $
        txFromCBOR (item ^. Submit_Fields.nativeBytes)

--------------------------------------------------------------------------------
-- Constitution
--------------------------------------------------------------------------------

-- | The on-chain constitution, sourced from UTxO-RPC's ReadGenesis.
utxoRpcGetConstitution :: UtxoRpc -> IO GYConstitution
utxoRpcGetConstitution provider =
  withUtxoRpcConnection (utxoRpcConfig provider) $ \conn -> do
    genesis <- utxoRpcReadGenesis (utxoRpcDefaultTimeout $ utxoRpcConfig provider) conn
    either fail pure (convertConstitution (genesis ^. constitution))

  where
    convertConstitution :: ProtoCardano.Constitution -> Either String GYConstitution
    convertConstitution constitution' = do
      anchor' <-
        maybe
          (Left "UTxO-RPC genesis constitution has no anchor")
          Right
          (constitution' ^. maybe'anchor)

      anchorUrl' <-
        maybe
          (Left "UTxO-RPC constitution anchor has an invalid URL")
          Right
          (textToUrl (anchor' ^. url))

      anchorDataHash' <-
        maybe
          (Left "UTxO-RPC constitution anchor has an invalid content hash")
          Right
          (anchorDataHashFromByteString (anchor' ^. contentHash))

      let scriptHash' =
            if BS.null (constitution' ^. hash)
              then Nothing
              else
                either
                  (const Nothing)
                  (Just . scriptHashFromApi)
                  (Api.deserialiseFromRawBytes Api.AsScriptHash (constitution' ^. hash))

      pure $
        GYConstitution
          { constitutionAnchor =
              GYAnchor anchorUrl' anchorDataHash'
          , constitutionScript =
              scriptHash'
          }