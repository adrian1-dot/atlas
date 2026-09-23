{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GeniusYield.GYConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.GYConfig (
  GYCoreConfig (..),
  Confidential (..),
  GYCoreProviderInfo (..),
  GYUtxoRpcRetryConfig (..),
  withCfgProviders,
  coreConfigIO,
  coreProviderIO,
  findMaestroTokenAndNetId,
  isNodeKupo,
  isOgmiosKupo,
  isMaestro,
  isBlockfrost,
  isUtxoRpc,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Network.GRPC.Client (Timeout (..), TimeoutUnit (..), TimeoutValue (TimeoutValue), ReconnectPolicy (..), ReconnectDecision (..), Reconnect (..), ReconnectTo (..))
import Network.GRPC.Common (HTTP2Settings (..), defaultHTTP2Settings)
import System.Random (randomRIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Text qualified as Text
import Data.Time (
  NominalDiffTime,
  diffUTCTime,
  getCurrentTime,
 )

import Cardano.Api qualified as Api

import GeniusYield.Imports
import GeniusYield.Providers.Blockfrost qualified as Blockfrost

-- import qualified GeniusYield.Providers.CachedQueryUTxOs as CachedQuery

import Data.Sequence qualified as Seq
import GeniusYield.Providers.CacheLocal
import GeniusYield.Providers.CacheMempool (augmentQueryUTxOWithMempool)
import GeniusYield.Providers.Common (mainnetEraHist, mainnetPlutusV3CostModel, preprodEraHist, preprodPlutusV3CostModel, previewEraHist)
import GeniusYield.Providers.Kupo qualified as KupoApi
import GeniusYield.Providers.Maestro qualified as MaestroApi
import GeniusYield.Providers.Node (nodeGetDRepState, nodeGetDRepsState, nodeStakeAddressInfo)
import GeniusYield.Providers.Node qualified as Node
import GeniusYield.Providers.Ogmios qualified as OgmiosApi
import GeniusYield.Providers.UtxoRpc qualified as UtxoRpcApi
import GeniusYield.ReadJSON (readJSON)
import GeniusYield.Types

-- | How many seconds to keep slots cached, before refetching the data.
slotCachingTime :: NominalDiffTime
slotCachingTime = 5

{- | Era history for the 'GYUtxoRpc' provider, keyed by network.

UTxO-RPC's @ReadEraSummary@ (as served by Dolos) only reflects whatever
era-boundary records the backing node has itself locally processed since it
started tracking chain state -- it does not reconstruct the full historical
era table the way a full node or Blockfrost's @/network/eras@ does (Dolos's
own miniBF @/network/eras@ route performs this reconstruction; its
gRPC/UTxO-RPC and Ogmios interfaces do not). Atlas' era interpreter is a
fixed 7-slot (Byron..Conway) structure with no partial form, so a live
response with fewer summaries can never be parsed into one. We supply the
network's own permanent historical boundaries instead of asking the backing
node for them; see 'GeniusYield.Providers.Common.preprodEraHist' et al.

__NOTE:__ must be updated on the next hardfork, same as the fixtures it
draws on. 'GYPrivnet' has no fixed history to hardcode and is unsupported by
this provider for that reason.
-}
utxoRpcNetworkEraHistory :: GYNetworkId -> IO Api.EraHistory
utxoRpcNetworkEraHistory GYMainnet = pure $ Api.EraHistory mainnetEraHist
utxoRpcNetworkEraHistory GYTestnetPreprod = pure $ Api.EraHistory preprodEraHist
utxoRpcNetworkEraHistory GYTestnetPreview = pure $ Api.EraHistory previewEraHist
utxoRpcNetworkEraHistory GYTestnetLegacy =
  throwIO $
    userError
      "UTxO-RPC: no hardcoded era history available for GYTestnetLegacy; \
      \the UtxoRpc provider only supports GYMainnet / GYTestnetPreprod / GYTestnetPreview"
utxoRpcNetworkEraHistory (GYPrivnet _) =
  throwIO $
    userError
      "UTxO-RPC: no hardcoded era history available for a private network (GYPrivnet); \
      \the UtxoRpc provider only supports GYMainnet / GYTestnetPreprod / GYTestnetPreview"

{- | PlutusV3 cost models for the 'GYUtxoRpc' provider, keyed by network.

Dolos derives "effective" cost models for the current epoch from the wrong
protocol state, on both its minibf REST and gRPC/UTxO-RPC interfaces (same
underlying bug either way) -- see
'GeniusYield.Providers.Common.preprodPlutusV3CostModel' and upstream
<https://github.com/txpipe/dolos/issues/1274 dolos#1274>. We supply the
network's current PlutusV3 cost model instead of asking the backing node
for it, same rationale as 'utxoRpcNetworkEraHistory' above.

__NOTE:__ must be updated on the next hard fork that changes the PlutusV3
cost model. 'GYPrivnet'/'GYTestnetLegacy' have no fixed model to hardcode
and are unsupported by this provider, same as era history.
-}
utxoRpcNetworkPlutusV3CostModel :: GYNetworkId -> IO [Integer]
utxoRpcNetworkPlutusV3CostModel GYMainnet = pure mainnetPlutusV3CostModel
utxoRpcNetworkPlutusV3CostModel GYTestnetPreprod = pure preprodPlutusV3CostModel
utxoRpcNetworkPlutusV3CostModel GYTestnetPreview =
  throwIO $
    userError
      "UTxO-RPC: no hardcoded PlutusV3 cost model available for GYTestnetPreview yet; \
      \the UtxoRpc provider only supports GYMainnet / GYTestnetPreprod for cost models"
utxoRpcNetworkPlutusV3CostModel GYTestnetLegacy =
  throwIO $
    userError
      "UTxO-RPC: no hardcoded PlutusV3 cost model available for GYTestnetLegacy; \
      \the UtxoRpc provider only supports GYMainnet / GYTestnetPreprod for cost models"
utxoRpcNetworkPlutusV3CostModel (GYPrivnet _) =
  throwIO $
    userError
      "UTxO-RPC: no hardcoded PlutusV3 cost model available for a private network (GYPrivnet); \
      \the UtxoRpc provider only supports GYMainnet / GYTestnetPreprod for cost models"

-- | Newtype with a custom show instance that prevents showing the contained data.
newtype Confidential a = Confidential a
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

instance Show (Confidential a) where
  showsPrec _ _ = showString "<Confidential>"

newtype MempoolCacheSettings = MempoolCacheSettings
  { mcsCacheInterval :: NominalDiffTime
  }
  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''MempoolCacheSettings
 )

newtype LocalTxSubmissionCacheSettings = LocalTxSubmissionCacheSettings
  { lcsCacheInterval :: NominalDiffTime
  }
  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''LocalTxSubmissionCacheSettings
 )

-- | Reconnect/timeout tuning for the 'GYUtxoRpc' connection. Plain numeric
-- fields (not the raw grapesy 'ReconnectPolicy'/'Timeout' types) so this can
-- ride along on 'GYCoreProviderInfo's auto-derived 'FromJSON' -- a
-- 'ReconnectPolicy' embeds an 'IO' action and can't be JSON-derived.
--
-- The resulting policy grows the delay exponentially from
-- (urrcDelayLoSec, urrcDelayHiSec) by urrcExponent each attempt, capped at
-- urrcDelayCapSec, and retries indefinitely at the capped rate rather than
-- giving up after a fixed attempt count. Matches the established pattern in
-- this org for Dolos/UTxO-RPC consumers with no reliable orchestrator
-- restart backstop -- see andamioscan's watcher.go (issue #78: capped, not
-- attempt-limited, "giving up would silently stop indexing") and
-- andamio-sponsorship-sidecar's background-consolidation backoff (60s cap,
-- never exits on exhaustion). See
-- 'GeniusYield.Providers.UtxoRpc.exponentialBackoff' if you specifically
-- want the bounded-attempts version instead (build 'UtxoRpcConfig' directly).
data GYUtxoRpcRetryConfig = GYUtxoRpcRetryConfig
  { urrcExponent :: !Double
  , urrcDelayLoSec :: !Double
  , urrcDelayHiSec :: !Double
  , urrcDelayCapSec :: !Double
  , urrcTimeoutSec :: !Word
  , urrcKeepAlivePingIntervalSec :: !(Maybe Double)
  -- ^ HTTP/2 keepalive PING interval. 'Nothing' disables it (grapesy default).
  , urrcIdleTimeoutSec :: !(Maybe Double)
  -- ^ Idle-connection timeout override (backstop for the ping above).
  -- 'Nothing' keeps grapesy's default (30s).
  }
  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 4 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''GYUtxoRpcRetryConfig
 )

-- | Exponential backoff that grows from @(lo, hi)@ by @exponent@ each
-- attempt, clamped to @capSec@, and retries indefinitely at the capped rate
-- -- unlike 'GeniusYield.Providers.UtxoRpc.exponentialBackoff', this never
-- reaches 'DontReconnect'. Suitable for a long-lived connection that should
-- keep trying to recover from an outage of any length, not just a handful
-- of attempts.
cappedIndefiniteBackoff ::
     (Int -> IO ())
  -> Double
  -> (Double, Double)
  -> Double
  -> ReconnectPolicy
cappedIndefiniteBackoff waitFor e = go
  where
    go :: (Double, Double) -> Double -> ReconnectPolicy
    go (lo, hi) capSec = ReconnectPolicy $ do
      delay <- randomRIO (lo, hi)
      waitFor $ round $ delay * 1_000_000
      pure $ DoReconnect Reconnect
        { reconnectTo = ReconnectToOriginal
        , onReconnect = Nothing
        , nextPolicy = go (min capSec (lo * e), min capSec (hi * e)) capSec
        }

-- | Seconds to microseconds, for grapesy's 'HTTP2Settings'.
secondsToMicros :: Double -> Int
secondsToMicros = round . (* 1_000_000)

{- |
The supported providers. The options are:

- Local node.socket along with Kupo
- Ogmios node instance along with Kupo
- Maestro blockchain API, provided its API token.
- Blockfrost API, provided its API key.
- Custom Blockfrost instance (e.g., self-hosted), with optional API key.

In JSON format, this essentially corresponds to:

= { socketPath: FilePath, kupoUrl: string, mempoolCache: { cacheInterval: number }, localTxSubmissionCache: { cacheInterval: number } }
| { ogmiosUrl: string, kupoUrl: string, mempoolCache: { cacheInterval: number }, localTxSubmissionCache: { cacheInterval: number } }
| { maestroToken: string, turboSubmit: boolean }
| { blockfrostKey: string }
| { blockfrostUrl: string, maybeBlockfrostKey?: string }

The constructor tags don't need to appear in the JSON.
-}
data GYCoreProviderInfo
  = GYNodeKupo {cpiSocketPath :: !FilePath, cpiKupoUrl :: !Text, cpiMempoolCache :: !(Maybe MempoolCacheSettings), cpiLocalTxSubmissionCache :: !(Maybe LocalTxSubmissionCacheSettings)}
  | GYOgmiosKupo {cpiOgmiosUrl :: !Text, cpiKupoUrl :: !Text, cpiMempoolCache :: !(Maybe MempoolCacheSettings), cpiLocalTxSubmissionCache :: !(Maybe LocalTxSubmissionCacheSettings)}
  | GYMaestro {cpiMaestroToken :: !(Confidential Text), cpiTurboSubmit :: !(Maybe Bool)}
  | GYBlockfrost {cpiBlockfrostKey :: !(Confidential Text)}
  | GYBlockfrostCustom {cpiBlockfrostUrl :: !Text, cpiMaybeBlockfrostKey :: !(Maybe (Confidential Text))}
  | GYUtxoRpc {cpiUtxoRpcHost :: !Text, cpiUtxoRpcPort :: !Int, cpiUtxoRpcUseTls :: !Bool, cpiUtxoRpcRetry :: !(Maybe GYUtxoRpcRetryConfig)}
  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''GYCoreProviderInfo
 )

coreProviderIO :: FilePath -> IO GYCoreProviderInfo
coreProviderIO = readJSON

isNodeKupo :: GYCoreProviderInfo -> Bool
isNodeKupo GYNodeKupo {} = True
isNodeKupo _ = False

isOgmiosKupo :: GYCoreProviderInfo -> Bool
isOgmiosKupo GYOgmiosKupo {} = True
isOgmiosKupo _ = False

isMaestro :: GYCoreProviderInfo -> Bool
isMaestro GYMaestro {} = True
isMaestro _ = False

isBlockfrost :: GYCoreProviderInfo -> Bool
isBlockfrost GYBlockfrost {} = True
isBlockfrost GYBlockfrostCustom {} = True
isBlockfrost _ = False

isUtxoRpc :: GYCoreProviderInfo -> Bool
isUtxoRpc GYUtxoRpc {} = True
isUtxoRpc _ = False

findMaestroTokenAndNetId :: [GYCoreConfig] -> IO (Text, GYNetworkId)
findMaestroTokenAndNetId configs = do
  let config = find (isMaestro . cfgCoreProvider) configs
  case config of
    Nothing -> throwIO $ userError "Missing Maestro Configuration"
    Just conf -> do
      let netId = cfgNetworkId conf
      case cfgCoreProvider conf of
        GYMaestro (Confidential token) _ -> return (token, netId)
        _ -> throwIO $ userError "Missing Maestro Token"

{- |
The config to initialize the GY framework with.
Should include information on the providers to use, as well as the network id.

In JSON format, this essentially corresponds to:

= { coreProvider: GYCoreProviderInfo, networkId: NetworkId, logging: [GYLogScribeConfig], utxoCacheEnable: boolean }
-}
data GYCoreConfig = GYCoreConfig
  { cfgCoreProvider :: !GYCoreProviderInfo
  , cfgNetworkId :: !GYNetworkId
  , cfgLogging :: ![GYLogScribeConfig]
  -- ^ List of scribes to register.
  , cfgLogTiming :: !(Maybe Bool)
  -- ^ Optional switch to enable timing and logging of requests sent to provider.
  }
  -- , cfgUtxoCacheEnable :: !Bool

  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      }
    ''GYCoreConfig
 )

coreConfigIO :: FilePath -> IO GYCoreConfig
coreConfigIO file = do
  bs <- LBS.readFile file
  case Aeson.eitherDecode' bs of
    Left err -> throwIO $ userError err
    Right cfg -> pure cfg

nodeConnectInfo :: FilePath -> GYNetworkId -> Api.LocalNodeConnectInfo
nodeConnectInfo path netId = Node.networkIdToLocalNodeConnectInfo netId path

withCfgProviders :: GYCoreConfig -> GYLogNamespace -> (GYProviders -> IO a) -> IO a
withCfgProviders
  GYCoreConfig
    { cfgCoreProvider
    , cfgNetworkId
    , cfgLogging
    , cfgLogTiming
    }
  ns
  f =
    case cfgCoreProvider of
      GYNodeKupo path kupoUrl mmempoolCache mlocalTxSubCache -> do
        -- YOUR EXISTING GYNodeKupo BRANCH, UNCHANGED
        (gyGetParameters, gySlotActions', gyQueryUTxO', gyLookupDatum, gySubmitTxConfirmed, gyAwaitTxConfirmed, gyGetStakeAddressInfo, gyGetDRepState, gyGetDRepsState, gyGetStakePools, gyGetConstitution, gyGetProposals, gyGetMempoolTxs) <-
          do
            let info = nodeConnectInfo path cfgNetworkId
            kEnv <- KupoApi.newKupoApiEnv $ Text.unpack kupoUrl

            nodeSlotActions <-
              makeSlotActions
                slotCachingTime
                (Node.nodeGetSlotOfCurrentBlock info)

            nodeGetParams <-
              Node.nodeGetParameters info

            queryUtxo <- case mmempoolCache of
              Nothing ->
                pure $ KupoApi.kupoQueryUtxo kEnv
              Just (MempoolCacheSettings cacheInterval) ->
                augmentQueryUTxOWithMempool
                  (KupoApi.kupoQueryUtxo kEnv)
                  (Node.nodeMempoolTxs info)
                  cacheInterval

            (queryUtxo', submitTx) <- case mlocalTxSubCache of
              Nothing ->
                pure
                  ( queryUtxo
                  , Node.nodeSubmitTx info
                  )
              Just (LocalTxSubmissionCacheSettings cacheInterval) -> do
                locallySubmittedTxsVar <-
                  mkLocallySubmittedTxsVar cacheInterval

                let augmentedSubmitTx =
                      augmentTxSubmission
                        (Node.nodeSubmitTx info)
                        locallySubmittedTxsVar

                pure
                  ( augmentQueryUTxOWithLocalSubmission
                      queryUtxo
                      locallySubmittedTxsVar
                  , augmentedSubmitTx
                  )

            pure
              ( nodeGetParams
              , nodeSlotActions
              , queryUtxo'
              , KupoApi.kupoLookupDatum kEnv
              , submitTx
              , KupoApi.kupoAwaitTxConfirmed kEnv
              , nodeStakeAddressInfo info
              , nodeGetDRepState info
              , nodeGetDRepsState info
              , Node.nodeStakePools info
              , Node.nodeConstitution info
              , Node.nodeProposals info
              , Node.nodeMempoolTxs info
              )

        runProviders
          gyGetParameters
          gySlotActions'
          gyQueryUTxO'
          gyLookupDatum
          gySubmitTxConfirmed
          gyAwaitTxConfirmed
          gyGetStakeAddressInfo
          gyGetDRepState
          gyGetDRepsState
          gyGetStakePools
          gyGetConstitution
          gyGetProposals
          gyGetMempoolTxs

      GYOgmiosKupo ogmiosUrl kupoUrl mmempoolCache mlocalTxSubCache -> do
        -- YOUR EXISTING GYOgmiosKupo BRANCH, UNCHANGED
        (gyGetParameters, gySlotActions', gyQueryUTxO', gyLookupDatum, gySubmitTxConfirmed, gyAwaitTxConfirmed, gyGetStakeAddressInfo, gyGetDRepState, gyGetDRepsState, gyGetStakePools, gyGetConstitution, gyGetProposals, gyGetMempoolTxs) <-
          do
            oEnv <- OgmiosApi.newOgmiosApiEnv $ Text.unpack ogmiosUrl
            kEnv <- KupoApi.newKupoApiEnv $ Text.unpack kupoUrl
            ogmiosSlotActions <- makeSlotActions slotCachingTime $ OgmiosApi.ogmiosGetSlotOfCurrentBlock oEnv
            ogmiosGetParams <-
              makeGetParameters
                (OgmiosApi.ogmiosProtocolParameters oEnv)
                (OgmiosApi.ogmiosStartTime oEnv)
                (OgmiosApi.ogmiosEraSummaries oEnv)
                (OgmiosApi.ogmiosGetSlotOfCurrentBlock oEnv)
            queryUtxo <- case mmempoolCache of
              Nothing -> pure $ KupoApi.kupoQueryUtxo kEnv
              Just (MempoolCacheSettings cacheInterval) -> do
                augmentQueryUTxOWithMempool (KupoApi.kupoQueryUtxo kEnv) (OgmiosApi.ogmiosMempoolTxsWs oEnv) cacheInterval
            (queryUtxo', submitTx) <- case mlocalTxSubCache of
              Nothing -> pure (queryUtxo, OgmiosApi.ogmiosSubmitTx oEnv)
              Just (LocalTxSubmissionCacheSettings cacheInterval) -> do
                locallySubmittedTxsVar <- mkLocallySubmittedTxsVar cacheInterval
                let augmentedSubmitTx = augmentTxSubmission (OgmiosApi.ogmiosSubmitTx oEnv) locallySubmittedTxsVar
                pure (augmentQueryUTxOWithLocalSubmission queryUtxo locallySubmittedTxsVar, augmentedSubmitTx)
            pure
              ( ogmiosGetParams
              , ogmiosSlotActions
              , queryUtxo'
              , KupoApi.kupoLookupDatum kEnv
              , submitTx
              , KupoApi.kupoAwaitTxConfirmed kEnv
              , OgmiosApi.ogmiosStakeAddressInfo oEnv
              , OgmiosApi.ogmiosGetDRepState oEnv
              , OgmiosApi.ogmiosGetDRepsState oEnv
              , OgmiosApi.ogmiosStakePools oEnv
              , OgmiosApi.ogmiosConstitution oEnv
              , OgmiosApi.ogmiosProposals oEnv
              , OgmiosApi.ogmiosMempoolTxsWs oEnv
              )
        runProviders
          gyGetParameters
          gySlotActions'
          gyQueryUTxO'
          gyLookupDatum
          gySubmitTxConfirmed
          gyAwaitTxConfirmed
          gyGetStakeAddressInfo
          gyGetDRepState
          gyGetDRepsState
          gyGetStakePools
          gyGetConstitution
          gyGetProposals
          gyGetMempoolTxs

      GYMaestro (Confidential apiToken) turboSubmit -> do
        -- YOUR EXISTING GYMaestro BRANCH, UNCHANGED
        (gyGetParameters, gySlotActions', gyQueryUTxO', gyLookupDatum, gySubmitTxConfirmed, gyAwaitTxConfirmed, gyGetStakeAddressInfo, gyGetDRepState, gyGetDRepsState, gyGetStakePools, gyGetConstitution, gyGetProposals, gyGetMempoolTxs) <-
          do
            maestroApiEnv <- MaestroApi.networkIdToMaestroEnv apiToken cfgNetworkId
            maestroSlotActions <- makeSlotActions slotCachingTime $ MaestroApi.maestroGetSlotOfCurrentBlock maestroApiEnv
            maestroGetParams <-
              makeGetParameters
                (MaestroApi.maestroProtocolParams maestroApiEnv)
                (MaestroApi.maestroSystemStart maestroApiEnv)
                (MaestroApi.maestroEraHistory maestroApiEnv)
                (MaestroApi.maestroGetSlotOfCurrentBlock maestroApiEnv)
            pure
              ( maestroGetParams
              , maestroSlotActions
              , MaestroApi.maestroQueryUtxo maestroApiEnv
              , MaestroApi.maestroLookupDatum maestroApiEnv
              , MaestroApi.maestroSubmitTx (Just True == turboSubmit) maestroApiEnv
              , MaestroApi.maestroAwaitTxConfirmed maestroApiEnv
              , MaestroApi.maestroStakeAddressInfo maestroApiEnv
              , MaestroApi.maestroDRepState maestroApiEnv
              , MaestroApi.maestroDRepsState maestroApiEnv
              , MaestroApi.maestroStakePools maestroApiEnv
              , MaestroApi.maestroConstitution maestroApiEnv
              , MaestroApi.maestroProposals maestroApiEnv
              , MaestroApi.maestroMempoolTxs maestroApiEnv
              )
        runProviders
          gyGetParameters
          gySlotActions'
          gyQueryUTxO'
          gyLookupDatum
          gySubmitTxConfirmed
          gyAwaitTxConfirmed
          gyGetStakeAddressInfo
          gyGetDRepState
          gyGetDRepsState
          gyGetStakePools
          gyGetConstitution
          gyGetProposals
          gyGetMempoolTxs

      GYBlockfrost (Confidential key) -> do
        -- YOUR EXISTING GYBlockfrost BRANCH, UNCHANGED
        (gyGetParameters, gySlotActions', gyQueryUTxO', gyLookupDatum, gySubmitTxConfirmed, gyAwaitTxConfirmed, gyGetStakeAddressInfo, gyGetDRepState, gyGetDRepsState, gyGetStakePools, gyGetConstitution, gyGetProposals, gyGetMempoolTxs) <-
            do
            let proj = Blockfrost.networkIdToProject cfgNetworkId key
            blockfrostSlotActions <- makeSlotActions slotCachingTime $ Blockfrost.blockfrostGetSlotOfCurrentBlock proj
            blockfrostGetParams <-
              makeGetParameters
                (Blockfrost.blockfrostProtocolParams proj)
                (Blockfrost.blockfrostSystemStart proj)
                (Blockfrost.blockfrostEraHistory proj)
                (Blockfrost.blockfrostGetSlotOfCurrentBlock proj)
            pure
              ( blockfrostGetParams
              , blockfrostSlotActions
              , Blockfrost.blockfrostQueryUtxo proj
              , Blockfrost.blockfrostLookupDatum proj
              , Blockfrost.blockfrostSubmitTx proj
              , Blockfrost.blockfrostAwaitTxConfirmed proj
              , Blockfrost.blockfrostStakeAddressInfo proj
              , Blockfrost.blockfrostDRepState proj
              , Blockfrost.blockfrostDRepsState proj
              , Blockfrost.blockfrostStakePools proj
              , Blockfrost.blockfrostConstitution proj
              , Blockfrost.blockfrostProposals proj
              , Blockfrost.blockfrostMempoolTxs proj
              )

        runProviders
          gyGetParameters
          gySlotActions'
          gyQueryUTxO'
          gyLookupDatum
          gySubmitTxConfirmed
          gyAwaitTxConfirmed
          gyGetStakeAddressInfo
          gyGetDRepState
          gyGetDRepsState
          gyGetStakePools
          gyGetConstitution
          gyGetProposals
          gyGetMempoolTxs

      GYBlockfrostCustom url mkey -> do
        -- YOUR EXISTING GYBlockfrostCustom BRANCH.
        -- DO NOT CHANGE THIS BRANCH.
        (gyGetParameters, gySlotActions', gyQueryUTxO', gyLookupDatum, gySubmitTxConfirmed, gyAwaitTxConfirmed, gyGetStakeAddressInfo, gyGetDRepState, gyGetDRepsState, gyGetStakePools, gyGetConstitution, gyGetProposals, gyGetMempoolTxs) <-
          do
            let key = maybe "" id $ coerce mkey
                proj = Blockfrost.networkIdToProjectCustom url key
            blockfrostSlotActions <- makeSlotActions slotCachingTime $ Blockfrost.blockfrostGetSlotOfCurrentBlock proj
            blockfrostGetParams <-
              makeGetParameters
                (Blockfrost.blockfrostProtocolParams proj)
                (Blockfrost.blockfrostSystemStart proj)
                (Blockfrost.blockfrostEraHistory proj)
                (Blockfrost.blockfrostGetSlotOfCurrentBlock proj)
            pure
              ( blockfrostGetParams
              , blockfrostSlotActions
              , Blockfrost.blockfrostQueryUtxo proj
              , Blockfrost.blockfrostLookupDatum proj
              , Blockfrost.blockfrostSubmitTx proj
              , Blockfrost.blockfrostAwaitTxConfirmed proj
              , Blockfrost.blockfrostStakeAddressInfo proj
              , Blockfrost.blockfrostDRepState proj
              , Blockfrost.blockfrostDRepsState proj
              , Blockfrost.blockfrostStakePools proj
              , Blockfrost.blockfrostConstitution proj
              , Blockfrost.blockfrostProposals proj
              , Blockfrost.blockfrostMempoolTxs proj
              )
        runProviders
          gyGetParameters
          gySlotActions'
          gyQueryUTxO'
          gyLookupDatum
          gySubmitTxConfirmed
          gyAwaitTxConfirmed
          gyGetStakeAddressInfo
          gyGetDRepState
          gyGetDRepsState
          gyGetStakePools
          gyGetConstitution
          gyGetProposals
          gyGetMempoolTxs

      GYUtxoRpc cpiUtxoRpcHost cpiUtxoRpcPort cpiUtxoRpcUseTls cpiUtxoRpcRetry -> do
        -- cpiUtxoRpcRetry is Nothing => grapesy's own defaults (no reconnect,
        -- no timeout), matching prior behaviour. The integrator (whoever
        -- constructs GYUtxoRpc) decides the curve, not this module.
        let urconf = case cpiUtxoRpcRetry of
              Nothing -> UtxoRpcApi.defaultUtxoRpcConfig (Text.unpack cpiUtxoRpcHost) cpiUtxoRpcPort cpiUtxoRpcUseTls slotCachingTime
              Just GYUtxoRpcRetryConfig{urrcExponent, urrcDelayLoSec, urrcDelayHiSec, urrcDelayCapSec, urrcTimeoutSec, urrcKeepAlivePingIntervalSec, urrcIdleTimeoutSec} ->
                (UtxoRpcApi.defaultUtxoRpcConfig (Text.unpack cpiUtxoRpcHost) cpiUtxoRpcPort cpiUtxoRpcUseTls slotCachingTime)
                  { UtxoRpcApi.utxoRpcReconnectPolicy = cappedIndefiniteBackoff threadDelay urrcExponent (urrcDelayLoSec, urrcDelayHiSec) urrcDelayCapSec
                  , UtxoRpcApi.utxoRpcDefaultTimeout = Just (Timeout Second (TimeoutValue urrcTimeoutSec))
                  , UtxoRpcApi.utxoRpcHTTP2Settings =
                      defaultHTTP2Settings
                        { http2ClientKeepAlivePingInterval = secondsToMicros <$> urrcKeepAlivePingIntervalSec
                        , http2ClientIdleTimeout = secondsToMicros <$> urrcIdleTimeoutSec
                        }
                  }
        provider <- UtxoRpcApi.mkUtxoRpc urconf
        eraHistory <- utxoRpcNetworkEraHistory cfgNetworkId
        plutusV3CostModel <- utxoRpcNetworkPlutusV3CostModel cfgNetworkId

        UtxoRpcApi.withUtxoRpcConnection urconf $ \conn -> do
          gySlotActions' <-
            UtxoRpcApi.utxoRpcSlotActions
              provider
              conn

          gyGetParameters <-
            UtxoRpcApi.utxoRpcGetParameters
              provider
              eraHistory
              plutusV3CostModel

          runProviders
            gyGetParameters
            gySlotActions'
            (UtxoRpcApi.utxoRpcQueryUtxo provider conn)
            (UtxoRpcApi.utxoRpcLookupDatum provider conn)
            (UtxoRpcApi.utxoRpcSubmitTx provider conn)
            (UtxoRpcApi.utxoRpcAwaitTxConfirmed provider conn)
            -- The next four are not stubs pending implementation -- UTxO-RPC
            -- has no RPC for any of them. 'Certificate'/'DRep'/
            -- 'PoolRegistrationCert'/'GovernanceActionProposal' exist only as
            -- shapes embedded in a transaction body (what a tx *did*), never
            -- as queryable current ledger state. Reconstructing them would
            -- mean building and running a persistent chain-indexer on top of
            -- 'dumpHistory'/'watchTx', out of scope for a provider module;
            -- stake-address reward-account balance is a ledger-computed
            -- value that can't be reconstructed from on-chain events at all.
            -- Checked exhaustively against every Request/Response message in
            -- 'SyncService'/'QueryService'/'SubmitService'/'WatchService' --
            -- none of them return this state.
            (\_ ->
              error "UTxO-RPC: stake address info not implemented -- no RPC exposes reward-account state")
            (\_ ->
              error "UTxO-RPC: DRep state not implemented -- no RPC exposes current DRep registry")
            (\_ ->
              error "UTxO-RPC: DRep states not implemented -- no RPC exposes current DRep registry")
            (pure (error "UTxO-RPC: stake pools not implemented -- no RPC exposes the pool registry"))
            (UtxoRpcApi.utxoRpcGetConstitution provider)
            (\_ ->
              error "UTxO-RPC: governance proposals not implemented -- no RPC exposes active proposals")
            (UtxoRpcApi.utxoRpcGetMempoolTxs provider conn)

  where
    runProviders
      gyGetParameters
      gySlotActions'
      gyQueryUTxO'
      gyLookupDatum
      gySubmitTx
      gyAwaitTxConfirmed
      gyGetStakeAddressInfo
      gyGetDRepState
      gyGetDRepsState
      gyGetStakePools
      gyGetConstitution
      gyGetProposals
      gyGetMempoolTxs =
      bracket (mkLogEnv ns cfgLogging) closeScribes $ \logEnv -> do
        let gyLog' =
              GYLogConfiguration
                { cfgLogNamespace = mempty
                , cfgLogContexts = mempty
                , cfgLogDirector = Left logEnv
                }

        (gyQueryUTxO, gySlotActions) <-
          pure
            ( gyQueryUTxO'
            , gySlotActions'
            )

        let f' =
              maybe
                f
                (\case
                    True -> f . logTiming
                    False -> f
                )
                cfgLogTiming

        e <- try $ f' GYProviders {..}

        case e of
          Right a ->
            pure a
          Left (err :: SomeException) -> do
            logRun
              gyLog'
              GYError
              ((printf "ERROR: %s" $ show err) :: String)
            throwIO err

logTiming :: GYProviders -> GYProviders
logTiming providers@GYProviders {..} =
  GYProviders
    { gyLookupDatum = gyLookupDatum'
    , gySubmitTx = gySubmitTx'
    , gyAwaitTxConfirmed = gyAwaitTxConfirmed'
    , gySlotActions = gySlotActions'
    , gyGetParameters = gyGetParameters'
    , gyQueryUTxO = gyQueryUTxO'
    , gyGetStakeAddressInfo = gyGetStakeAddressInfo'
    , gyGetDRepState = gyGetDRepState'
    , gyGetDRepsState = gyGetDRepsState'
    , gyLog' = gyLog'
    , gyGetStakePools = gyGetStakePools'
    , gyGetConstitution = gyGetConstitution'
    , gyGetProposals = gyGetProposals'
    , gyGetMempoolTxs = gyGetMempoolTxs'
    }
 where
  wrap :: String -> IO a -> IO a
  wrap msg m = do
    (!a, !t) <- duration m
    gyLog providers "" GYDebug $ msg <> " took " <> show t
    pure a

  gyLookupDatum' :: GYLookupDatum
  gyLookupDatum' = wrap "gyLookupDatum" . gyLookupDatum

  gySubmitTx' :: GYSubmitTx
  gySubmitTx' = wrap "gySubmitTx" . gySubmitTx

  gyAwaitTxConfirmed' :: GYAwaitTx
  gyAwaitTxConfirmed' p = wrap "gyAwaitTxConfirmed" . gyAwaitTxConfirmed p

  gySlotActions' :: GYSlotActions
  gySlotActions' =
    GYSlotActions
      { gyGetSlotOfCurrentBlock' = wrap "gyGetSlotOfCurrentBlock" $ gyGetSlotOfCurrentBlock providers
      , gyWaitForNextBlock' = wrap "gyWaitForNextBlock" $ gyWaitForNextBlock providers
      , gyWaitUntilSlot' = wrap "gyWaitUntilSlot" . gyWaitUntilSlot providers
      }

  gyGetParameters' :: GYGetParameters
  gyGetParameters' =
    GYGetParameters
      { gyGetProtocolParameters' = wrap "gyGetProtocolParameters" $ gyGetProtocolParameters providers
      , gyGetSystemStart' = wrap "gyGetSystemStart" $ gyGetSystemStart providers
      , gyGetEraHistory' = wrap "gyGetEraHistory" $ gyGetEraHistory providers
      , gyGetSlotConfig' = wrap "gyGetSlotConfig" $ gyGetSlotConfig providers
      }

  gyGetStakePools' = wrap "gyGetStakePools" gyGetStakePools

  gyQueryUTxO' :: GYQueryUTxO
  gyQueryUTxO' =
    GYQueryUTxO
      { gyQueryUtxosAtTxOutRefs' = wrap "gyQueryUtxosAtTxOutRefs" . gyQueryUtxosAtTxOutRefs providers
      , gyQueryUtxosAtTxOutRefsWithDatums' = case gyQueryUtxosAtTxOutRefsWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ wrap "gyQueryUtxosAtTxOutRefsWithDatums" . q
      , gyQueryUtxoAtTxOutRef' = wrap "gyQueryUtxoAtTxOutRef" . gyQueryUtxoAtTxOutRef providers
      , gyQueryUtxoRefsAtAddress' = wrap "gyQueryUtxoRefsAtAddress" . gyQueryUtxoRefsAtAddress providers
      , gyQueryUtxosAtAddress' = \addr mac -> wrap "gyQueryUtxosAtAddress'" $ gyQueryUtxosAtAddress providers addr mac
      , gyQueryUtxosWithAsset' = wrap "gyQueryUtxosWithAsset'" . gyQueryUtxosWithAsset providers
      , gyQueryUtxosAtAddressWithDatums' = case gyQueryUtxosAtAddressWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ \addr mac -> wrap "gyQueryUtxosAtAddressWithDatums'" $ q addr mac
      , gyQueryUtxosAtAddresses' = wrap "gyQueryUtxosAtAddresses" . gyQueryUtxosAtAddresses providers
      , gyQueryUtxosAtAddressesWithDatums' = case gyQueryUtxosAtAddressesWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ wrap "gyQueryUtxosAtAddressesWithDatums" . q
      , gyQueryUtxosAtPaymentCredential' = \cred -> wrap "gyQueryUtxosAtPaymentCredential" . gyQueryUtxosAtPaymentCredential providers cred
      , gyQueryUtxosAtPaymentCredWithDatums' = case gyQueryUtxosAtPaymentCredWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ \cred mac -> wrap "gyQueryUtxosAtPaymentCredWithDatums" $ q cred mac
      , gyQueryUtxosAtPaymentCredentials' = wrap "gyQueryUtxosAtPaymentCredentials" . gyQueryUtxosAtPaymentCredentials providers
      , gyQueryUtxosAtPaymentCredsWithDatums' = case gyQueryUtxosAtPaymentCredsWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ wrap "gyQueryUtxosAtPaymentCredsWithDatums" . q
      }

  gyGetStakeAddressInfo' :: GYStakeAddress -> IO (Maybe GYStakeAddressInfo)
  gyGetStakeAddressInfo' = wrap "gyGetStakeAddressInfo" . gyGetStakeAddressInfo

  gyGetDRepState' :: GYCredential 'GYKeyRoleDRep -> IO (Maybe GYDRepState)
  gyGetDRepState' = wrap "gyGetDRepState" . gyGetDRepState

  gyGetDRepsState' :: Set (GYCredential 'GYKeyRoleDRep) -> IO (Map (GYCredential 'GYKeyRoleDRep) (Maybe GYDRepState))
  gyGetDRepsState' = wrap "gyGetDRepsState" . gyGetDRepsState

  gyGetConstitution' :: IO GYConstitution
  gyGetConstitution' = wrap "gyGetConstitution" gyGetConstitution

  gyGetProposals' :: Set GYGovActionId -> IO (Seq.Seq GYGovActionState)
  gyGetProposals' = wrap "gyGetProposals" . gyGetProposals

  gyGetMempoolTxs' = wrap "gyGetMempoolTxs" gyGetMempoolTxs

duration :: IO a -> IO (a, NominalDiffTime)
duration m = do
  start <- getCurrentTime
  a <- m
  end <- getCurrentTime
  pure (a, end `diffUTCTime` start)
