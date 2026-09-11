{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GeniusYield.Providers.Common
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.Common (
  SomeDeserializeError (..),
  SubmitTxException (..),
  datumFromCBOR,
  newServantClientEnv,
  fromJson,
  makeLastEraEndUnbounded,
  parseEraHist,
  preprodEraHist,
  previewEraHist,
  mainnetEraHist,
  preprodPlutusV3CostModel,
  mainnetPlutusV3CostModel,
  silenceHeadersClientError,
  extractAssetClass,
  extractNonAdaToken,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTLS
import PlutusTx (
  FromData,
  fromData,
 )
import Servant.Client qualified as Servant
import Servant.Client.Core qualified as Servant

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api
import Cardano.Slotting.Slot (
  EpochNo (..),
  EpochSize (..),
 )
import Cardano.Slotting.Time (
  RelativeTime (RelativeTime),
  mkSlotLength,
 )
import Control.Exception (Exception)
import Data.Bifunctor (first)
import Data.SOP.NonEmpty (NonEmpty (NonEmptyCons, NonEmptyOne), nonEmptyFromList, nonEmptyToList)
import GeniusYield.CardanoApi.EraHistory (extractEraSummaries)
import GeniusYield.Types.Datum (
  GYDatum,
  datumFromApi',
 )
import GeniusYield.Types.Script (mintingPolicyIdToText)
import GeniusYield.Types.Value (
  GYAssetClass (..),
  GYNonAdaToken (..),
  nonAdaTokenFromAssetClass,
  tokenNameToHex,
 )
import Ouroboros.Consensus.Cardano.Block qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History.EraParams (EraParams (eraGenesisWin))

deriving newtype instance Num EpochSize
deriving newtype instance Num EpochNo

data SomeDeserializeError
  = DeserializeErrorBech32 !Api.Bech32DecodeError
  | DeserializeErrorAeson !Text
  | DeserializeErrorAssetClass !Text
  | DeserializeErrorScriptDataJson !Api.ScriptDataJsonError
  | -- Api.RawBytesHexError isn't exported; use that if it gets exported
    -- https://github.com/input-output-hk/cardano-node/issues/4579
    DeserializeErrorHex !Text
  | DeserializeErrorAddress
  | DeserializeErrorImpossibleBranch !Text
  deriving stock (Eq, Show)

newtype SubmitTxException = SubmitTxException Text
  deriving stock Show
  deriving anyclass Exception

-- | Get datum from bytes.
datumFromCBOR :: Text -> Either SomeDeserializeError GYDatum
datumFromCBOR d = do
  bs <- fromEither $ BS16.decode $ Text.encodeUtf8 d
  api <- fromEither $ Api.deserialiseFromCBOR Api.AsHashableScriptData bs
  return $ datumFromApi' api
 where
  e = DeserializeErrorHex d

  fromEither :: Either e a -> Either SomeDeserializeError a
  fromEither = first $ const e

{- | Remove request headers info from returned ClientError.

This is used as quick and simple way to hide confidential information such as API token.
-}
silenceHeadersClientError :: Servant.ClientError -> Servant.ClientError
silenceHeadersClientError (Servant.FailureResponse reqF res) = Servant.FailureResponse reqF {Servant.requestHeaders = mempty} res
silenceHeadersClientError other = other

-- | Creates a new Servant 'Servant.ClientEnv' from a base url.
newServantClientEnv :: String -> IO Servant.ClientEnv
newServantClientEnv baseUrl = do
  url <- Servant.parseBaseUrl baseUrl
  manager <-
    if Servant.baseUrlScheme url == Servant.Https
      then HttpClient.newManager HttpClientTLS.tlsManagerSettings
      else HttpClient.newManager HttpClient.defaultManagerSettings
  pure $ Servant.mkClientEnv manager url

fromJson :: FromData a => LBS.ByteString -> Either SomeDeserializeError a
fromJson b = do
  v <- first (DeserializeErrorAeson . Text.pack) $ Aeson.eitherDecode b
  x <- first DeserializeErrorScriptDataJson $ Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema v
  pure . fromJust . fromData $ Api.toPlutusData $ Api.getScriptData x

makeLastEraEndUnbounded :: Api.EraHistory -> Api.EraHistory
makeLastEraEndUnbounded eh =
  let Ouroboros.Summary eraList = extractEraSummaries eh
   in Api.EraHistory $ Ouroboros.mkInterpreter $ Ouroboros.Summary $ g eraList
 where
  g eraList =
    let eraList' = nonEmptyToList eraList
        f [] = []
        f [x] =
          let oldEraParams = Ouroboros.eraParams x
           in [x {Ouroboros.eraEnd = Ouroboros.EraUnbounded, Ouroboros.eraParams = oldEraParams {Ouroboros.eraSafeZone = Ouroboros.UnsafeIndefiniteSafeZone}}]
        f (x : xs) = x : f xs
     in fromJust $ nonEmptyFromList $ f eraList'

{- | Convert a regular list of era summaries (a la Ogmios) into a typed EraHistory (a la Ouroboros).

== NOTE ==

TODO: This must be updated with each hardfork.

See 'Ouroboros.NonEmptyCons'/'Ouroboros.Summary'/'Ouroboros.CardanoEras' types to understand
why one cannot trivially automate this.

Well, unless one uses vectors, from dependent type land.
-}
parseEraHist :: (t -> Ouroboros.EraSummary) -> [t] -> Maybe Api.EraHistory
parseEraHist mkEra [byronEra, shelleyEra, allegraEra, maryEra, alonzoEra, babbageEra, conwayEra] =
  Just
    . makeLastEraEndUnbounded
    . Api.EraHistory
    . Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons (mkEra byronEra)
    . NonEmptyCons (mkEra shelleyEra)
    . NonEmptyCons (mkEra allegraEra)
    . NonEmptyCons (mkEra maryEra)
    . NonEmptyCons (mkEra alonzoEra)
    . NonEmptyCons (mkEra babbageEra)
    $ NonEmptyOne (mkEra conwayEra)
parseEraHist _ _ = Nothing

{- | Hardcoded era history for preprod.

__NOTE:__ This is only to be used for testing.

Also see: "GeniusYield.CardanoApi.EraHistory.showEraHistory"
-}
preprodEraHist :: Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
preprodEraHist =
  Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons byronEra
    . NonEmptyCons shelleyEra
    . NonEmptyCons allegraEra
    . NonEmptyCons maryEra
    . NonEmptyCons alonzoEra
    . NonEmptyCons babbageEra
    $ NonEmptyOne conwayEra
 where
  byronEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 1728000, boundSlot = 86400, boundEpoch = 4})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 21600, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 4320, eraGenesisWin = 4320}
      }
  shelleyEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 1728000, boundSlot = 86400, boundEpoch = 4}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 2160000, boundSlot = 518400, boundEpoch = 5})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  allegraEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 2160000, boundSlot = 518400, boundEpoch = 5}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 2592000, boundSlot = 950400, boundEpoch = 6})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  maryEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 2592000, boundSlot = 950400, boundEpoch = 6}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 3024000, boundSlot = 1382400, boundEpoch = 7})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  alonzoEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 3024000, boundSlot = 1382400, boundEpoch = 7}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 5184000, boundSlot = 3542400, boundEpoch = 12})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  babbageEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 5184000, boundSlot = 3542400, boundEpoch = 12}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 70416000, boundSlot = 68774400, boundEpoch = 163})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  conwayEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 70416000, boundSlot = 68774400, boundEpoch = 163}
      , eraEnd = Ouroboros.EraUnbounded
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }

previewEraHist :: Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
previewEraHist =
  Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons byronEra
    . NonEmptyCons shelleyEra
    . NonEmptyCons allegraEra
    . NonEmptyCons maryEra
    . NonEmptyCons alonzoEra
    . NonEmptyCons babbageEra
    $ NonEmptyOne conwayEra
 where
  byronEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 4320, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 864, eraGenesisWin = 864}
      }
  shelleyEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  allegraEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  maryEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  alonzoEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 259200, boundSlot = 259200, boundEpoch = 3})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  babbageEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 259200, boundSlot = 259200, boundEpoch = 3}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 55814400, boundSlot = 55814400, boundEpoch = 646})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  conwayEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 55814400, boundSlot = 55814400, boundEpoch = 646}
      , eraEnd = Ouroboros.EraUnbounded
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }

mainnetEraHist :: Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
mainnetEraHist =
  Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons byronEra
    . NonEmptyCons shelleyEra
    . NonEmptyCons allegraEra
    . NonEmptyCons maryEra
    . NonEmptyCons alonzoEra
    . NonEmptyCons babbageEra
    $ NonEmptyOne conwayEra
 where
  byronEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 89856000, boundSlot = 4492800, boundEpoch = 208})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 21600, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 4320, eraGenesisWin = 4320}
      }
  shelleyEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 89856000, boundSlot = 4492800, boundEpoch = 208}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 101952000, boundSlot = 16588800, boundEpoch = 236})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  allegraEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 101952000, boundSlot = 16588800, boundEpoch = 236}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 108432000, boundSlot = 23068800, boundEpoch = 251})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  maryEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 108432000, boundSlot = 23068800, boundEpoch = 251}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 125280000, boundSlot = 39916800, boundEpoch = 290})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  alonzoEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 125280000, boundSlot = 39916800, boundEpoch = 290}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 157680000, boundSlot = 72316800, boundEpoch = 365})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  babbageEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 157680000, boundSlot = 72316800, boundEpoch = 365}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 219024000, boundSlot = 133660800, boundEpoch = 507})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  conwayEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 219024000, boundSlot = 133660800, boundEpoch = 507}
      , eraEnd = Ouroboros.EraUnbounded
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }

{- | Hardcoded PlutusV3 cost-model parameter list for preprod, as protocol
version 11 currently defines it (350 values).

Dolos derives the "effective" cost models for the current epoch from the
wrong protocol state on both its minibf REST and gRPC/UTxO-RPC interfaces --
same underlying bug on both, see 'GeniusYield.Providers.UtxoRpc' module note
and 'TODO.md'. Measured 2026-09-11: Dolos gRPC (\"ReadParams\") returned the
genesis-baked 251-parameter, protocol-version-10 model; Dolos minibf
returned a third, still-wrong 297-parameter answer; only the real network
(cross-checked via Maestro and Koios) returns the correct 350-parameter,
protocol-version-11 model below. See upstream
<https://github.com/txpipe/dolos/issues/1274 dolos#1274> (open, no linked
fix as of 2026-09-11).

PlutusV1/V2 are left sourced live from Dolos for now -- not confirmed
affected by the same bug, unlike V3.

__NOTE:__ must be updated on the next hard fork that changes the PlutusV3
cost model, same discipline as the era-history tables above. Remove this
override (and 'mainnetPlutusV3CostModel', which is byte-identical --
verified 2026-09-11 against Koios mainnet @epoch_params@) once
@dolos#1274@ ships and is released.
-}
preprodPlutusV3CostModel :: [Integer]
preprodPlutusV3CostModel =
  [ 100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957,
    4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000,
    100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100,
    100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010,
    178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848,
    123203, 7305, -900, 1716, 960, 57, 85848, 0, 1, 1,
    1000, 42921, 4, 2, 30623, 28755, 75, 1, 898148, 27279,
    1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32,
    83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10,
    28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749,
    541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32,
    7391, 32, 11546, 32, 85848, 123203, 7305, -900, 1716, 960,
    57, 85848, 0, 1, 90434, 519, 0, 1, 74433, 32,
    85848, 123203, 7305, -900, 1716, 960, 57, 85848, 0, 1,
    1, 85848, 123203, 7305, -900, 1716, 960, 57, 85848, 0,
    1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566,
    4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1,
    1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744,
    32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10,
    43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678,
    6, 442008, 1, 52538055, 3756, 18, 267929, 18, 76433006, 8868,
    18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917843,
    4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36, 333849714,
    1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310,
    4, 1293828, 28716, 63, 0, 1, 1006041, 43623, 251, 0,
    1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0,
    1, 100181, 726, 719, 0, 1, 107878, 680, 0, 1,
    95336, 1, 281145, 18848, 0, 1, 180194, 159, 1, 1,
    158519, 8942, 0, 1, 159378, 8813, 0, 1, 107490, 3298,
    1, 106057, 655, 1, 1964219, 24520, 3, 607153, 231697, 53144,
    0, 1, 116711, 1957, 4, 231883, 10, 1000, 24838, 7,
    1, 232010, 32, 321837444, 25087669, 18, 617887431, 67302824, 36, 356924,
    18413, 45, 21, 219951, 9444, 1, 1000, 172116, 183150, 6,
    24, 21, 213283, 618401, 1998, 28258, 1, 1000, 38159, 2,
    22, 1000, 95933, 1, 1, 11, 1000, 277577, 12, 21
  ]

-- | Hardcoded PlutusV3 cost-model parameter list for mainnet. See 'preprodPlutusV3CostModel' --
-- byte-identical, since PlutusV3 cost models are consensus-level constants shared across networks
-- at the same protocol version (both mainnet and preprod are at protocol version 11 as of 2026-09-11).
mainnetPlutusV3CostModel :: [Integer]
mainnetPlutusV3CostModel = preprodPlutusV3CostModel

-- | Extract currency symbol & token name part of an `GYAssetClass` when it is of such a form. When input is @Just GYLovelace@ or @Nothing@, this function returns @Nothing@.
extractAssetClass :: Maybe GYAssetClass -> Maybe (Text, Text)
extractAssetClass mac = do
  ac <- mac
  nat <- nonAdaTokenFromAssetClass ac
  pure $ extractNonAdaToken nat

-- | Extract currency symbol & token name part of a `GYNonAdaToken`.
extractNonAdaToken :: GYNonAdaToken -> (Text, Text)
extractNonAdaToken (GYNonAdaToken pid tn) = (mintingPolicyIdToText pid, tokenNameToHex tn)
