module GeniusYield.Test.Providers.UtxoRpc (
  utxoRpcProviderTests,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Short qualified as SBS
import Data.Maybe (fromJust)
import Data.Ratio (denominator, numerator, (%))
import Test.Tasty
import Test.Tasty.HUnit

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger

import Network.GRPC.Common.Protobuf

import Proto.Utxorpc.V1alpha.Cardano.Cardano qualified as PC
import Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields qualified as PCF

import GeniusYield.Providers.UtxoRpc
import GeniusYield.Types

utxoRpcProviderTests :: TestTree
utxoRpcProviderTests =
  testGroup
    "UtxoRpc"
    [ bigIntTests
    , addressTests
    , txOutRefTests
    , assetTests
    , datumTests
    , scriptTests
    , nativeScriptTests
    , txOutputTests
    , votingThresholdsTests
    ]

--------------------------------------------------------------------------------
-- BigInt
--------------------------------------------------------------------------------

bigIntTests :: TestTree
bigIntTests =
  testGroup
    "BigInt conversion"
    [ testCase "Int" $
        convertBigInt (mkBigInt $ PC.BigInt'Int (-42)) @?= Right (-42)
    , testCase "BigUInt" $
        convertBigInt (mkBigInt $ PC.BigInt'BigUInt (BS.pack [0x01, 0x00])) @?= Right 256
    , testCase "BigNInt" $
        convertBigInt (mkBigInt $ PC.BigInt'BigNInt (BS.pack [0x01, 0x00])) @?= Right (-256)
    , testCase "No value" $
        convertBigInt defMessage @?= Left "UTxO-RPC BigInt has no value"
    ]
 where
  mkBigInt :: PC.BigInt'BigInt -> PC.BigInt
  mkBigInt v = defMessage & PCF.maybe'bigInt .~ Just v

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

addressTests :: TestTree
addressTests =
  testGroup
    "Address conversion"
    [ testCase "Roundtrip" $
        convertAddress (convertAddressToBytes mockAddress) @?= Right mockAddress
    , testCase "Invalid bytes" $
        case convertAddress "not an address" of
          Left _ -> pure ()
          Right a -> assertFailure $ "expected decode failure, got " <> show a
    ]

--------------------------------------------------------------------------------
-- TxOutRef
--------------------------------------------------------------------------------

txOutRefTests :: TestTree
txOutRefTests =
  testGroup
    "TxOutRef conversion"
    [ testCase "Roundtrip" $
        convertTxoRef (convertTxOutRef mockTxOutRef) @?= Right mockTxOutRef
    ]

--------------------------------------------------------------------------------
-- Assets
--------------------------------------------------------------------------------

assetTests :: TestTree
assetTests =
  testGroup
    "Asset conversion"
    [ testCase "Single token" $
        convertMultiasset mockMultiasset
          @?= Right [(GYToken mockMintingPolicyId "MyToken", 1000)]
    , testCase "Empty token name" $
        convertMultiasset (mockMultiassetNamed "")
          @?= Right [(GYToken mockMintingPolicyId "", 1000)]
    ]

--------------------------------------------------------------------------------
-- Datum
--------------------------------------------------------------------------------

datumTests :: TestTree
datumTests =
  testGroup
    "Datum conversion"
    [ -- Dolos always emits a 'Datum' submessage even for outputs with no
      -- datum at all -- see the note above 'convertDatum' in the provider.
      testCase "Dolos degenerate no-datum message" $
        convertDatum defMessage @?= Right GYOutDatumNone
    , testCase "Hash-only" $
        convertDatum (defMessage & PCF.hash .~ mockDatumHashBytes)
          @?= Right (GYOutDatumHash mockDatumHash)
    , testCase "Inline" $
        convertDatum
          ( defMessage
              & PCF.maybe'payload .~ Just defMessage
              & PCF.originalCbor .~ mockDatumCbor
          )
          @?= Right (GYOutDatumInline mockDatum)
    ]

--------------------------------------------------------------------------------
-- Script
--------------------------------------------------------------------------------

scriptTests :: TestTree
scriptTests =
  testGroup
    "Script conversion"
    [ testCase "PlutusV1" $
        convertScript (mkScript $ PC.Script'PlutusV1 mockScriptBytes)
          @?= Right (GYPlutusScript $ scriptFromSerialisedScript @'PlutusV1 $ SBS.toShort mockScriptBytes)
    , testCase "PlutusV2" $
        convertScript (mkScript $ PC.Script'PlutusV2 mockScriptBytes)
          @?= Right (GYPlutusScript $ scriptFromSerialisedScript @'PlutusV2 $ SBS.toShort mockScriptBytes)
    , testCase "PlutusV3" $
        convertScript (mkScript $ PC.Script'PlutusV3 mockScriptBytes)
          @?= Right (GYPlutusScript $ scriptFromSerialisedScript @'PlutusV3 $ SBS.toShort mockScriptBytes)
    , testCase "PlutusV4 unsupported" $
        convertScript (mkScript $ PC.Script'PlutusV4 mockScriptBytes)
          @?= Left "UTxO-RPC Plutus V4 reference script is not supported by Atlas"
    , testCase "Native" $
        convertScript (mkScript $ PC.Script'Native mockPubkeyNativeScript)
          @?= (GYSimpleScript <$> convertNativeScript mockPubkeyNativeScript)
    ]
 where
  mkScript :: PC.Script'Script -> PC.Script
  mkScript v = defMessage & PCF.maybe'script .~ Just v

--------------------------------------------------------------------------------
-- Native script
--------------------------------------------------------------------------------

nativeScriptTests :: TestTree
nativeScriptTests =
  testGroup
    "Native script conversion"
    [ testCase "ScriptPubkey" $
        convertNativeScript mockPubkeyNativeScript
          @?= Right (RequireSignature $ paymentKeyHashFromApi $ either (error . show) id $ Api.deserialiseFromRawBytes (Api.AsHash Api.AsPaymentKey) mockKeyHashBytes)
    , testCase "ScriptAll" $
        convertNativeScript (mkNative $ PC.NativeScript'ScriptAll mockNativeScriptList)
          @?= (RequireAllOf <$> traverse convertNativeScript [mockPubkeyNativeScript])
    , testCase "ScriptAny" $
        convertNativeScript (mkNative $ PC.NativeScript'ScriptAny mockNativeScriptList)
          @?= (RequireAnyOf <$> traverse convertNativeScript [mockPubkeyNativeScript])
    , testCase "ScriptNOfK" $
        convertNativeScript (mkNative $ PC.NativeScript'ScriptNOfK mockScriptNOfK)
          @?= (RequireMOf 1 <$> traverse convertNativeScript [mockPubkeyNativeScript])
    , testCase "InvalidBefore" $
        convertNativeScript (mkNative $ PC.NativeScript'InvalidBefore 100)
          @?= Right (RequireTimeAfter $ slotFromWord64 100)
    , testCase "InvalidHereafter" $
        convertNativeScript (mkNative $ PC.NativeScript'InvalidHereafter 200)
          @?= Right (RequireTimeBefore $ slotFromWord64 200)
    ]
 where
  mkNative :: PC.NativeScript'NativeScript -> PC.NativeScript
  mkNative v = defMessage & PCF.maybe'nativeScript .~ Just v

--------------------------------------------------------------------------------
-- TxOutput
--------------------------------------------------------------------------------

txOutputTests :: TestTree
txOutputTests =
  testGroup
    "TxOutput conversion"
    [ testCase "Simplest case" $
        convertTxOutput mockTxOutRef (mkTxOutput 100_000_000 [] Nothing Nothing)
          @?= Right
            GYUTxO
              { utxoRef = mockTxOutRef
              , utxoAddress = mockAddress
              , utxoValue = valueFromLovelace 100_000_000
              , utxoOutDatum = GYOutDatumNone
              , utxoRefScript = Nothing
              }
    , testCase "With native asset" $
        convertTxOutput mockTxOutRef (mkTxOutput 100_000_000 [mockMultiasset] Nothing Nothing)
          @?= Right
            GYUTxO
              { utxoRef = mockTxOutRef
              , utxoAddress = mockAddress
              , utxoValue = valueFromList [(GYLovelace, 100_000_000), (GYToken mockMintingPolicyId "MyToken", 1000)]
              , utxoOutDatum = GYOutDatumNone
              , utxoRefScript = Nothing
              }
    , testCase "With datum hash" $
        convertTxOutput mockTxOutRef (mkTxOutput 0 [] (Just $ defMessage & PCF.hash .~ mockDatumHashBytes) Nothing)
          @?= Right
            GYUTxO
              { utxoRef = mockTxOutRef
              , utxoAddress = mockAddress
              , utxoValue = valueFromLovelace 0
              , utxoOutDatum = GYOutDatumHash mockDatumHash
              , utxoRefScript = Nothing
              }
    , testCase "With ref script" $
        convertTxOutput mockTxOutRef (mkTxOutput 0 [] Nothing (Just $ defMessage & PCF.maybe'script .~ Just (PC.Script'PlutusV2 mockScriptBytes)))
          @?= Right
            GYUTxO
              { utxoRef = mockTxOutRef
              , utxoAddress = mockAddress
              , utxoValue = valueFromLovelace 0
              , utxoOutDatum = GYOutDatumNone
              , utxoRefScript = Just $ GYPlutusScript $ scriptFromSerialisedScript @'PlutusV2 $ SBS.toShort mockScriptBytes
              }
    ]
 where
  mkTxOutput :: Integer -> [PC.Multiasset] -> Maybe PC.Datum -> Maybe PC.Script -> PC.TxOutput
  mkTxOutput lovelace multiassets mDatum mScript =
    defMessage
      & PCF.address .~ convertAddressToBytes mockAddress
      & PCF.coin .~ (defMessage & PCF.maybe'bigInt .~ Just (PC.BigInt'Int $ fromIntegral lovelace))
      & PCF.assets .~ multiassets
      & PCF.maybe'datum .~ mDatum
      & PCF.maybe'script .~ mScript

--------------------------------------------------------------------------------
-- Voting thresholds
--
-- 'convertPoolVotingThresholds'/'convertDRepVotingThresholds' hand-map a flat
-- 'repeated RationalNumber' list to named fields by index, per an
-- undocumented ordering inferred from Dolos's own construction (see the note
-- in the provider). That makes them the highest-risk hand-rolled parsers in
-- this module, so they get dedicated regression coverage for both the
-- correct-length happy path and the wrong-length failure.
--------------------------------------------------------------------------------

votingThresholdsTests :: TestTree
votingThresholdsTests =
  testGroup
    "Voting thresholds conversion"
    [ testCase "PoolVotingThresholds happy path" $
        convertPoolVotingThresholds (Just $ mkThresholds [1 % 10, 2 % 10, 3 % 10, 4 % 10, 5 % 10])
          @?= Right
            Ledger.PoolVotingThresholds
              { Ledger.pvtMotionNoConfidence = unitInterval (1 % 10)
              , Ledger.pvtCommitteeNormal = unitInterval (2 % 10)
              , Ledger.pvtCommitteeNoConfidence = unitInterval (3 % 10)
              , Ledger.pvtHardForkInitiation = unitInterval (4 % 10)
              , Ledger.pvtPPSecurityGroup = unitInterval (5 % 10)
              }
    , testCase "PoolVotingThresholds wrong length" $
        convertPoolVotingThresholds (Just $ mkThresholds [1 % 10, 2 % 10])
          @?= Left "UTxO-RPC protocol parameter pool_voting_thresholds has 2 values, expected 5"
    , testCase "PoolVotingThresholds missing" $
        convertPoolVotingThresholds Nothing
          @?= Left "UTxO-RPC protocol parameter pool_voting_thresholds is missing"
    , testCase "DRepVotingThresholds happy path" $
        convertDRepVotingThresholds (Just $ mkThresholds (fmap (% 100) [1 .. 10]))
          @?= Right
            Ledger.DRepVotingThresholds
              { Ledger.dvtMotionNoConfidence = unitInterval (1 % 100)
              , Ledger.dvtCommitteeNormal = unitInterval (2 % 100)
              , Ledger.dvtCommitteeNoConfidence = unitInterval (3 % 100)
              , Ledger.dvtUpdateToConstitution = unitInterval (4 % 100)
              , Ledger.dvtHardForkInitiation = unitInterval (5 % 100)
              , Ledger.dvtPPNetworkGroup = unitInterval (6 % 100)
              , Ledger.dvtPPEconomicGroup = unitInterval (7 % 100)
              , Ledger.dvtPPTechnicalGroup = unitInterval (8 % 100)
              , Ledger.dvtPPGovGroup = unitInterval (9 % 100)
              , Ledger.dvtTreasuryWithdrawal = unitInterval (10 % 100)
              }
    , testCase "DRepVotingThresholds wrong length" $
        convertDRepVotingThresholds (Just $ mkThresholds [1 % 10])
          @?= Left "UTxO-RPC protocol parameter drep_voting_thresholds has 1 values, expected 10"
    , testCase "DRepVotingThresholds missing" $
        convertDRepVotingThresholds Nothing
          @?= Left "UTxO-RPC protocol parameter drep_voting_thresholds is missing"
    ]
 where
  unitInterval :: Rational -> Ledger.UnitInterval
  unitInterval = fromJust . Ledger.boundRational

  mkThresholds :: [Rational] -> PC.VotingThresholds
  mkThresholds rs =
    defMessage
      & PCF.thresholds .~ fmap toRationalNumber rs

  toRationalNumber :: Rational -> PC.RationalNumber
  toRationalNumber r =
    defMessage
      & PCF.numerator .~ fromIntegral (numerator r)
      & PCF.denominator .~ fromIntegral (denominator r)

--------------------------------------------------------------------------------
-- Mock values
--------------------------------------------------------------------------------

mockAddress :: GYAddress
mockAddress = unsafeAddressFromText "addr_test1qp3fx29hm39xvyeer3v5xalcpmye7078vz09uylzuvxgqy0ma9xfzua4lag9wwgwk059k07f3kf46cjvx5ldmknqh7xqmg5pu4"

mockTxOutRef :: GYTxOutRef
mockTxOutRef = "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#0"

mockPolicyIdBytes :: ByteString
mockPolicyIdBytes = BS.replicate 28 0x11

mockMintingPolicyId :: GYMintingPolicyId
mockMintingPolicyId =
  mintingPolicyIdFromApi $
    either (error . show) id $
      Api.deserialiseFromRawBytes Api.AsPolicyId mockPolicyIdBytes

mockMultiasset :: PC.Multiasset
mockMultiasset = mockMultiassetNamed "MyToken"

mockMultiassetNamed :: ByteString -> PC.Multiasset
mockMultiassetNamed tokenName =
  defMessage
    & PCF.policyId .~ mockPolicyIdBytes
    & PCF.assets
      .~ [ defMessage
             & PCF.name .~ tokenName
             & PCF.outputCoin .~ (defMessage & PCF.maybe'bigInt .~ Just (PC.BigInt'Int 1000))
         ]

mockDatumHashBytes :: ByteString
mockDatumHashBytes = BS.replicate 32 0x22

mockDatumHash :: GYDatumHash
mockDatumHash =
  datumHashFromApi $
    either (error . show) id $
      Api.deserialiseFromRawBytes (Api.AsHash Api.AsScriptData) mockDatumHashBytes

-- | A known-good CBOR encoding of @Constr 0 [Constr 0 [I 48]]@, same shape as
-- the fixture used by the Maestro provider's own datum tests.
mockDatumCbor :: ByteString
mockDatumCbor = either (error . show) id $ BS16.decode "d8799fd8799f1830ffff"

mockDatum :: GYDatum
mockDatum =
  datumFromApi' $
    either (error . show) id $
      Api.deserialiseFromCBOR Api.AsHashableScriptData mockDatumCbor

mockScriptBytes :: ByteString
mockScriptBytes = BS.pack [0x01, 0x02, 0x03, 0x04]

mockKeyHashBytes :: ByteString
mockKeyHashBytes = BS.replicate 28 0x33

mockPubkeyNativeScript :: PC.NativeScript
mockPubkeyNativeScript =
  defMessage
    & PCF.maybe'nativeScript .~ Just (PC.NativeScript'ScriptPubkey mockKeyHashBytes)

mockNativeScriptList :: PC.NativeScriptList
mockNativeScriptList =
  defMessage
    & PCF.items .~ [mockPubkeyNativeScript]

mockScriptNOfK :: PC.ScriptNOfK
mockScriptNOfK =
  defMessage
    & PCF.k .~ 1
    & PCF.scripts .~ [mockPubkeyNativeScript]
