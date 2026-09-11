# TODO

Open items tracking upstream Dolos/UTxO-RPC behavior this provider works around. Referenced from `src/GeniusYield/Providers/UtxoRpc.hs`.

## Degenerate empty `Datum` message on outputs with no datum

`pallas-utxorpc`'s `map_tx_datum` always emits a `Datum` submessage for a UTxO, even when the UTxO has no datum at all -- it sets `hash = []` and `payload = None` in that case, rather than omitting the field. A real datum hash is always 32 bytes, so `convertDatum` special-cases "empty hash + no payload" as `GYOutDatumNone`.

Once this is fixed upstream (Dolos should stop emitting the degenerate message), the empty-hash special case in `convertDatum` can be dropped -- the `Nothing` branch would then only ever mean "genuinely no datum" via the payload field's own absence.

## `ReadEraSummary` doesn't return the full historical era table

Dolos's (as of 1.6.0) `ReadEraSummary` (`src/serve/grpc/v1alpha/query.rs`) only reflects whatever era-boundary records the backing node has itself locally processed since it started tracking chain state -- it does not reconstruct the full historical era table the way a full node or Blockfrost's `/network/eras` endpoint does. Dolos's own miniBF `/network/eras` route (`crates/minibf/src/routes/network.rs`, "Special, hardcoded stuff" block) already does this padding; the gRPC/UTxO-RPC and Ogmios (`src/serve/o7s_unix/statequery.rs`) interfaces do not.

Atlas' era interpreter is a fixed 7-slot (Byron..Conway) structure with no partial form, so a live response with fewer summaries can never be parsed into one. Era history for the UtxoRpc provider is, for now, supplied by the caller instead (`GeniusYield.GYConfig.utxoRpcNetworkEraHistory`, a hardcoded per-network table).

The commented-out `utxoRpcEraHistory` block above `utxoRpcSlotActions` in `UtxoRpc.hs` is the long-term replacement: once Dolos's `read_era_summary` is patched to reuse miniBF's padding logic, restore it and thread `utxoRpcEraHistory provider` into `utxoRpcGetParameters` instead of the hardcoded `Api.EraHistory`.

## `ReadParams` (and minibf's `/epoch/*/parameters`) return the wrong effective PlutusV3 cost model

Dolos derives the "effective" cost models for the current epoch from the wrong protocol state -- confirmed on both its gRPC/UTxO-RPC `ReadParams` and its minibf REST `/epochs/{n}/parameters` (same underlying bug, not gRPC-specific). Measured against real preprod 2026-09-11 (protocol version 11, PlutusV3 = 350 parameters, cross-checked via Maestro and Koios `epoch_params`):

| Source | protocol_major_ver | PlutusV3 param count |
| --- | --- | --- |
| Dolos gRPC (`ReadParams`) | 10 | 251 (= genesis-baked value, byte-identical to `input-output-hk/cardano-configurations`' immutable preprod genesis) |
| Dolos minibf REST | 10 | 297 (a third, still-wrong value) |
| Real network | 11 | 350 |

This is upstream `txpipe/dolos#1274` ("minibf: return the effective Plutus cost models from `/epoch/*/parameters` endpoints"), open since 2026-08-26, no linked PR as of `v2.0.0-alpha.0` (2026-09-11). The issue's own testing covered Mainnet/Preview only -- the preprod measurement above is new signal, worth adding as a comment upstream.

Worked around client-side: `preprodPlutusV3CostModel`/`mainnetPlutusV3CostModel` (`Providers/Common.hs`), threaded through `utxoRpcNetworkPlutusV3CostModel` (`GYConfig.hs`) and `convertCostModels` (`UtxoRpc.hs`) to override just the PlutusV3 field -- PlutusV1/V2 are left sourced live from Dolos (not confirmed affected). Once `dolos#1274` is fixed and released, remove the override and let `convertCostModels` derive PlutusV3 from the live `ReadParams` response like V1/V2 already do.
