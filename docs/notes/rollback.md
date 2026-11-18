---
purpose: Implementation notes for rollback depth (max-rollback-depth, the safe rollback height), what RocksDBWriter prunes behind it, and how to repair a DB rolled back past it
---

# Rollback depth and history pruning

## What the safe rollback height protects

Every append sets `SafeRollbackHeight` to `height - max-rollback-depth` (`RocksDBWriter.doAppend`). `rollbackTo`, `BlockchainUpdaterImpl.removeAfter` and `waves util rollback-to` refuse to go below it, because history behind it is pruned in two ways:

- Keys kept through `updateHistory` (`AssetDetails`, `LeaseDetails`, `AddressScript`, `AssetScript`, `Sponsorship`): on each update of a key, every entry below the threshold except the newest one is deleted, both the value and its height in the `*History` list. `CarryFee(threshold - 1)` is deleted on every append, so only the last `max-rollback-depth + 1` carry fees exist.
- With `cleanup-interval` set (off by default), the background cleanup also deletes WAVES balance, asset balance and account data nodes behind the safe height. A rollback past it is then unrecoverable.

An offline rollback (`waves util rollback-to`, or a repair as below) calls `RocksDBWriter.rollbackTo` directly, so BlockchainUpdates subscribers get no ROLLBACK event: the extension's own DB rolls back to the node height on the next start, but a consumer resuming from its stored height must re-check block ids.

Never pruned: transactions, per-height lists (`IssuedAssets`, `UpdatedAssets`, `SponsoredAssets`, `ChangedAddresses`), `StateHash`, `BlockStateHash`, and (without `cleanup-interval`) balance, data, lease balance and order fill history.

Forcing a rollback below the safe height (by lowering the `SafeRollbackHeight` key) runs without error and leaves state silently wrong: a key whose entry at or before the target was pruned ends up with an empty history, so the asset, lease or script reads as absent, and `CarryFee(target)` reads as 0, so the next block pays its miner the wrong amount.

## Repairing a DB rolled back past the safe height

Possible only with `cleanup-interval` off. Done once on testnet (2026-11, 7683 blocks back to 4336649, verified against the original state hashes):

1. Pick the target T and list damaged keys: scan each `*History` keyspace for keys whose remaining heights are all above T, then keep those whose object existed at T (`AssetStaticInfo` issue height at or below T for assets, sponsorship and asset scripts; creation height for leases; first balance height of the address for address scripts). Because pruning keeps the newest entry below the threshold, a key with any surviving entry at or below T is intact. A target just before the damaged objects were issued often leaves nothing to repair: 5 assets were damaged at 4339234, none at 4336649.
2. Dump `StateHash` for T+1 up to the last good height first; the rollback deletes them and they are the reference for the check in step 6. This needs `store-state-hashes` on in the original run.
3. With the node stopped, put T into the `SafeRollbackHeight` key and call `RocksDBWriter.rollbackTo(T)`.
4. Write `CarryFee(T)`: the sum of `BlockDiffer.computeTxFeeInfo(blockchain, tx, hasNg = true).carry` over block T's transactions, with the rolled-back writer as the blockchain. Check the computation first against carry fees still stored near the tip.
5. Repair damaged keys, if any are left: an asset's volume at T is the sum of its holders' balances at T (balance history is intact); name, description and the reissuable flag can be copied from the oldest remaining entry only if no `UpdateAssetInfo` or non-reissuable reissue happened in between (the per-height `UpdatedAssets` lists show every update).
6. Replay from T with `store-state-hashes = true` and compare the recomputed `StateHash` values with the dump. The block header `stateHash` check (light node fields) fails fast on a wrong carry fee or volume too, but only for keys the replayed blocks touch, so it is not a substitute for the dump.

## Choosing max-rollback-depth

Pruning saves little: on testnet at height 4.34M (66.7 GB of keys and values, depth 2000) it had removed about 0.4 GB over the whole chain, about 0.6% (address scripts about 290 MB at about 8 KB each, carry fees about 60 MB, asset details about 45 MB, the rest a few MB). A larger depth costs elsewhere: `blockHeightCache` is sized `max-rollback-depth + 1000` and an import fills it, and each `*History` list holds every update within the depth and is rewritten in full on each update (the hottest asset had 528 entries within 2000 blocks, so about 2.6k at 10000 and about 26k, 100 KB per rewrite, at 100000). Raising the depth protects only blocks appended after the change; entries already pruned do not come back.

Keep 2000 on nodes, where forks are far shorter. For imports and benchmarks, where a jar may be swapped mid-run, use 10000 (about a week of blocks at one per minute). Better still, take a RocksDB checkpoint of `data` before switching jars; that covers any depth.
