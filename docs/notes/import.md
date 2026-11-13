---
purpose: Implementation notes for the blockchain importer (Importer.scala) and the block append path it drives, including how to measure it
---

# Importer

## Measuring an import

A full import takes hours, so measure a slice instead. Import once to a height just before a heavy stretch (`waves import -c <conf> -i <blocks-file> -h <height>`), copy the resulting `data` directory, then time each variant importing from a fresh copy of it to the same height. Testnet 170001 to 200001 holds load-test blocks of up to ~1 MB (6.7M transactions in 30k blocks) and runs in about 4 to 5 minutes. Identical runs differ by up to 5%, so a smaller difference needs repeated runs before it means anything.

Check every variant for correctness by digesting the resulting database: entry count and a hash over all key/value pairs, per column family, opened read-only. The node puts each column family in its own subdirectory (`cf_paths`), so a read-only open needs those paths set too. Imports that append the same blocks must leave byte-identical databases; executing transactions and applying exported snapshots (`-s`) already leave different `default` and `api` column families.

`waves.db.rocksdb.enable-statistics = true` writes RocksDB statistics to `<data>/LOG`, but only every `stats_dump_period_sec` (600 s by default, not configurable here). `rocksdb.db.write.micros`, `rocksdb.db.multiget.micros`, `rocksdb.db.get.micros` and `rocksdb.sst.read.micros` show where the database time goes; the per column family `flush_started` events count the keys each family receives.

## Where the time goes

On the slice above, after the fixes below, the appender thread is busy almost all the time and splits roughly into: the RocksDB batch write (memtable inserts of ~2,200 keys per block, a quarter of the time), building that batch (`RocksDBWriter.doAppend`), batch-loading the accounts a block touches (`Caches.loadCacheData`, mostly SST reads for addresses missing from the block cache), and transaction execution. A larger block cache (`rocksdb.main-cache-size = 2G`) took 11% off; a larger `max-cache-size` did not help. Disabling the WAL did not help either: the write cost is the memtable, not the log.

Things that were slow for reasons unrelated to the work itself:

- cats `foldM` on a `Seq` reads elements by index, which is quadratic on a `List`, and block transactions are a `List`. Summing a block's fees with it took a fifth of the appender on large blocks.
- The root logger was at TRACE, filtered per appender, so every `log.trace` built its message (Base58 of every transaction id in `appendKeyBlock`). The root level now follows the appender levels.
- Error messages built eagerly on the hot path (`safeSum`'s source string Base58-encodes an address per balance change).
- Batch keys in program order: RocksDB's skiplist reuses the previous insert's position, so `RW` writes a batch sorted by key.
- The appender waited for the reader to parse the next hundred blocks; parsing now runs on its own thread. Whatever stops the reader, other than the end of the input, goes through the queue and is rethrown by the importer, so a corrupt file fails the import instead of ending it early.
- Every lookup that missed the block cache re-read a whole-file bloom filter (about 5 MB on `default` and `api`), 710 GB of filter reads in the first ten minutes of an import. Index and filter blocks are now partitioned (`kTwoLevelIndexSearch`, `partitionFilters`), so a miss reads a few kilobytes. `rocksdb.block.cache.filter.bytes.insert` in the statistics shows it.
- Block validation checked each VRF proof twice (`validateGenerationSignature`, `validateBlockDelay`), serially on the appender. `consensus.CachedVrf` keeps successful results, and the importer checks proofs ahead on the signature pool: a proof needs the hit source from 101 blocks back under fair PoS, so many run in parallel. Both take the hit source height from `PoSSelector.hitSourceHeight`, so they cannot drift apart.
- Comparing `Height`s went through an implicit conversion to `Ordered`, boxing the `Int` on every comparison, and the balance-at-height caches were keyed by `(Height, AddressId)` tuples, which box both fields in `equals` and `hashCode`.

Past height 2M testnet blocks are dominated by dApp invocations: from 2146124 to 2176100 (30k blocks, about 11 minutes), 94% of the appender is `InvokeScriptTransactionDiff`, so that stretch measures the RIDE evaluator rather than the importer.

## Snapshot import (`-s`)

Applying snapshots skips `BlockDiffer.prepareCaches`, so every account the block touches is read with its own `RocksDB.get` in `Caches.append`, and those point reads mostly hit SST files. On the slice above that made snapshot import over four times slower than executing the transactions. Batch-loading the accounts named in the snapshots (balances, lease balances, order fills) brought it from 1343 s to 228 s; not merged yet.
