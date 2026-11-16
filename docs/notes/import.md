---
purpose: Implementation notes for the blockchain importer (Importer.scala) and the block append path it drives, including how to measure it
---

# Importer

## Measuring an import

A full import takes hours, so measure a slice instead. Import once to a height just before a heavy stretch (`waves import -c <conf> -i <blocks-file> -h <height>`), copy the resulting `data` directory, then time each variant importing from a fresh copy of it to the same height. Testnet 170001 to 200001 holds load-test blocks of up to ~1 MB (6.7M transactions in 30k blocks) and runs in about 4 to 5 minutes. Identical runs differ by up to 5%, so a smaller difference needs repeated runs before it means anything.

Check every variant for correctness by digesting the resulting database: entry count and a hash over all key/value pairs, per column family, opened read-only. The node puts each column family in its own subdirectory (`cf_paths`), so a read-only open needs those paths set too. Imports that append the same blocks must leave byte-identical databases; executing transactions and applying exported snapshots (`-s`) already leave different `default` and `api` column families.

The importer logs the file offset it starts reading from (`Opening import file: ..., offset=N`). The blocks file is a sequence of records, each a 4-byte big-endian length and a block (legacy bytes early in the chain, protobuf `PBBlock` later), so a throwaway `runMain` that seeks to such an offset and parses records can count transactions by type over a stretch without importing it.

Profile with the JProfiler agent started from a config file (`-agentpath:...=config=jprofiler_config.xml`), and take intermediate snapshots with `jpcontroller -n <pid>` (`saveSnapshot <path>`): attaching another session replaces the agent's settings and drops what it recorded. A periodic snapshot is a `<timer intervalTime="10" intervalTimeUnit="m">` trigger; the period is in attributes, and an unknown child element is ignored. On macOS, JProfiler's async sampling puts 40 to 50% of the appender into the `write` syscall under RocksDB's WAL while RocksDB's own timers measure a few percent, so take proportions from Linux profiles and the database share from RocksDB statistics.

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

Past height 2M testnet blocks are dominated by dApp invocations: from 2146124 to 2176100 (30k blocks, about 11 minutes), 94% of the appender is `InvokeScriptTransactionDiff`, so that stretch measures the RIDE evaluator rather than the importer (see `docs/notes/ride-evaluator.md`).

A full testnet import with all the changes, on an Apple silicon Mac with `rocksdb.main-cache-size = 2G`, reached height 2137047 in 51 minutes; `rocksdb.write.self` counts one write per block, which gives the height per statistics window. Up to about 1.8M the database took 15 to 25% of each window; past 2.15M it takes 4 to 5% (10 to 25 s per 10 minutes, no stalls, compaction keeping up), throughput drops to 50 to 180 blocks per second, and the rest is script execution. Block size is not worth tuning there: SST reads take 2 to 3 s per window, filter and index partitions mostly hit the cache, and the many data block misses are compaction reads, which bypass the cache.

## Import probe (JProfiler)

`metrics.ImportProbe` is an embedded JProfiler probe; without the agent its calls do nothing and its telemetries are never computed. It records the time of each block append as a payload named after its 50,000-block height range, so the probe's hot spots give the append time per range, and these telemetries, polled once per second by the agent:

- Import height, blocks/s, transactions/s, and script complexity/s. Complexity is read after each append from the liquid block's snapshot (`scriptsComplexity`, the sum over its transactions).
- RocksDB time in get, multiGet, write and seek, and stall time, in microseconds per second (1 s means the appender spent the whole second there). SST read and table open IO time count all threads, compactions included, so they can exceed 1 s.
- Block cache hit rate for data, index and filter blocks; 0 in a second with no lookups of that kind.
- Gets served by the memtable, L0, L1 and L2+, and lookups skipped by a bloom filter; table opens/s; keys written/s; bytes/s written, read by gets, to the WAL, by flushes and by compactions.
- Per column family: L0 file count and pending compaction in MB. Totals over all column families: block cache usage and memtable size in MB. Running compactions and flushes.
- Per state cache (the Guava caches in `Caches` and `RocksDBWriter`, named in `Caches.cacheStats`): hit rate, misses/s and evictions/s.

Everything except height, throughput and complexity needs `waves.db.rocksdb.enable-statistics = true`; without it those lines stay at 0. The gauges come from RocksDB properties, which need no statistics. `num-files-at-level<N>` is a string property: `getLongProperty` throws `NotFound` for it, and a throwing telemetry aborts the whole sample.

To record it, start the agent from a config file as above and add a probe recording action to the session's `jvmStart` trigger, next to `startRecording`:

```xml
<startProbeRecording name="com.wavesplatform.metrics.ImportProbe" events="false" />
```

`startProbeRecording` is an action of its own; nested inside `startRecording` it is silently ignored. Keep `events` off: one event per block is millions over a full import. Leave `<allocation>` out of `startRecording` for a full import: allocation recording put 72% of the appender's samples into `JvmtiObjectAllocEventCollector` and cut a mainnet import to about 2,000 blocks in 20 minutes. `asyncBufferPercentage` only sizes the per-thread sampling buffers and cost nothing measurable at 400. The probe and its telemetries are under "JVM & Custom Probes" in the JProfiler UI, and the MCP server serves them as subsystem `custom:com.wavesplatform.metrics.ImportProbe` (hot spots in `performance.hotspots`, telemetries in `telemetry.list`). Telemetry values are stored as 32-bit ints. Do not use `TelemetryFormat.scale`: the MCP server applies it but the JProfiler 16.3 UI did not, so byte rates returned in kB with `scale = -3` showed as 0 MB. Byte rates are plain bytes (an int holds up to 2 GB/s) and anything that reaches gigabytes is a plain number of MB with the unit in the telemetry name.

Script probes (`<scriptProbes>` in the session) did not record when defined by hand in an offline config file: with JProfiler 16.3 neither the probe nor an error appeared, with or without a `startProbeRecording` for it. Per-dApp RIDE time needs them defined in the JProfiler UI or through the MCP server instead (`InvokeScriptTransactionDiff$.apply`, the transaction is `param4`), or an embedded payload.

Past mainnet height 4.5M RIDE's `wavesBalance()` was about 8% of the appender on macOS: it computes the generating balance (the minimum over 1,000 blocks of balance history) on every call, even when the script reads only `available`, and blocks query the same few dApp addresses again and again. `RocksDBWriter` keeps the persisted part of `balanceSnapshots` per address, `from` and target height until the next append or rollback; the import probe's `balanceSnapshots` cache lines show its hit rate.

At mainnet height 4.58M RIDE `pow` (BigDecimalMath at 154 digits) was about 590 calls/s and 7% of the appender, about 116 us per call, measured with a temporary counter; only 8% of calls repeated the arguments of one of the last 10,000, so an argument cache would save under 1%, and the counter was removed. BigDecimalMath must stay the implementation: another library could differ in the last digits, and an exact result such as 1.999...9 against 2 rounds differently, which would break consensus on replay.

## Snapshot import (`-s`)

Applying snapshots skips `BlockDiffer.prepareCaches`, so every account the block touches is read with its own `RocksDB.get` in `Caches.append`, and those point reads mostly hit SST files. On the slice above that made snapshot import over four times slower than executing the transactions. Batch-loading the accounts named in the snapshots (balances, lease balances, order fills) brought it from 1343 s to 228 s; not merged yet.
