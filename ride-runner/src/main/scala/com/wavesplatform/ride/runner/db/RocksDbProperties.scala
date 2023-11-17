package com.wavesplatform.ride.runner.db

object RocksDbProperties {
  // List: https://github.com/facebook/rocksdb/blob/main/include/rocksdb/db.h#L1217-L1249
  // Description: https://github.com/facebook/rocksdb/blob/main/include/rocksdb/db.h#L921
  val All = List(
    // number of immutable memtables that have not yet been flushed.
    "rocksdb.num-immutable-mem-table",
    // 1 if a memtable flush is pending; otherwise, returns 0.
    "rocksdb.mem-table-flush-pending",
    // 1 if at least one compaction is  pending; otherwise, returns 0.
    "rocksdb.compaction-pending",
    // accumulated number of background errors.
    "rocksdb.background-errors",
    // approximate size of active memtable (bytes).
    "rocksdb.cur-size-active-mem-table",
    // approximate size of active and unflushed immutable memtables (bytes).
    "rocksdb.cur-size-all-mem-tables",
    // approximate size of active and unflushed immutable memtables (bytes).
    "rocksdb.size-all-mem-tables",
    // total number of entries in the active memtable.
    "rocksdb.num-entries-active-mem-table",
    // total number of entries in the unflushed immutable memtables.
    "rocksdb.num-entries-imm-mem-tables",
    // total number of delete entries in the active memtable.
    "rocksdb.num-deletes-active-mem-table",
    // total number of delete entries in the unflushed immutable memtables.
    "rocksdb.num-deletes-imm-mem-tables",
    // estimated number of total keys in the active and unflushed immutable memtables and storage.
    "rocksdb.estimate-num-keys",
    // estimated memory used for reading SST tables, excluding memory used in block cache
    // e.g., filter and index blocks.
    "rocksdb.estimate-table-readers-mem",
    // 0 if deletion of obsolete files is enabled, otherwise, returns a non-zero number.
    // This name may be misleading because true(non-zero) means disable, but we keep the name for backward
    // compatibility.
    "rocksdb.is-file-deletions-enabled",
    // number of unreleased snapshots of the database.
    "rocksdb.num-snapshots",
    // number representing unix timestamp of oldest unreleased snapshot.
    "rocksdb.oldest-snapshot-time",
    // number of live versions.
    // `Version` is an internal data structure.
    // See version_set.h for details. More live versions often mean more SST files are held from being deleted, by
    // iterators or unfinished compactions.
    "rocksdb.num-live-versions",
    // number of current LSM version.
    // It is a uint64_t integer number, incremented after there is any change to the LSM tree.
    // The number is not preserved after restarting the DB. After DB restart, it will start from 0 again.
    "rocksdb.current-super-version-number",
    // estimate of the amount of live data in bytes.
    // For BlobDB, it also includes the exact value of live bytes in the blob files of the version.
    "rocksdb.estimate-live-data-size",
    // the minimum log number of the log files that should be kept.
    "rocksdb.min-log-number-to-keep",
    // the minimum file number for an obsolete SST to be kept.
    // The max value of `uint64_t` will be returned if all obsolete files can be deleted.
    "rocksdb.min-obsolete-sst-number-to-keep",
    // total size (bytes) of all SST files.
    // WARNING: may slow down online queries if there are too many files.
    "rocksdb.total-sst-files-size",
    // total size (bytes) of all SST files belong to the latest LSM tree.
    "rocksdb.live-sst-files-size",
    // number of level to which L0 data will be compacted.
    "rocksdb.base-level",
    // estimated total number of bytes compaction needs to rewrite to get all levels down to under target size.
    // Not valid for other compactions than level-based.
    "rocksdb.estimate-pending-compaction-bytes",
    "rocksdb.num-running-compactions",
    "rocksdb.num-running-flushes",
    // the current actual delayed write rate. 0 means no delay.
    "rocksdb.actual-delayed-write-rate",
    // 1 if write has been stopped.
    "rocksdb.is-write-stopped",
    // estimation of oldest key timestamp in the DB.
    // Currently only available for FIFO compaction with compaction_options_fifo.allow_compaction = false.
    "rocksdb.estimate-oldest-key-time",
    "rocksdb.block-cache-capacity",
    // the memory size for the entries residing in block cache.
    "rocksdb.block-cache-usage",
    // the memory size for the entries being pinned.
    "rocksdb.block-cache-pinned-usage"
  )
}
