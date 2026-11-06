---
purpose: Implementation notes for BlockchainUpdaterImpl locking (validation vs persistence) and what readers can observe
---

# BlockchainUpdaterImpl

## Lock scope: validate without the lock, persist under it

`processBlock` and `processMicroBlock` validate (BlockDiffer: state reads, script execution, signatures, finalization voting) without the write lock; only persistence takes it: the BlockchainUpdates triggers, `rocksdb.append`, the `ngState` swap, `restTotalConstraint` and `publishLastBlockInfo`. Readers on other threads (API, UTX, microblock mining, gRPC) keep reading the previous state while a block is validated. Pinned by `BlockchainUpdaterImplSpec` ("readers are not blocked while a block/microblock is validated", which park validation inside `RocksDBWriter.loadCacheData`).

This is correct only because there is a single writer at a time: in the node every `processBlock`, `processMicroBlock` and `removeAfter` runs on the appender thread (`Application.scala`, `singleThread("appender")`: BlockAppender, MicroblockAppender, ExtensionAppender, rollbackTask, BlockChallenger, key-block forging, checkGenesis). The Importer appends, rolls back (extension `rollbackTo`) and, in its shutdown hook, persists the liquid block on its own appender scheduler; the hook used to call `processBlock` on the JVM hook thread, which could overlap an extension rollback once validation left the lock. So nothing can change the state between validation and persistence. A new writer must keep this: there is no guard and no second lock in the updater.

Keep in the locked part:

- The triggers. Their "blockchain before" arguments are views over the live `rocksdb` (`referencedBlockchain`, `rocksdb` itself), valid only around the append, and firing them under the lock means a BlockchainUpdates subscriber that queries the node after an event blocks until the commit is visible. They are cheap: "after" values come from the snapshot, "before" values hit the caches validation just warmed.
- Building the new `NgState`: `featuresApprovedWithBlock`, `collectLeasesToCancel` (raw DB) and the conflict generators read `rocksdb` after the append.

`removeAfter` stays fully under the write lock: the rollback is itself the state change. `scheduleMining`, logging and metrics run after the commit (see "No pacing between key blocks" in `mining.md` for the side effect on mining).

Readers still take the read lock per call, so a reader issuing several calls can see two states; a lock-free design (immutable view of `ngState` + its own height over the append-only `rocksdb`) was considered and deferred: it needs every getter to stop deriving heights from `rocksdb.height`, and expired-lease cancellations and rollbacks break the idempotent overlay.
