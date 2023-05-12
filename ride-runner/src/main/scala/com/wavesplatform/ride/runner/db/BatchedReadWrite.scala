package com.wavesplatform.ride.runner.db

import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.rocksdb.stats.RocksDBStats.DbHistogramExt
import com.wavesplatform.utils.OptimisticLockable
import org.rocksdb.*

class BatchedReadWrite(db: RocksDB, readOptions: ReadOptions, batch: SynchronizedWriteBatch)
    extends BatchedReadOnly(db, readOptions)
    with ReadWrite
    with OptimisticLockable {
  override def put[V](key: Key[V], value: V): Int = {
    val bytes = key.encode(value)
    RocksDBStats.write.recordTagged(key, bytes)
    batch.use(_.put(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes, bytes))
    bytes.length
  }

  override def put(key: Array[Byte], value: Array[Byte]): Unit = batch.use(_.put(key, value))

  override def update[V](key: Key[V], default: => V)(f: V => V): Unit = put(key, f(getOpt(key).getOrElse(default)))

  override def delete(key: Array[Byte]): Unit = batch.use(_.delete(key))

  override def delete[V](key: Key[V]): Unit =
    batch.use(_.delete(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes))
}
