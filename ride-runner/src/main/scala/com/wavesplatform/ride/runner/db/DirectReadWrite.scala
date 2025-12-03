package com.wavesplatform.ride.runner.db

import com.wavesplatform.database.Key
import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.rocksdb.stats.RocksDBStats.DbHistogramExt
import com.wavesplatform.utils.OptimisticLockable
import org.rocksdb.*

class DirectReadWrite(protected override val db: RocksDB) extends DirectReadOnly(db) with ReadWrite with HasDb with OptimisticLockable {
  override def put[V](key: Key[V], value: V): Int = {
    val bytes = key.encode(value)
    RocksDBStats.write.recordTagged(key, bytes)
    db.put(getCFH(key), key.keyBytes, bytes)
    bytes.length
  }

  override def put(key: Array[Byte], value: Array[Byte], cfh: Option[ColumnFamilyHandle]): Unit = db.put(orDefaultCFH(cfh), key, value)

  override def update[V](key: Key[V], default: => V)(f: V => V): Unit = put(key, f(getOpt(key).getOrElse(default)))

  override def delete(key: Array[Byte], cfh: Option[ColumnFamilyHandle]): Unit = db.delete(orDefaultCFH(cfh), key)

  override def delete[V](key: Key[V]): Unit = db.delete(getCFH(key), key.keyBytes)
}
