package com.wavesplatform.database.rocksdb

import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.rocksdb.stats.RocksDBStats.DbHistogramExt
import org.rocksdb.{ReadOptions, RocksDB, WriteBatch}

class RW(db: RocksDB, readOptions: ReadOptions, batch: WriteBatch) extends ReadOnlyDB(db, readOptions) {
  def put[V](key: Key[V], value: V): Int = {
    val bytes = key.encode(value)
    RocksDBStats.write.recordTagged(key, bytes)
    batch.put(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes, bytes)
    bytes.length
  }

  def put(key: Array[Byte], value: Array[Byte]): Unit = batch.put(key, value)

  def update[V](key: Key[V])(f: V => V): Unit = put(key, f(get(key)))

  def delete(key: Array[Byte]): Unit = batch.delete(key)

  def delete[V](key: Key[V]): Unit =
    batch.delete(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes)

  def filterHistory(key: Key[Seq[Int]], heightToRemove: Int): Unit = {
    val newValue = get(key).filterNot(_ == heightToRemove)
    if (newValue.nonEmpty) put(key, newValue)
    else delete(key)
  }
}
