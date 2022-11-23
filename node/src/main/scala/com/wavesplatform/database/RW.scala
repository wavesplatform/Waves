package com.wavesplatform.database

import com.wavesplatform.metrics.RocksDBStats
import com.wavesplatform.metrics.RocksDBStats.DbHistogramExt
import org.rocksdb.{ReadOptions, RocksDB, WriteBatch}

class RW(db: RocksDB, readOptions: ReadOptions, batch: WriteBatch) extends ReadOnlyDB(db, readOptions) {
  def put[V](key: Key[V], value: V): Int = {
    val bytes = key.encode(value)
    RocksDBStats.write.recordTagged(key, bytes)
    batch.put(key.keyBytes, bytes)
    bytes.length
  }

  def put(key: Array[Byte], value: Array[Byte]): Unit = batch.put(key, value)

  def update[V](key: Key[V])(f: V => V): Unit = put(key, f(get(key)))

  /** Because of how leveldb batches work, you can increment a specific value only once! */
  def inc(key: Key[Int]): Int = {
    val newValue = get(key) + 1
    put(key, newValue)
    newValue
  }

  def delete(key: Array[Byte], statsKey: String): Unit = batch.delete(key)

  def delete(key: Array[Byte]): Unit = batch.delete(key)

  def delete[V](key: Key[V]): Unit = batch.delete(key.keyBytes)

  def filterHistory(key: Key[Seq[Int]], heightToRemove: Int): Unit = {
    val newValue = get(key).filterNot(_ == heightToRemove)
    if (newValue.nonEmpty) put(key, newValue)
    else delete(key)
  }

  def filterMetaHistory(key: Key[Seq[(Int, Int)]], heightToRemove: Int): Unit = {
    val newValue = get(key).filterNot(_._1 == heightToRemove)
    if (newValue.nonEmpty) put(key, newValue)
    else delete(key)
  }
}
