package com.wavesplatform.database

import com.wavesplatform.metrics.LevelDBStats
import org.iq80.leveldb.{DB, ReadOptions, WriteBatch}

class RW(db: DB, readOptions: ReadOptions, batch: WriteBatch) extends ReadOnlyDB(db, readOptions) {
  def put[V](key: Key[V], value: V): Unit = LevelDBStats.write.measureForKey(key) {
    batch.put(key.keyBytes, key.encode(value))
  }

  def update[V](key: Key[V])(f: V => V): Unit = put(key, f(get(key)))

  def delete(key: Array[Byte], statsKey: String): Unit = LevelDBStats.delete.measureForKey(statsKey) {
    batch.delete(key)
  }

  def delete[V](key: Key[V]): Unit = LevelDBStats.delete.measureForKey(key) {
    batch.delete(key.keyBytes)
  }

  def filterHistory(key: Key[Seq[Int]], heightToRemove: Int): Unit = put(key, get(key).filterNot(_ == heightToRemove))
}
