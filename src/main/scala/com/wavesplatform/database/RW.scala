package com.wavesplatform.database

import com.wavesplatform.database.LevelDBWriter.Key
import org.iq80.leveldb.{DB, ReadOptions}

class RW(db: DB) extends AutoCloseable {
  private val batch = db.createWriteBatch()
  private val snapshot = db.getSnapshot
  private val readOptions = new ReadOptions().snapshot(snapshot)

  def get[V](key: Key[V]): V = key.parse(db.get(key.keyBytes, readOptions))

  def put[V](key: Key[V], value: V): Unit = batch.put(key.keyBytes, key.encode(value))

  def delete(key: Array[Byte]): Unit = batch.delete(key)

  def delete[V](key: Key[V]): Unit = batch.delete(key.keyBytes)

  override def close(): Unit = {
    try { db.write(batch) }
    finally {
      batch.close()
      snapshot.close()
    }
  }
}
