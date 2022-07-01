package com.wavesplatform.database

import org.rocksdb.{ReadOptions, RocksDB, RocksIterator}

trait DBResource extends AutoCloseable {
  def get[V](key: Key[V]): V
  def get(key: Array[Byte]): Array[Byte]
  def iterator: RocksIterator // Should have a single instance
}

object DBResource {
  def apply(db: RocksDB): DBResource = new DBResource {
    private[this] val snapshot    = db.getSnapshot
    private[this] val readOptions = new ReadOptions().setSnapshot(snapshot)

    override def get[V](key: Key[V]): V = key.parse(db.get(readOptions, key.keyBytes))

    override def get(key: Array[Byte]): Array[Byte] = db.get(readOptions, key)

    override lazy val iterator: RocksIterator = db.newIterator(readOptions)

    override def close(): Unit = {
      iterator.close()
      snapshot.close()
    }
  }
}
