package com.wavesplatform.database

import org.rocksdb.{ReadOptions, RocksDB, RocksIterator}

import scala.collection.View
import scala.collection.mutable.ArrayBuffer

trait DBResource extends AutoCloseable {
  def get[V](key: Key[V]): V
  def get(key: Array[Byte]): Array[Byte]
  def multiGet[V, K](keys: ArrayBuffer[(Key[V], K)]): View[(V, K)]
  def prefixIterator: RocksIterator // Should have a single instance
  def fullIterator: RocksIterator
}

object DBResource {
  def apply(db: RocksDB): DBResource = new DBResource {
    private[this] val snapshot    = db.getSnapshot
    private[this] val readOptions = new ReadOptions().setSnapshot(snapshot).setVerifyChecksums(false)

    override def get[V](key: Key[V]): V = key.parse(db.get(readOptions, key.keyBytes))

    override def get(key: Array[Byte]): Array[Byte] = db.get(readOptions, key)

    override def multiGet[V, K](keys: ArrayBuffer[(Key[V], K)]): View[(V, K)] = db.multiGet(readOptions, keys)

    override lazy val prefixIterator: RocksIterator = db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))

    override lazy val fullIterator: RocksIterator = db.newIterator(readOptions.setTotalOrderSeek(true))

    override def close(): Unit = {
      prefixIterator.close()
      fullIterator.close()
      snapshot.close()
    }
  }
}
