package com.wavesplatform.database

import com.google.common.collect.Maps
import com.wavesplatform.metrics.RocksDBStats
import com.wavesplatform.metrics.RocksDBStats.DbHistogramExt
import org.rocksdb.{ColumnFamilyHandle, ReadOptions, RocksDB, RocksIterator}

import scala.util.Using

class ReadOnlyDB(db: RocksDB, readOptions: ReadOptions) {
  def get[V](key: Key[V]): V = {
    val bytes = db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    key.parse(bytes)
  }

  def multiGetOpt[V](keys: collection.Seq[Key[Option[V]]], valBufferSize: Int): Seq[Option[V]] =
    db.multiGetOpt(readOptions, keys, valBufferSize)

  def multiGet[V](keys: collection.Seq[Key[V]], valBufferSize: Int): Seq[Option[V]] =
    db.multiGet(readOptions, keys, valBufferSize)

  def multiGetOpt[V](keys: collection.Seq[Key[Option[V]]], valBufSizes: Seq[Int]): Seq[Option[V]] =
    db.multiGetOpt(readOptions, keys, valBufSizes)

  def multiGetInts(keys: collection.Seq[Key[Int]]): Seq[Option[Int]] =
    db.multiGetInts(readOptions, keys)

  def has[V](key: Key[V]): Boolean = {
    val bytes = db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    bytes != null
  }

  def newIterator: RocksIterator = db.newIterator(readOptions.setTotalOrderSeek(true))

  def iterateOverWithSeek(prefix: Array[Byte], seek: Array[Byte], cfh: Option[ColumnFamilyHandle] = None)(f: DBEntry => Boolean): Unit =
    Using.resource(db.newIterator(cfh.getOrElse(db.getDefaultColumnFamily), readOptions.setTotalOrderSeek(true))) { iter =>
      iter.seek(seek)
      var continue = true
      while (iter.isValid && iter.key().startsWith(prefix) && continue) {
        continue = f(Maps.immutableEntry(iter.key(), iter.value()))
        if (continue) iter.next()
      }
    }

  def iterateOver(prefix: Array[Byte], cfh: Option[ColumnFamilyHandle] = None)(f: DBEntry => Unit): Unit =
    Using.resource(db.newIterator(cfh.getOrElse(db.getDefaultColumnFamily), readOptions.setTotalOrderSeek(true))) { iter =>
      iter.seek(prefix)
      while (iter.isValid && iter.key().startsWith(prefix)) {
        f(Maps.immutableEntry(iter.key(), iter.value()))
        iter.next()
      }
    }

  /** Tries to find the exact key if prefix.length < 10.
    * @see
    *   RDB.newColumnFamilyOptions
    */
  def prefixExists(prefix: Array[Byte]): Boolean = Using.resource(db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))) {
    iter =>
      iter.seek(prefix)
      iter.isValid
  }
}
