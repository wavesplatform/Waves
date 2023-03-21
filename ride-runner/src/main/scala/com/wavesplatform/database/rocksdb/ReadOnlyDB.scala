package com.wavesplatform.database.rocksdb

import com.google.common.collect.Maps
import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.{DBEntry, KeyTags}
import RocksDBStats.DbHistogramExt
import org.rocksdb.{ColumnFamilyHandle, ReadOptions, RocksDB, RocksIterator}

import scala.annotation.tailrec
import scala.util.Using

class ReadOnlyDB(db: RocksDB, readOptions: ReadOptions) {
  def get[V](key: Key[V]): V = {
    val bytes = db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    key.parse(bytes)
  }

  def getOpt[V](key: Key[V]): Option[V] = {
    val bytes = db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    if (bytes == null) None else Some(key.parse(bytes))
  }

  def multiGetOpt[V](keys: Seq[Key[Option[V]]], valBufferSize: Int): Seq[Option[V]] =
    db.multiGetOpt(readOptions, keys, valBufferSize)

  def multiGet[V](keys: Seq[Key[V]], valBufferSize: Int): Seq[Option[V]] =
    db.multiGet(readOptions, keys, valBufferSize)

  def multiGetOpt[V](keys: Seq[Key[Option[V]]], valBufSizes: Seq[Int]): Seq[Option[V]] =
    db.multiGetOpt(readOptions, keys, valBufSizes)

  def multiGetInts(keys: Seq[Key[Int]]): Seq[Option[Int]] =
    db.multiGetInts(readOptions, keys.map(_.keyBytes))

  def has[V](key: Key[V]): Boolean = {
    val bytes = db.get(readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    bytes != null
  }

  def newIterator: RocksIterator = db.newIterator(readOptions.setTotalOrderSeek(true))

  def newPrefixIterator: RocksIterator = db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))

  def iterateOverPrefix(tag: KeyTags.KeyTag)(f: DBEntry => Unit): Unit = iterateOverPrefix(tag.prefixBytes)(f)

  def iterateOverPrefix(prefix: Array[Byte])(f: DBEntry => Unit): Unit = {
    @tailrec
    def loop(iter: RocksIterator): Unit = {
      val key = iter.key()
      if (iter.isValid) {
        f(Maps.immutableEntry(key, iter.value()))
        iter.next()
        loop(iter)
      } else ()
    }

    Using.resource(db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))) { iter =>
      iter.seek(prefix)
      loop(iter)
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

  def iterateFrom(prefix: Array[Byte], first: Array[Byte], cfh: Option[ColumnFamilyHandle] = None)(f: DBEntry => Boolean): Unit = {
    Using.resource(db.newIterator(cfh.getOrElse(db.getDefaultColumnFamily), readOptions.setTotalOrderSeek(true))) { iter =>
      iter.seek(first)
      while (iter.isValid && iter.key().startsWith(prefix)) {
        f(Maps.immutableEntry(iter.key(), iter.value()))
        iter.next()
      }
    }
  }

  def prefixExists(prefix: Array[Byte]): Boolean = Using.resource(db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))) {
    iter =>
      iter.seek(prefix)
      iter.isValid
  }
}
