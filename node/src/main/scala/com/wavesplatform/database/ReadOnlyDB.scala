package com.wavesplatform.database

import com.google.common.collect.Maps
import com.wavesplatform.metrics.RocksDBStats
import com.wavesplatform.metrics.RocksDBStats.DbHistogramExt
import org.rocksdb.{ReadOptions, RocksDB, RocksIterator}

import scala.annotation.tailrec
import scala.util.Using

class ReadOnlyDB(db: RocksDB, readOptions: ReadOptions) {
  def get[V](key: Key[V]): V = {
    val bytes = db.get(readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    key.parse(bytes)
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

  def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Unit =
    Using.resource(db.newIterator(readOptions.setTotalOrderSeek(true))) { iter =>
      iter.seek(prefix)
      while (iter.isValid && iter.key().startsWith(prefix)) {
        f(Maps.immutableEntry(iter.key(), iter.value()))
        iter.next()
      }
    }

  def prefixExists(prefix: Array[Byte]): Boolean = Using.resource(db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))) {
    iter =>
      iter.seek(prefix)
      iter.isValid
  }
}
