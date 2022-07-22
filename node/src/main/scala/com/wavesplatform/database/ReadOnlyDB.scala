package com.wavesplatform.database

import com.google.common.collect.Maps
import com.wavesplatform.metrics.LevelDBStats
import com.wavesplatform.metrics.LevelDBStats.DbHistogramExt
import org.rocksdb.{ReadOptions, RocksDB, RocksIterator}

import scala.annotation.tailrec
import scala.util.Using

class ReadOnlyDB(db: RocksDB, readOptions: ReadOptions) {
  def get[V](key: Key[V]): V = {
    val bytes = db.get(readOptions, key.keyBytes)
    LevelDBStats.read.recordTagged(key, bytes)
    key.parse(bytes)
  }

  def has[V](key: Key[V]): Boolean = {
    val bytes = db.get(readOptions, key.keyBytes)
    LevelDBStats.read.recordTagged(key, bytes)
    bytes != null
  }

  def newIterator: RocksIterator = db.newIterator(readOptions)

  def iterateOver(tag: KeyTags.KeyTag)(f: DBEntry => Unit): Unit = iterateOver(tag.prefixBytes)(f)

  def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Unit = {
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

  def prefixExists(prefix: Array[Byte]): Boolean = Using.resource(db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))) {
    iter =>
      iter.seek(prefix)
      iter.isValid
  }
}
