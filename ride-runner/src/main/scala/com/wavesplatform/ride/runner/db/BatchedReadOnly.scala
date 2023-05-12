package com.wavesplatform.ride.runner.db

import com.google.common.collect.Maps
import com.wavesplatform.database.DBEntry
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.rocksdb.stats.RocksDBStats.DbHistogramExt
import org.rocksdb.*

import scala.annotation.tailrec
import scala.util.Using

class BatchedReadOnly(db: RocksDB, readOptions: ReadOptions) extends ReadOnly {
  override def get[V](key: Key[V]): V = {
    val bytes = db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    key.parse(bytes)
  }

  override def getOpt[V](key: Key[V]): Option[V] = {
    val bytes = db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    if (bytes == null) None else Some(key.parse(bytes))
  }

  override def has[V](key: Key[V]): Boolean = {
    val bytes = db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    bytes != null
  }

  /** Iterate from seekKeyBytes with a prefix of short size
    * @see
    *   RideRocksDb#newColumnFamilyOptions useFixedLengthPrefixExtractor
    */
  override def iterateOverPrefixContinue(seekKeyBytes: Array[Byte], cfh: Option[ColumnFamilyHandle] = None)(f: DBEntry => Boolean): Unit = {
    @tailrec
    def loop(iter: RocksIterator): Unit = if (iter.isValid) {
      val key = iter.key()
      if (f(Maps.immutableEntry(key, iter.value()))) {
        iter.next()
        loop(iter)
      }
    }

    Using.resource(mkPrefixedIterator(cfh, totalOrder = true)) { iter =>
      iter.seek(seekKeyBytes)
      loop(iter)
    }
  }

  override def prefixExists(prefix: Array[Byte]): Boolean =
    Using.resource(mkPrefixedIterator(None, totalOrder = false)) { iter =>
      iter.seek(prefix)
      iter.isValid
    }

  private def mkPrefixedIterator(cfh: Option[ColumnFamilyHandle], totalOrder: Boolean) = db.newIterator(
    cfh.getOrElse(db.getDefaultColumnFamily),
    readOptions.setTotalOrderSeek(totalOrder).setPrefixSameAsStart(true)
  )
}
