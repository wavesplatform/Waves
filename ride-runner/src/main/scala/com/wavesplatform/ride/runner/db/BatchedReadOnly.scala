package com.wavesplatform.ride.runner.db

import com.google.common.collect.Maps
import com.wavesplatform.database.DBEntry
import com.wavesplatform.database.Key
import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.rocksdb.stats.RocksDBStats.DbHistogramExt
import org.rocksdb.*

import scala.annotation.tailrec
import scala.util.Using

class BatchedReadOnly(protected override val db: RocksDB, readOptions: ReadOptions) extends ReadOnly with HasDb {
  override def get[V](key: Key[V]): V = {
    val bytes = db.get(getCFH(key), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    key.parse(bytes)
  }

  override def getOpt[V](key: Key[V]): Option[V] = {
    val bytes = db.get(getCFH(key), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    if (bytes == null) None else Some(key.parse(bytes))
  }

  override def has[V](key: Key[V]): Boolean = {
    val bytes = db.get(getCFH(key), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    bytes != null
  }

  /** Iterate from seekKeyBytes with a prefix of short size
    * @see
    *   RideRocksDb#newColumnFamilyOptions useFixedLengthPrefixExtractor
    */
  override def iterateOverPrefixContinue(seekKeyBytes: Array[Byte], cfh: Option[ColumnFamilyHandle])(f: DBEntry => Boolean): Unit = {
    @tailrec
    def loop(iter: RocksIterator): Unit = if (iter.isValid) {
      val key = iter.key()
      if (f(Maps.immutableEntry(key, iter.value()))) {
        iter.next()
        loop(iter)
      }
    }

    usePrefixedIterator(totalOrder = true, cfh) { iter =>
      iter.seek(seekKeyBytes)
      loop(iter)
    }
  }

  override def prefixExists(prefix: Array[Byte], cfh: Option[ColumnFamilyHandle]): Boolean = usePrefixedIterator(totalOrder = false, cfh) { iter =>
    iter.seek(prefix)
    iter.isValid
  }

  private def usePrefixedIterator[T](totalOrder: Boolean, cfh: Option[ColumnFamilyHandle])(f: RocksIterator => T): T =
    Using.resource(
      db.newIterator(
        cfh.getOrElse(db.getDefaultColumnFamily),
        readOptions.setTotalOrderSeek(totalOrder).setPrefixSameAsStart(true)
      )
    )(f)
}
