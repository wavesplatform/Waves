package com.wavesplatform.ride.runner.db

import com.google.common.collect.Maps
import com.wavesplatform.database.DBEntry
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.rocksdb.stats.RocksDBStats.DbHistogramExt
import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.ride.runner.storage.persistent.{AsBytes, KvPair}
import com.wavesplatform.state.Height
import org.rocksdb.*

import scala.annotation.tailrec
import scala.util.Using

// TODO make an interface to easier mocking in tests?
class ReadOnly(db: RocksDB, readOptions: ReadOptions) {
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

  def has[V](key: Key[V]): Boolean = {
    val bytes = db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes)
    RocksDBStats.read.recordTagged(key, bytes)
    bytes != null
  }

  private def newPrefixIterator: RocksIterator = db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))

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

    Using.resource(newPrefixIterator) { iter =>
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

  def iterateFrom(prefix: Array[Byte], first: Array[Byte], cfh: Option[ColumnFamilyHandle] = None)(f: DBEntry => Boolean): Unit =
    Using.resource(db.newIterator(cfh.getOrElse(db.getDefaultColumnFamily), readOptions.setTotalOrderSeek(true))) { iter =>
      iter.seek(first)
      var goNext = true
      while (goNext && iter.isValid && iter.key().startsWith(prefix)) {
        goNext = f(Maps.immutableEntry(iter.key(), iter.value()))
        iter.next()
      }
    }

  def prefixExists(prefix: Array[Byte]): Boolean = Using.resource(db.newIterator(readOptions.setTotalOrderSeek(true))) { iter =>
    iter.seek(prefix)
    iter.isValid
  }

  def readKeys[KeyT](kvPair: KvPair[KeyT, ?])(implicit ctx: ReadOnly): List[KeyT] =
    readKeys(kvPair.prefixBytes)(ctx, kvPair.prefixedKeyAsBytes)

  def readKeys[KeyT](prefixBytes: Array[Byte])(implicit ctx: ReadOnly, keyAsBytes: AsBytes[KeyT]): List[KeyT] = {
    var r = List.empty[KeyT]
    ctx.iterateOverPrefix(prefixBytes) { dbEntry =>
      val key = keyAsBytes.read(dbEntry.getKey)
      r = key :: r
    }
    r
  }

  def readHistoricalFromDbOpt[T](
      historyKey: Key[Heights],
      dataOnHeightKey: Height => Key[Option[T]],
      maxHeight: Height
  ): RemoteData[T] = {
    val height = getOpt(historyKey).getOrElse(Vector.empty).find(_ <= maxHeight) // ordered from the newest to the oldest
    height
      .flatMap(height => getOpt(dataOnHeightKey(height)))
      .fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.loaded)
  }

  def readHistoricalFromDb[T](
      historyKey: Key[Heights],
      dataOnHeightKey: Height => Key[T],
      maxHeight: Height
  ): RemoteData[T] = {
    val height = getOpt(historyKey).getOrElse(Vector.empty).find(_ <= maxHeight) // ordered from the newest to the oldest
    height
      .map(height => get(dataOnHeightKey(height)))
      .fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.Cached(_))
  }

  def getRemoteDataOpt[T](key: Key[Option[T]]): RemoteData[T] = getOpt(key).fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.loaded)
  def getRemoteData[T](key: Key[T]): RemoteData[T]            = getOpt(key).fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.Cached(_))
}
