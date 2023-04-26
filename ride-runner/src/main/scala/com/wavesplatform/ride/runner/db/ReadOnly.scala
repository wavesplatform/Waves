package com.wavesplatform.ride.runner.db

import com.google.common.collect.Maps
import com.wavesplatform.database.DBEntry
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.rocksdb.stats.RocksDBStats.DbHistogramExt
import com.wavesplatform.ride.runner.db.ReadOnly.DbPair
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

  def collect[KeyT, ValueT, T](
      kvPair: KvPair[KeyT, ValueT],
      seekKey: KeyT
  )(f: DbPair[KeyT, ValueT] => IterableOnce[T]): List[T] = {
    val r = List.newBuilder[T]
    iterateOverPrefix(kvPair.at(seekKey)) { p =>
      r.addAll(f(new DbPair(kvPair, p)))
    }
    r.result()
  }

  def collectKeys[KeyT](kvPair: KvPair[KeyT, ?])(implicit ctx: ReadOnly): List[KeyT] =
    collectKeys(kvPair.prefixBytes)(ctx, kvPair.prefixedKeyAsBytes)

  def collectKeys[KeyT](
      prefixBytes: Array[Byte],
      cfh: Option[ColumnFamilyHandle] = None
  )(implicit
      ctx: ReadOnly,
      keyAsBytes: AsBytes[KeyT]
  ): List[KeyT] = {
    var r = List.empty[KeyT]
    ctx.iterateOverPrefix(prefixBytes, cfh) { dbEntry =>
      val key = keyAsBytes.read(dbEntry.getKey)
      r = key :: r
    }
    r
  }

  def iterateOverPrefix[KeyT, ValueT](
      kvPair: KvPair[KeyT, ValueT],
      seekKey: KeyT
  )(f: DbPair[KeyT, ValueT] => Unit): Unit = iterateOverPrefixContinue(kvPair, seekKey) { p => f(p); true }

  def iterateOverPrefix[ValueT](seekKey: Key[ValueT])(f: DBEntry => Unit): Unit =
    iterateOverPrefixContinue(seekKey.keyBytes, seekKey.columnFamilyHandle) { p =>
      f(p)
      true
    }

  def iterateOverPrefix(keyBytes: Array[Byte], cfh: Option[ColumnFamilyHandle] = None)(f: DBEntry => Unit): Unit =
    iterateOverPrefixContinue(keyBytes, cfh) { x =>
      f(x)
      true
    }

  def iterateOverPrefixContinue[KeyT, ValueT](
      kvPair: KvPair[KeyT, ValueT],
      seekKey: KeyT
  )(f: DbPair[KeyT, ValueT] => Boolean): Unit = iterateOverPrefixContinue(kvPair.at(seekKey).keyBytes, kvPair.columnFamilyHandle) { p =>
    f(new DbPair(kvPair, p))
  }

  /** Iterate from seekKeyBytes with a prefix of short size
    * @see
    *   RideRocksDb#newColumnFamilyOptions useFixedLengthPrefixExtractor
    */
  def iterateOverPrefixContinue(seekKeyBytes: Array[Byte], cfh: Option[ColumnFamilyHandle] = None)(f: DBEntry => Boolean): Unit = {
    @tailrec
    def loop(iter: RocksIterator): Unit = if (iter.isValid) {
      val key = iter.key()
      if (f(Maps.immutableEntry(key, iter.value()))) {
        iter.next()
        loop(iter)
      }
    }

    Using.resource(db.newIterator(cfh.getOrElse(db.getDefaultColumnFamily), readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))) {
      iter =>
        iter.seek(seekKeyBytes)
        loop(iter)
    }
  }

  def prefixExists(prefix: Array[Byte]): Boolean = Using.resource(db.newIterator(readOptions.setTotalOrderSeek(true))) { iter =>
    iter.seek(prefix)
    iter.isValid
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

object ReadOnly {
  class DbPair[KeyT, ValueT](kvPair: KvPair[KeyT, ValueT], val dbEntry: DBEntry) {
    lazy val key: KeyT     = kvPair.parseKey(dbEntry.getKey)
    lazy val value: ValueT = kvPair.parseValue(dbEntry.getValue)
  }
}
