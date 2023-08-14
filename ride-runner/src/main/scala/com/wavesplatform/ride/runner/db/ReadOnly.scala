package com.wavesplatform.ride.runner.db

import com.wavesplatform.database.DBEntry
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.caches.disk.{AsBytes, KvPair}
import com.wavesplatform.state.Height
import org.rocksdb.ColumnFamilyHandle

trait ReadOnly {

  def get[V](key: Key[V]): V

  def getOpt[V](key: Key[V]): Option[V]

  def has[V](key: Key[V]): Boolean

  /** Iterate from seekKeyBytes with a prefix of short size
    *
    * @see
    *   RideRocksDb#newColumnFamilyOptions useFixedLengthPrefixExtractor
    */
  def iterateOverPrefixContinue(seekKeyBytes: Array[Byte], cfh: Option[ColumnFamilyHandle])(f: DBEntry => Boolean): Unit

  def prefixExists(prefix: Array[Byte], cfh: Option[ColumnFamilyHandle]): Boolean

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

  def collectKeys[KeyT](kvPair: KvPair[KeyT, ?]): List[KeyT] = collectKeys(kvPair.prefixBytes, kvPair.columnFamilyHandle)(kvPair.prefixedKeyAsBytes)

  def collectKeys[KeyT](prefixBytes: Array[Byte], cfh: Option[ColumnFamilyHandle])(implicit keyAsBytes: AsBytes[KeyT]): List[KeyT] = {
    var r = List.empty[KeyT]
    iterateOverPrefix(prefixBytes, cfh) { dbEntry =>
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

  def iterateOverPrefix(keyBytes: Array[Byte], cfh: Option[ColumnFamilyHandle])(f: DBEntry => Unit): Unit =
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

  def getRemoteDataOpt[T](key: Key[Option[T]]): RemoteData[T] = getOpt(key).fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.loaded)

  def getRemoteData[T](key: Key[T]): RemoteData[T] = getOpt(key).fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.Cached(_))

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
}
