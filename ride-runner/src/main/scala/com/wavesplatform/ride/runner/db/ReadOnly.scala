package com.wavesplatform.ride.runner.db

import com.wavesplatform.database.DBEntry
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.caches.disk.{KvHistoryPair, KvPair}
import com.wavesplatform.state.Height
import org.rocksdb.ColumnFamilyHandle
import shapeless.=:!=

import scala.annotation.unused

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
  )(f: DbPair[KeyT, ValueT] => Boolean): Unit =
    iterateOverPrefixContinue(kvPair.at(seekKey).keyBytes, kvPair.columnFamilyHandle) { p =>
      f(new DbPair(kvPair, p))
    }

  def getRemoteDataOpt[T](key: Key[Option[T]]): RemoteData[T] = RemoteData(getOpt(key))

  def readFromDb[K, V](k: K, kvHistoryPair: KvHistoryPair[K, V], maxHeight: Height)(implicit @unused ev: V =:!= Option[?]): RemoteData[V] =
    readFromDbRaw(k, kvHistoryPair, maxHeight).fold(RemoteData.unknown[V])(RemoteData.Cached(_))

  def readFromDb[K, V](k: K, kvHistoryPair: KvHistoryPair[K, Option[V]], maxHeight: Height): RemoteData[V] =
    readFromDbRaw(k, kvHistoryPair, maxHeight).fold(RemoteData.unknown[V])(RemoteData.loaded)

  private def readFromDbRaw[K, V](k: K, kvHistoryPair: KvHistoryPair[K, V], maxHeight: Height): Option[V] =
    getOpt(kvHistoryPair.at(k))
      .getOrElse(Vector.empty)
      .find(_ <= maxHeight) // the recent is in the front
      .map(h => get(kvHistoryPair.kvPairAtHeight.at((h, k))))
}
