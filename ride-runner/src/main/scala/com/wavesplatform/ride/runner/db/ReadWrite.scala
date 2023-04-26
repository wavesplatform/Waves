package com.wavesplatform.ride.runner.db

import com.google.common.primitives.Ints
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.rocksdb.stats.RocksDBStats.DbHistogramExt
import com.wavesplatform.ride.runner.db.Heights.{splitHeightsAt, splitHeightsAtRollback}
import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.ride.runner.storage.persistent.KvPair
import com.wavesplatform.state.Height
import com.wavesplatform.utils.OptimisticLockable
import org.rocksdb.*

import scala.collection.mutable

class ReadWrite(db: RocksDB, readOptions: ReadOptions, batch: SynchronizedWriteBatch) extends ReadOnly(db, readOptions) with OptimisticLockable {
  def put[V](key: Key[V], value: V): Int = {
    val bytes = key.encode(value)
    RocksDBStats.write.recordTagged(key, bytes)
    batch.use(_.put(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes, bytes))
    bytes.length
  }

  def put(key: Array[Byte], value: Array[Byte]): Unit = batch.use(_.put(key, value))

  def update[V](key: Key[V], default: => V)(f: V => V): Unit = put(key, f(getOpt(key).getOrElse(default)))

  def delete(key: Array[Byte]): Unit = batch.use(_.delete(key))

  def delete[V](key: Key[V]): Unit =
    batch.use(_.delete(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes))

  def writeToDb[T](key: Key[Option[T]], data: RemoteData[T]): Unit =
    if (data.loaded) put(key, data.mayBeValue)
    else delete(key)

  // TODO #112 currentHeight, because height could >> currentHeight
  def writeHistoricalToDbOpt[T](
      historyKey: Key[Heights],
      dataOnHeightKey: Height => Key[Option[T]],
      height: Height,
      data: RemoteData[T]
  ): Unit = {
    // TODO duplicate
    val (preservedHistory, removedHistory) = splitHeightsAtRollback(height, getOpt(historyKey).getOrElse(Vector.empty))
    removedHistory.foreach(h => delete(dataOnHeightKey(h)))

    put(historyKey, preservedHistory.prepended(height))
    put(dataOnHeightKey(height), data.mayBeValue)
  }

  def writeHistoricalToDb[T](
      historyKey: Key[Heights],
      dataOnHeightKey: Height => Key[T],
      height: Height,
      data: RemoteData[T],
      default: => T
  ): Unit = {
    val (preservedHistory, removedHistory) = splitHeightsAtRollback(height, getOpt(historyKey).getOrElse(Vector.empty))
    removedHistory.foreach(h => delete(dataOnHeightKey(h)))

    put(historyKey, preservedHistory.prepended(height))
    put(dataOnHeightKey(height), data.mayBeValue.getOrElse(default))
  }

  def removeFromAndGetLatestExistedOpt[T](
      historyKey: Key[Heights],
      dataOnHeightKey: Height => Key[Option[T]],
      fromHeight: Height
  ): RemoteData[T] = {
    val history = getOpt(historyKey).getOrElse(Vector.empty)
    if (history.isEmpty) RemoteData.Unknown
    else {
      val (preservedHistory, removedHistory) = splitHeightsAt(fromHeight, history)
      removedHistory.foreach(h => delete(dataOnHeightKey(h)))

      preservedHistory.headOption match {
        case None =>
          delete(historyKey)
          RemoteData.Unknown

        case Some(h) =>
          put(historyKey, preservedHistory)
          getRemoteDataOpt(dataOnHeightKey(h))
      }
    }
  }

  // TODO duplicate
  def removeFromAndGetLatestExisted[T](
      historyKey: Key[Heights],
      dataOnHeightKey: Height => Key[T],
      fromHeight: Height
  ): RemoteData[T] = {
    val history = getOpt(historyKey).getOrElse(Vector.empty)
    if (history.isEmpty) RemoteData.Unknown
    else {
      val (preservedHistory, removedHistory) = splitHeightsAt(fromHeight, history)
      removedHistory.foreach(h => delete(dataOnHeightKey(h)))

      preservedHistory.headOption match {
        case None =>
          delete(historyKey)
          RemoteData.Unknown

        case Some(h) =>
          put(historyKey, preservedHistory)
          getRemoteData(dataOnHeightKey(h))
      }
    }
  }

  def removeFrom[K, V](
      historyKey: KvPair[K, Heights],
      entriesKey: KvPair[(Height, K), V],
      fromHeight: Height
  )(implicit ctx: ReadWrite): List[K] = {
    val affectedEntryKeys = mutable.Set.empty[K]

    ctx.iterateOverPrefix(entriesKey.prefixBytes ++ Ints.toByteArray(fromHeight)) { e =>
      val rawKey        = e.getKey
      val keyWithHeight = entriesKey.parseKey(rawKey)
      val (_, key)      = keyWithHeight
      ctx.delete(rawKey)

      affectedEntryKeys.add(key)
    }

    affectedEntryKeys.foreach { k =>
      ctx.update(historyKey.at(k), EmptyHeights)(_.dropWhile(_ >= fromHeight))
    }

    affectedEntryKeys.toList
  }
}
