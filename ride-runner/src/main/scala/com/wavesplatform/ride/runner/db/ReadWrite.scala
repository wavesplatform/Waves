package com.wavesplatform.ride.runner.db

import com.google.common.primitives.Ints
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.caches.disk.KvHistoryPair
import com.wavesplatform.ride.runner.db.Heights.{splitHeightsAt, splitHeightsAtRollback}
import com.wavesplatform.state.Height
import org.rocksdb.ColumnFamilyHandle

import scala.collection.mutable

trait ReadWrite extends ReadOnly {
  def put[V](key: Key[V], value: V): Int
  def put(key: Array[Byte], value: Array[Byte], cfh: Option[ColumnFamilyHandle]): Unit
  def update[V](key: Key[V], default: => V)(f: V => V): Unit
  def delete(key: Array[Byte], cfh: Option[ColumnFamilyHandle]): Unit
  def delete[V](key: Key[V]): Unit

  def writeToDb[T](key: Key[Option[T]], data: RemoteData[T]): Unit =
    if (data.loaded) put(key, data.mayBeValue)
    else delete(key)

  def writeHistoricalToDbOpt[K, V](k: K, kvHistoryPair: KvHistoryPair[K, Option[V]], height: Height, data: RemoteData[V]): Unit = {
    def dataOnHeightKey(h: Height) = kvHistoryPair.kvPairAtHeight.at((h, k))
    val historyKey                 = kvHistoryPair.at(k)

    // TODO duplicate
    val (preservedHistory, removedHistory) = splitHeightsAtRollback(height, getOpt(historyKey).getOrElse(Vector.empty))
    removedHistory.foreach(h => delete(dataOnHeightKey(h)))

    put(historyKey, preservedHistory.prepended(height))
    put(dataOnHeightKey(height), data.mayBeValue)
  }

  def writeHistoricalToDb[K, V](k: K, kvHistoryPair: KvHistoryPair[K, V], height: Height, data: V): Unit = {
    def dataOnHeightKey(h: Height) = kvHistoryPair.kvPairAtHeight.at((h, k))
    val historyKey                 = kvHistoryPair.at(k)

    // TODO duplicate
    val (preservedHistory, removedHistory) = splitHeightsAtRollback(height, getOpt(historyKey).getOrElse(Vector.empty))
    removedHistory.foreach(h => delete(dataOnHeightKey(h)))

    put(historyKey, preservedHistory.prepended(height))
    put(dataOnHeightKey(height), data)
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

  def removeFromAndGetLatestExisted[K, V](k: K, kvHistoryPair: KvHistoryPair[K, V], fromHeight: Height): RemoteData[V] = {
    def dataOnHeightKey(h: Height) = kvHistoryPair.kvPairAtHeight.at((h, k))
    val historyKey                 = kvHistoryPair.at(k)

    // TODO duplicate
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

  def removeFrom[K, V](historyKey: KvHistoryPair[K, V], fromHeight: Height): List[K] = {
    val affectedEntryKeys = mutable.Set.empty[K]
    val entriesKey        = historyKey.kvPairAtHeight

    iterateOverPrefix(entriesKey.prefixBytes ++ Ints.toByteArray(fromHeight), entriesKey.columnFamilyHandle) { e =>
      val rawKey        = e.getKey
      val keyWithHeight = entriesKey.parseKey(rawKey)
      val (_, key)      = keyWithHeight
      delete(rawKey, entriesKey.columnFamilyHandle)

      affectedEntryKeys.add(key)
    }

    affectedEntryKeys.foreach { k =>
      update(historyKey.at(k), EmptyHeights)(_.dropWhile(_ >= fromHeight))
    }

    affectedEntryKeys.toList
  }
}
