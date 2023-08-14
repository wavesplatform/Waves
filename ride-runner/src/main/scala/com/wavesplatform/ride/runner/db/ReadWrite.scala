package com.wavesplatform.ride.runner.db

import com.google.common.primitives.Ints
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.caches.disk.KvPair
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

  // TODO #112 currentHeight, because height could >> currentHeight
  def writeHistoricalToDbOpt[T](
      historyKey: Key[Heights],
      dataOnHeightKey: Height => Key[Option[T]],
      height: Height,
      data: RemoteData[T]
  ): Unit = {
    // TODO duplicate
    val (preservedHistory, removedHistory) = splitHeightsAtRollback(height, getOpt(historyKey).getOrElse(Vector.empty))
    removedHistory.foreach(h => delete(dataOnHeightKey(h))) // TODO #123 Use deleteRange

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
    removedHistory.foreach(h => delete(dataOnHeightKey(h))) // TODO #123 Use deleteRange

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
      removedHistory.foreach(h => delete(dataOnHeightKey(h))) // TODO #123 Use deleteRange

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
      removedHistory.foreach(h => delete(dataOnHeightKey(h))) // TODO #123 Use deleteRange

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
  ): List[K] = {
    val affectedEntryKeys = mutable.Set.empty[K]

    // TODO #123 Use deleteRange
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
