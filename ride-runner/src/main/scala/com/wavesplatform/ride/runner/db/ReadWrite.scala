package com.wavesplatform.ride.runner.db

import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.database.rocksdb.stats.RocksDBStats
import com.wavesplatform.database.rocksdb.stats.RocksDBStats.DbHistogramExt
import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.utils.OptimisticLockable
import org.rocksdb.*

class ReadWrite(db: RocksDB, readOptions: ReadOptions, batch: SynchronizedWriteBatch) extends ReadOnly(db, readOptions) with OptimisticLockable {
  def put[V](key: Key[V], value: V): Int = {
    val bytes = key.encode(value)
    RocksDBStats.write.recordTagged(key, bytes)
    batch.use(_.put(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes, bytes))
    bytes.length
  }

  def put(key: Array[Byte], value: Array[Byte]): Unit = batch.use(_.put(key, value))

  def update[V](key: Key[V])(f: V => V): Unit = put(key, f(get(key)))

  def delete(key: Array[Byte]): Unit = batch.use(_.delete(key))

  def delete[V](key: Key[V]): Unit =
    batch.use(_.delete(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes))

  def filterHistory(key: Key[Seq[Int]], heightToRemove: Int): Unit = {
    val newValue = get(key).filterNot(_ == heightToRemove)
    if (newValue.nonEmpty) put(key, newValue)
    else delete(key)
  }

  def writeToDb[T](dbKey: Key[Option[T]], data: RemoteData[T]): Unit = {
    if (data.loaded) put(dbKey, data.mayBeValue)
    else delete(dbKey)
  }

  // TODO cleanup < 100
  def writeHistoricalToDbOpt[T](
      historyKey: Key[Seq[Int]],
      dataOnHeightKey: Int => Key[Option[T]],
      height: Int,
      data: RemoteData[T]
  ): Unit = {
    put(historyKey, getOpt(historyKey).getOrElse(Seq.empty).prepended(height))
    put(dataOnHeightKey(height), data.mayBeValue)
  }

  def writeHistoricalToDb[T](
      historyKey: Key[Seq[Int]],
      dataOnHeightKey: Int => Key[T],
      height: Int,
      data: RemoteData[T],
      default: => T
  ): Unit = {
    put(historyKey, getOpt(historyKey).getOrElse(Seq.empty).prepended(height))
    put(dataOnHeightKey(height), data.mayBeValue.getOrElse(default))
  }

  def removeAfterAndGetLatestExistedOpt[T](
      historyKey: Key[Seq[Int]],
      dataOnHeightKey: Int => Key[Option[T]],
      fromHeight: Int
  ): RemoteData[T] = {
    val history = getOpt(historyKey).getOrElse(Seq.empty)
    if (history.isEmpty) RemoteData.Unknown
    else {
      val (removedHistory, updatedHistory) = history.partition(_ >= fromHeight) // TODO #13 Better search for height
      put(historyKey, updatedHistory) // not deleting, because it will be added with a high probability
      removedHistory.foreach(h => delete(dataOnHeightKey(h)))

      updatedHistory.headOption match {
        case None    => RemoteData.Unknown
        case Some(h) => readFromDbOpt(dataOnHeightKey(h))
      }
    }
  }

  def removeAfterAndGetLatestExisted[T](
      historyKey: Key[Seq[Int]],
      dataOnHeightKey: Int => Key[T],
      fromHeight: Int
  ): RemoteData[T] = {
    val history = getOpt(historyKey).getOrElse(Seq.empty)
    if (history.isEmpty) RemoteData.Unknown
    else {
      val (removedHistory, updatedHistory) = history.partition(_ >= fromHeight) // TODO #13: binary search
      put(historyKey, updatedHistory) // not deleting, because it will be added with a high probability
      removedHistory.foreach(h => delete(dataOnHeightKey(h)))

      updatedHistory.headOption match {
        case None    => RemoteData.Unknown
        case Some(h) => readFromDb(dataOnHeightKey(h))
      }
    }
  }
}
