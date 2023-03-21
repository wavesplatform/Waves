package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.database.rocksdb.{Key, RW, ReadOnlyDB}
import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.ride.runner.storage.persistent.DefaultPersistentCaches.ReadOnlyDBOps

object PersistentStorageContext {
  class ReadOnly(val db: ReadOnlyDB)
  class ReadWrite(override val db: RW) extends ReadOnly(db) {
    // TODO cleanup < 100
    def writeHistoricalToDbOpt[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[Option[T]],
        height: Int,
        data: RemoteData[T]
    ): Unit = {
      db.put(historyKey, db.getOpt(historyKey).getOrElse(Seq.empty).prepended(height))
      db.put(dataOnHeightKey(height), data.mayBeValue)
    }

    def writeHistoricalToDb[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[T],
        height: Int,
        data: RemoteData[T],
        default: => T
    ): Unit = {
      db.put(historyKey, db.getOpt(historyKey).getOrElse(Seq.empty).prepended(height))
      db.put(dataOnHeightKey(height), data.mayBeValue.getOrElse(default))
    }

    def removeAfterAndGetLatestExistedOpt[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[Option[T]],
        fromHeight: Int
    ): RemoteData[T] = {
      val history = db.getOpt(historyKey).getOrElse(Seq.empty)
      if (history.isEmpty) RemoteData.Unknown
      else {
        val (removedHistory, updatedHistory) = history.partition(_ >= fromHeight) // TODO #13 Better search for height
        db.put(historyKey, updatedHistory) // not deleting, because it will be added with a high probability
        removedHistory.foreach(h => db.delete(dataOnHeightKey(h)))

        updatedHistory.headOption match {
          case None    => RemoteData.Unknown
          case Some(h) => db.readFromDbOpt(dataOnHeightKey(h))
        }
      }
    }

    def removeAfterAndGetLatestExisted[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[T],
        fromHeight: Int
    ): RemoteData[T] = {
      val history = db.getOpt(historyKey).getOrElse(Seq.empty)
      if (history.isEmpty) RemoteData.Unknown
      else {
        val (removedHistory, updatedHistory) = history.partition(_ >= fromHeight) // TODO #13: binary search
        db.put(historyKey, updatedHistory) // not deleting, because it will be added with a high probability
        removedHistory.foreach(h => db.delete(dataOnHeightKey(h)))

        updatedHistory.headOption match {
          case None    => RemoteData.Unknown
          case Some(h) => db.readFromDb(dataOnHeightKey(h))
        }
      }
    }

    def writeToDb[T](dbKey: Key[Option[T]], data: RemoteData[T]): Unit = {
      if (data.loaded) db.put(dbKey, data.mayBeValue)
      else db.delete(dbKey)
    }
  }
}
