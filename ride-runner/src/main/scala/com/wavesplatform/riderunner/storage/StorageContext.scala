package com.wavesplatform.riderunner.storage

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.database.{DBExt, Key, RW, ReadOnlyDB}
import com.wavesplatform.riderunner.storage.StorageContext.{ReadOnly, ReadWrite}
import com.wavesplatform.riderunner.storage.persistent.LevelDbPersistentCaches.ReadOnlyDBOps
import org.rocksdb.RocksDB

trait Storage {
  def readOnly[T](f: ReadOnly => T): T
  def readWrite[T](f: ReadWrite => T): T
}

object Storage {
  def rocksDb(db: RocksDB): Storage = new Storage {
    override def readOnly[T](f: ReadOnly => T): T   = db.readOnly(x => f(new ReadOnly(x)))
    override def readWrite[T](f: ReadWrite => T): T = db.readWrite(x => f(new ReadWrite(x)))
  }
}

object StorageContext {
  class ReadOnly(val db: ReadOnlyDB)
  class ReadWrite(override val db: RW) extends ReadOnly(db) {
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
        val (removedHistory, updatedHistory) = history.partition(_ >= fromHeight) // TODO #13: binary search
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
