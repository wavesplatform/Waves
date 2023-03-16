package com.wavesplatform.riderunner.storage

import cats.syntax.option.*
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.database.{DBExt, Key, RW, ReadOnlyDB}
import com.wavesplatform.riderunner.storage.StorageContext.{ReadOnly, ReadWrite}
import com.wavesplatform.riderunner.storage.persistent.LevelDbPersistentCaches.ReadOnlyDBOps
import monix.eval.Task
import org.rocksdb.*

import scala.util.Using

trait Storage {
  def asyncReadOnly[T](f: ReadOnly => Task[T]): Task[T]
  def readOnly[T](f: ReadOnly => T): T

  def asyncReadWrite[T](f: ReadWrite => Task[T]): Task[T]
  def readWrite[T](f: ReadWrite => T): T

  def mkReadWriteHandle: ReadWriteHandle
}

class ReadWriteHandle(db: RocksDB) extends AutoCloseable {
  val snapshot    = db.getSnapshot
  val readOptions = new ReadOptions().setSnapshot(snapshot).setVerifyChecksums(false)
  val batch       = new WriteBatch()

  override def close(): Unit = {
    try Using.resource(new WriteOptions().setSync(false).setDisableWAL(true))(db.write(_, batch))
    finally {}

    try readOptions.close()
    finally {}

    try snapshot.close()
    finally {}
  }
}

object Storage {
  def rocksDb(db: RocksDB): Storage = new Storage {
    // TODO bracket
    override def asyncReadOnly[T](f: ReadOnly => Task[T]): Task[T] = {
      @volatile var snapshot = none[Snapshot]
      @volatile var options  = none[ReadOptions]
      Task {
        val s = db.getSnapshot
        val o = new ReadOptions().setSnapshot(s).setVerifyChecksums(false)
        snapshot = s.some
        options = o.some
        new ReadOnly(new ReadOnlyDB(db, o))
      }
        .flatMap(f)
        .doOnFinish { _ =>
          Task {
            try options.foreach(_.close())
            finally {}
            try snapshot.foreach(_.close())
            finally {}
          }
        }
    }

    override def readOnly[T](f: ReadOnly => T): T = db.readOnly(x => f(new ReadOnly(x)))

    override def asyncReadWrite[T](f: ReadWrite => Task[T]): Task[T] = {
      @volatile var snapshot    = none[Snapshot]
      @volatile var readOptions = none[ReadOptions]
      @volatile var batch       = none[WriteBatch]
      Task {
        val s  = db.getSnapshot
        val ro = new ReadOptions().setSnapshot(s).setVerifyChecksums(false)
        val b  = new WriteBatch()
        snapshot = s.some
        readOptions = ro.some
        batch = b.some
        new ReadWrite(new RW(db, ro, b))
      }
        .flatMap(f)
        .doOnFinish { _ =>
          Task {
            try
              batch.foreach { b =>
                Using.resource(new WriteOptions().setSync(false).setDisableWAL(true)) { wo =>
                  db.write(wo, b)
                }
              }
            finally {}

            try readOptions.foreach(_.close())
            finally {}

            try snapshot.foreach(_.close())
            finally {}
          }
        }
    }

    override def readWrite[T](f: ReadWrite => T): T = db.readWrite(x => f(new ReadWrite(x)))

    override def mkReadWriteHandle: ReadWriteHandle = new ReadWriteHandle(db)
  }
}

object StorageContext {
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
