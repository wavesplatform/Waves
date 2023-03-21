package com.wavesplatform.ride.runner.storage.persistent

import cats.syntax.option.*
import com.wavesplatform.database.rocksdb.{DBExt, RW, ReadOnlyDB}
import com.wavesplatform.ride.runner.storage.persistent.PersistentStorageContext.{ReadOnly, ReadWrite}
import monix.eval.Task
import org.rocksdb.*

import scala.util.Using

trait PersistentStorage {
  def asyncReadOnly[T](f: ReadOnly => Task[T]): Task[T]
  def readOnly[T](f: ReadOnly => T): T

  def asyncReadWrite[T](f: ReadWrite => Task[T]): Task[T]
  def readWrite[T](f: ReadWrite => T): T
}

// TODO #98 Wrapped Task.bracket instead of vars
object PersistentStorage {
  def rocksDb(db: RocksDB): PersistentStorage = new PersistentStorage {
    override def asyncReadOnly[T](f: ReadOnly => Task[T]): Task[T] = {
      @volatile var snapshot = none[Snapshot]
      @volatile var options  = none[ReadOptions]
      Task {
        val s = db.getSnapshot
        val o = new ReadOptions().setSnapshot(s).setVerifyChecksums(false)
        snapshot = s.some
        options = o.some
        new ReadOnly(new ReadOnlyDB(db, o))
      }.bracket(f) { _ =>
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
      }.bracket(f) { _ =>
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
  }
}

// TODO #98 Wrapped Task.bracket instead of vars
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
