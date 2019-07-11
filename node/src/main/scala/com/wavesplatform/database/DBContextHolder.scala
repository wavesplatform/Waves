package com.wavesplatform.database

import java.io.IOException

import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.{DB, ReadOptions, Snapshot}

import scala.util.{Failure, Success, Try}

//noinspection ScalaStyle
private[database] class LocalDBContext(val db: DB) extends ScorexLogging {
  private[this] var counter             = 0
  private[this] var snapshotV: Snapshot = _
  private[this] var rw: RW              = _
  private[this] var ro: ReadOnlyDB      = _

  def snapshot(): Snapshot = {
    require(!isClosed)
    if (snapshotV == null) snapshotV = db.getSnapshot.ensuring(_ != null, "Snapshot is null")
    snapshotV
  }

  def readOnlyDB(): ReadOnlyDB = {
    require(!isClosed)

    if (rw != null) rw
    else if (ro != null) ro
    else {
      val snapshot = this.snapshot()
      ro = new ReadOnlyDB(db, new ReadOptions().snapshot(snapshot))
      ro
    }
  }

  def readWriteDB(): RW = {
    require(!isClosed)

    if (rw == null) {
      val snapshot    = this.snapshot()
      val readOptions = new ReadOptions().snapshot(snapshot)
      val batch       = db.createWriteBatch()
      rw = RW(db, readOptions, batch)
    }
    rw
  }

  def incCounter(): Unit = {
    counter += 1
  }

  def close(successful: Boolean = true): Unit = {
    if (isClosed) return

    counter -= 1
    if (counter == 0 || !successful) {
      if (successful) Option(rw).foreach(rw => db.write(rw.batch))
      Option(snapshotV).foreach(_.close())
      ro = null
      rw = null
      snapshotV = null
      counter = 0
    }
  }

  def isClosed: Boolean =
    counter <= 0

  override def finalize(): Unit = {
    if (!this.isClosed) {
      this.close(false)
      log.warn(s"DB context leaked: ${Integer.toHexString(System.identityHashCode(this))}")
    }
    super.finalize()
  }
}

private[database] class SynchronizedDBContext(db: DB) extends LocalDBContext(db) {
  override def snapshot(): Snapshot = synchronized(super.snapshot())

  override def readOnlyDB(): ReadOnlyDB = synchronized(super.readOnlyDB())

  override def readWriteDB(): RW = synchronized(super.readWriteDB())

  override def incCounter(): Unit = synchronized(super.incCounter())

  override def close(successful: Boolean): Unit = synchronized(super.close(successful))

  override def isClosed: Boolean = synchronized(super.isClosed)
}

private[database] final class DBContextHolder(val db: DB) {
  private[this] val tlContexts = new ThreadLocal[LocalDBContext]

  def openContext(): LocalDBContext = {
    val context = Option(tlContexts.get()) match {
      case Some(localContext) =>
        localContext
      case None =>
        val context = new LocalDBContext(db)
        tlContexts.set(context)
        context
    }

    context.incCounter()
    context
  }

  def withContext[T](f: LocalDBContext => T): T = {
    val context = openContext()
    val result  = Try(f(context))

    val closeErr = Try(context.close(successful = result.isSuccess)).failed
    if (context.isClosed) tlContexts.remove()

    result match {
      case Success(value) =>
        closeErr.map(new RuntimeException("LevelDB close exception", _)).foreach(throw _)
        value

      case Failure(exception) =>
        throw new IOException("LevelDB operation exception", exception)
    }
  }
}
