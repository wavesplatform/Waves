package com.wavesplatform.ride.runner.db

import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import org.rocksdb.*
import shapeless.<:!<

import scala.util.Using

trait RideDbAccess {
  def readOnly[T](f: ReadOnly => T)(implicit ev: T <:!< Task[?]): T
  def readWrite[T](f: ReadWrite => T)(implicit ev: T <:!< Task[?]): T
  def asyncReadOnly[T](f: ReadOnly => Task[T]): Task[T]
  def asyncReadWrite[T](f: ReadWrite => Task[T]): Task[T]
}

object RideDbAccess {
  def fromRocksDb(db: RocksDB): RideDbAccess = new RideDbAccess with ScorexLogging {
    override def readOnly[T](f: ReadOnly => T)(implicit ev: T <:!< Task[?]): T =
      Using.resource(db.getSnapshot) { s =>
        Using.resource(new ReadOptions().setSnapshot(s).setVerifyChecksums(false)) { ro =>
          f(new ReadOnly(db, ro))
        }
      }((resource: Snapshot) => db.releaseSnapshot(resource))

    override def readWrite[T](f: ReadWrite => T)(implicit ev: T <:!< Task[?]): T =
      Using.resource(db.getSnapshot) { s =>
        Using.resource(new ReadOptions().setSnapshot(s).setVerifyChecksums(false)) { ro =>
          Using.resource(new WriteOptions().setSync(false).setDisableWAL(true)) { wo =>
            Using.resource(SynchronizedWriteBatch()) { wb =>
              val r = f(new ReadWrite(db, ro, wb))
              db.write(wo, wb.underlying)
              r
            }
          }
        }
      }((resource: Snapshot) => db.releaseSnapshot(resource))

    override def asyncReadOnly[T](f: ReadOnly => Task[T]): Task[T] =
      withReadOption { ro => f(new ReadOnly(db, ro)) }

    override def asyncReadWrite[T](f: ReadWrite => Task[T]): Task[T] =
      withReadOption { ro =>
        withResource(new WriteOptions().setSync(false).setDisableWAL(true))(_.close()) { wo =>
          withResource(SynchronizedWriteBatch())(_.close()) { wb =>
            f(new ReadWrite(db, ro, wb)).tapEval { _ =>
              Task.evalAsync(db.write(wo, wb.underlying))
            }
          }
        }
      }

    private def withReadOption[T](use: ReadOptions => Task[T]): Task[T] =
      // Snapshot.close does nothing, see https://github.com/facebook/rocksdb/blob/601320164b41643e39851245ee90a90caa61d311/java/src/main/java/org/rocksdb/Snapshot.java#L31
      withResource(db.getSnapshot)(db.releaseSnapshot) { s =>
        withResource(new ReadOptions().setSnapshot(s).setVerifyChecksums(false))(_.close())(use)
      }

    private def withResource[R, T](acquire: => R)(release: R => Unit)(use: R => Task[T]): Task[T] =
      Task
        .evalAsync(acquire) // evalAsync is not evaluated immediately
        .bracket(use(_).doOnCancel(Task(log.error("Cancelled, a resource may be leak"))))(r => Task.evalAsync(release(r)))
  }
}
