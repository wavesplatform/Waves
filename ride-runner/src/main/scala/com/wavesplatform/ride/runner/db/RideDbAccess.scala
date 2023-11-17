package com.wavesplatform.ride.runner.db

import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import org.rocksdb.*
import shapeless.<:!<

import scala.util.Using

trait RideDbAccess {
  def batchedReadOnly[T](f: ReadOnly => T)(implicit ev: T <:!< Task[?]): T
  def batchedReadWrite[T](f: ReadWrite => T)(implicit ev: T <:!< Task[?]): T
  def directReadOnly[T](f: ReadOnly => T): T
  def directReadWrite[T](f: ReadWrite => T): T
}

object RideDbAccess {
  def fromRocksDb(db: RocksDB): RideDbAccess = new RideDbAccess with ScorexLogging {
    override def batchedReadOnly[T](f: ReadOnly => T)(implicit ev: T <:!< Task[?]): T = withReadOptions { ro =>
      f(new BatchedReadOnly(db, ro))
    }

    override def batchedReadWrite[T](f: ReadWrite => T)(implicit ev: T <:!< Task[?]): T = withReadOptions { ro =>
      Using.resource(mkWriteOptions()) { wo =>
        Using.resource(SynchronizedWriteBatch()) { wb =>
          val r = f(new BatchedReadWrite(db, ro, wb))
          db.write(wo, wb.underlying)
          r
        }
      }
    }

    private val direct                                    = new DirectReadWrite(db)
    override def directReadOnly[T](f: ReadOnly => T): T   = f(direct)
    override def directReadWrite[T](f: ReadWrite => T): T = f(direct)

    private def withReadOptions[T](use: ReadOptions => T): T =
      // Snapshot.close does nothing, see https://github.com/facebook/rocksdb/blob/601320164b41643e39851245ee90a90caa61d311/java/src/main/java/org/rocksdb/Snapshot.java#L31
      Using.resource(db.getSnapshot) { s =>
        Using.resource(mkReadOptions(s))(use)
      }((resource: Snapshot) => db.releaseSnapshot(resource))

    private def mkReadOptions(s: Snapshot): ReadOptions = new ReadOptions().setSnapshot(s).setVerifyChecksums(false)
    private def mkWriteOptions(): WriteOptions          = new WriteOptions().setSync(false).setDisableWAL(false)
  }
}
