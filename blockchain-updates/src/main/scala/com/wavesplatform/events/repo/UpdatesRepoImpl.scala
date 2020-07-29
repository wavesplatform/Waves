package com.wavesplatform.events.repo

import java.nio.ByteBuffer
import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}

import cats.implicits._
import com.wavesplatform.Shutdownable
import com.wavesplatform.database.openDB
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.events._
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

class UpdatesRepoImpl(directory: String)(implicit val scheduler: Scheduler)
    extends UpdatesRepo.Read
    with UpdatesRepo.Write
    with UpdatesRepo.Stream
    with ScorexLogging
    with Shutdownable {
  import UpdatesRepoImpl._

  // State with locks
  private[this] val db                               = openDB(directory)
  private[this] var liquidState: Option[LiquidState] = None
  private val lock: ReadWriteLock                    = new ReentrantReadWriteLock()

  log.info(s"BlockchainUpdates extension opened db at ${directory}")

  override def shutdown(): Unit = db.close()

  private[this] def withReadLock[A](a: => A): A = {
    lock.readLock().lock()
    try {
      a
    } finally {
      lock.readLock().unlock()
    }
  }

  private[this] def withWriteLock[A](a: => A): A = {
    lock.writeLock().lock()
    try {
      a
    } finally {
      lock.writeLock().unlock()
    }
  }

  // Recent updates stream
  /*
  A limited buffered replayable stream of recent updates for synchronization.
  A streaming subscriber will real leveldb + liquid state fully, then find exact place
  in recent updates to start from, and continue from there.

  Buffer size reasoning:
  Let's assume max rollback to be a 100 blocks (current mainnet assumption).
  On average, there are 12 microblocks per block. Add one microfork, get 13 events per block.
  100 * 13 = 1300. 2048 should to be enough.
   */
  val RECENT_UPDATES_BUFFER_SIZE  = 2048
  private[this] val recentUpdates = ConcurrentSubject.replayLimited[BlockchainUpdated](RECENT_UPDATES_BUFFER_SIZE)
  private[this] def sendToRecentUpdates(ba: BlockchainUpdated): Try[Unit] = {
    recentUpdates.onNext(ba) match {
      case Ack.Continue => Success(())
      case Ack.Stop     => Failure(new IllegalStateException("recentUpdates stream sent Ack.Stop"))
    }
  }

  // UpdatesRepo.Read impl
  override def height: Int = withReadLock {
    liquidState.map(_.keyBlock.toHeight).getOrElse(0)
  }

  override def updateForHeight(height: Int): Try[Option[BlockAppended]] = withReadLock {
    liquidState match {
      case Some(ls) if ls.keyBlock.toHeight == height =>
        log.debug(s"BlockchainUpdates extension requested liquid block at height $height")
        Success(Some(ls.solidify()))
      case Some(ls) if ls.keyBlock.toHeight < height =>
        log.warn(s"BlockchainUpdates extension requested non-existing block at height $height, current ${ls.keyBlock.toHeight}")
        Success(None)
      case _ if height <= 0 =>
        Failure(new IllegalArgumentException("BlockchainUpdates asked for an update at a non-positive height"))
      case _ =>
        val bytes = db.get(key(height + 100000000))
        if (bytes == null || bytes.isEmpty) {
          Success(None)
        } else {
          Try {
            log.debug(s"BlockchainUpdates extension parsing bytes from leveldb for height $height")
            PBBlockchainUpdated
              .parseFrom(bytes)
              .vanilla
              .toOption
              .collect { case ba: BlockAppended => ba }
          }
        }
    }
  }

  override def updatesRange(from: Int, to: Int): Try[Seq[BlockAppended]] = withReadLock {
    // todo error handling, limits and timeouts
    log.info(s"BlockchainUpdates request updatesRange from $from to $to")
    val cnt = to - from + 1
    Try(Await.result(stream(from).collect { case u: BlockAppended => u }.take(cnt).bufferTumbling(cnt).runAsyncGetLast, Duration.Inf).get)
      .flatMap { appends =>
        if (appends.nonEmpty && appends.length < (appends.last.toHeight - appends.head.toHeight + 1)) {
          Failure(new IllegalStateException(s"Missing blocks found in range $from, $to"))
        } else {
          Success(appends)
        }
      }
  }

  // UpdatesRepo.Write impl
  override def appendBlock(blockAppended: BlockAppended): Try[Unit] = withWriteLock {
    Try {
      liquidState.foreach { ls =>
        val solidBlock = ls.solidify()
        db.put(
          key(solidBlock.toHeight),
          solidBlock.protobuf.toByteArray
        )
      }
      liquidState = Some(LiquidState(blockAppended, Seq.empty))
      sendToRecentUpdates(blockAppended)
    }
  }

  override def appendMicroBlock(microBlockAppended: MicroBlockAppended): Try[Unit] = withWriteLock {
    liquidState match {
      case Some(LiquidState(keyBlock, microBlocks)) =>
        liquidState = Some(LiquidState(keyBlock, microBlocks :+ microBlockAppended))
        sendToRecentUpdates(microBlockAppended)
      case None =>
        Failure(new IllegalStateException("BlockchainUpdates attempted to insert a microblock without a keyblock"))
    }
  }

  override def rollback(rollback: RollbackCompleted): Try[Unit] =
    if (rollback.toHeight > height) {
      Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to a height higher than current"))
    } else if (rollback.toHeight <= 0) {
      Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to a non-positive height"))
    } else if (rollback.toHeight == height) {
      Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to current height"))
    } else if (rollback.toHeight == height - 1) {
      liquidState = None
      Success(())
    } else
      withWriteLock {
        val iter  = db.iterator()
        val batch = db.createWriteBatch()
        try {
          iter.seek(key(rollback.toHeight))
          iter.next
          while (iter.hasNext) { batch.delete(iter.next.getKey) }
          db.write(batch)
          Success(())
        } catch {
          case t: Throwable => Failure(t)
        } finally {
          iter.close()
          batch.close()
        }
      }

  override def rollbackMicroBlock(microBlockRollback: MicroBlockRollbackCompleted): Try[Unit] = withWriteLock {
    if (microBlockRollback.toHeight != height) {
      Failure(new IllegalArgumentException("BlockchainUpdates microblock rollback height was not equal to current height"))
    } else {
      liquidState match {
        case Some(ls) =>
          if (microBlockRollback.toId == ls.keyBlock.toId) {
            liquidState = Some(ls.copy(microBlocks = Seq.empty))
            Success(())
          } else {
            val remainingMicroBlocks = ls.microBlocks.reverse.dropWhile(_.toId != microBlockRollback.toId).reverse
            if (remainingMicroBlocks.isEmpty) {
              Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback a non-existing microblock"))
            } else {
              liquidState = Some(ls.copy(microBlocks = remainingMicroBlocks))
              Success(())
            }
          }
        case None => Failure(new IllegalStateException("BlockchainUpdates attempted to rollback microblock without liquid state present"))
      }
    }
  }

  // UpdatesRepo.Stream impl
  val LEVELDB_READ_BATCH_SIZE = 1024

  // todo detect out-or-order events and throw an exception, or at least log an error
  override def stream(fromHeight: Int): Observable[BlockchainUpdated] = {

    /**
      * reads from level db by synchronous batches each using one iterator
      * each batch gets a read lock
      * last batch also includes liquid state appended at the end
      * @param startingFrom batch start height
      * @return Task to be consumed by Observable.unfoldEval
      */
    def readBatch(startingFrom: Option[Int]): Task[Option[(Seq[BlockchainUpdated], Option[Int])]] = Task.eval {
      startingFrom map { from =>
        withReadLock {
          val res = Seq.newBuilder[BlockchainUpdated]
          res.sizeHint(LEVELDB_READ_BATCH_SIZE)

          val iterator = db.iterator()
          val isLastBatch = try {
            iterator.seek(key(from))

            @tailrec
            def goUnsafe(remaining: Int): Boolean = {
              if (remaining > 0 && iterator.hasNext) {
                val next       = iterator.next()
                val blockBytes = next.getValue
                res += PBBlockchainUpdated.parseFrom(blockBytes).vanilla.get
                goUnsafe(remaining - 1)
              } else {
                !iterator.hasNext
              }
            }

            goUnsafe(LEVELDB_READ_BATCH_SIZE)
          } catch {
            case t: Throwable =>
              Task.raiseError(t)
              true
          } finally {
            iterator.close()
          }

          if (isLastBatch) {
            // send all liquid state to the stream
            liquidState.foreach(res ++= _.toSeq)
            (res.result(), None)
          } else {
            val result       = res.result()
            val nextTickFrom = result.lastOption.map(_.toHeight + 1)
            (result, nextTickFrom)
          }
        }
      }
    }

    val historical = Observable
      .unfoldEval(fromHeight.some)(readBatch)
      .flatMap(Observable.fromIterable)
      .map((Historical, _))

    val recent = recentUpdates.map((Recent, _))

    /*
    Recent events is a buffered stream, so they overlap with historical.
    Some of the recent events need to be dropped.
    The construct below is used to get lastHistorical event
    and, based on it, to decide when to append recent events.
     */
    (historical ++ recent)
      .mapAccumulate(none[BlockchainUpdated]) {
        case (_, (Historical, ba)) => (Some(ba), Some(ba))
        case (Some(lastHistorical), (Recent, recentUpdate)) =>
          if (recentUpdate.toHeight > lastHistorical.toHeight) {
            (Some(lastHistorical), Some(recentUpdate))
          } else (Some(lastHistorical), None)
        case _ => ???
      }
      .collect { case Some(value) => value }
  }
}

private[repo] sealed trait UpdateType
private[repo] case object Historical extends UpdateType
private[repo] case object Recent     extends UpdateType

object UpdatesRepoImpl {
  private def key(height: Int): Array[Byte] = ByteBuffer.allocate(8).putInt(height).array()
}
