package com.wavesplatform.events.repo

import java.nio.ByteBuffer
import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}

import cats.implicits._
import com.wavesplatform.Shutdownable
import com.wavesplatform.common.state.ByteStr
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
  // STATE
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

  // RECENT UPDATES STREAM
  // A buffered replayable stream of recent updates for synchronization.
  // A streaming subscriber will real leveldb + liquid state fully, then find exact place
  // in recent events to start from, and continue from there
  // todo buffer size establish
  // todo handle long rollback (big buffer? find tollback event?)
  val RECENT_UPDATES_BUFFER_SIZE  = 1024
  private[this] val recentUpdates = ConcurrentSubject.replayLimited[BlockchainUpdated](RECENT_UPDATES_BUFFER_SIZE)
  private[this] def sendToRecentUpdates(ba: BlockchainUpdated): Try[Unit] = {
    recentUpdates.onNext(ba) match {
      case Ack.Continue => Success(())
      case Ack.Stop     => Failure(new IllegalStateException("recentUpdates stream sent Ack.Stop"))
    }
  }

  // Read
  override def height: Int = withReadLock {
    liquidState.map(_.keyBlock.toHeight).getOrElse(0)
  }

  override def updateForHeight(height: Int): Try[Option[BlockAppended]] = withReadLock {
    liquidState match {
      case Some(ls) if ls.keyBlock.toHeight == height =>
        log.debug(s"BlockchainUpdates extension requested liquid block at height $height")
        Success(Some(ls.solidify()))
      case Some(ls) if ls.keyBlock.toHeight < height =>
        log.debug(s"BlockchainUpdates extension requested non-existing block at height $height, current ${ls.keyBlock.toHeight}")
        Success(None)
      case _ =>
        val bytes = db.get(blockKey(height))
        if (bytes.isEmpty) {
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
    log.info("Requesting updatesRange")
    val cnt = to - from + 1
    Try(Await.result(stream(from).collect { case u: BlockAppended => u }.take(cnt).bufferTumbling(cnt).runAsyncGetLast, Duration.Inf).get)
  }

  // Write
  override def appendBlock(blockAppended: BlockAppended): Try[Unit] = withWriteLock {
    Try {
      liquidState.foreach { ls =>
        val solidBlock = ls.solidify()
        val key        = blockKey(solidBlock.toHeight)
        db.put(key, solidBlock.protobuf.toByteArray)
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
        Failure(new IllegalStateException("Attempting to insert a microblock without a keyblock"))
    }
  }

  override def rollback(rollback: RollbackCompleted): Try[Unit] = ???

  override def rollbackMicroBlock(microBlockRollback: MicroBlockRollbackCompleted): Try[Unit] = ???

  // todo remove
  private[this] def dropLiquidState(afterId: Option[ByteStr]): Unit =
    (afterId, liquidState) match {
      case (Some(id), Some(LiquidState(keyBlock, microBlocks))) =>
        if (keyBlock.toId == id) {
          // rollback to key block
          liquidState = Some(LiquidState(keyBlock, Seq.empty))
        } else {
          // otherwise, rollback to a microblock
          val index = microBlocks.indexWhere(_.toId == id)
          if (index != -1) {
            liquidState = Some(LiquidState(keyBlock, microBlocks.dropRight(microBlocks.length - index - 1)))
          }
        }
      case (None, _) => liquidState = None
      case _         => ()
    }

  // Stream
  val BATCH_SIZE                = 10
  val BACK_PRESSURE_BUFFER_SIZE = 1000

  // todo maybe the stream should be back-pressured
  // todo detect out-or-order events and throw exception
  override def stream(from: Int): Observable[BlockchainUpdated] = {
    // it guarantees to send solid and liquid states concatenated as they are at the moment of last tick
    def syncTick(maybeFrom: Option[Int]): Task[Option[(Seq[BlockchainUpdated], Option[Int])]] = Task.eval {
      maybeFrom match {
        case None => None
        case Some(from) =>
          withReadLock {
            val res = Seq.newBuilder[BlockchainUpdated]
            res.sizeHint(BATCH_SIZE)

            val iterator = db.iterator()
            val isLastBatch = try {
              iterator.seek(blockKey(from))

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

              goUnsafe(BATCH_SIZE)
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
              Some((res.result(), None))
            } else {
              val result = res.result()
              Some((result, result.lastOption.map(_.toHeight + 1)))
            }
          }
      }
    }

    val history = Observable
      .unfoldEval(from.some)(syncTick)
      .flatMap(Observable.fromIterable) // this gets us all historical updates
      .map((Historical, _))

    val recent = recentUpdates.map((Recent, _))

    (history ++ recent)
    // the construct below is used to get lastHistorical event
    // and based on it decide when to start streaming recent events
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
  private def blockKey(height: Int): Array[Byte] = ByteBuffer.allocate(8).putInt(height).array()
}
