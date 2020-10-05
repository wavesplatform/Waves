package com.wavesplatform.events.repo

import java.nio.{ByteBuffer, ByteOrder}

import cats.implicits._
import com.wavesplatform.Shutdownable
import com.wavesplatform.database.openDB
import com.wavesplatform.events._
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class UpdatesRepoImpl(directory: String)(implicit val scheduler: Scheduler)
    extends UpdatesRepo.Read
    with UpdatesRepo.Write
    with UpdatesRepo.Stream
    with ScorexLogging
    with Shutdownable
    with Lockable {
  import UpdatesRepoImpl._

  private[this] val db                               = openDB(directory)
  private[this] var liquidState: Option[LiquidState] = None

  log.info(s"BlockchainUpdates extension opened db at ${directory}")

  override def shutdown(): Unit = db.close()

  private[this] val realTimeUpdates = ConcurrentSubject.publish[BlockchainUpdated]
  private[this] def sendRealTimeUpdate(ba: BlockchainUpdated): Try[Unit] = {
    realTimeUpdates.onNext(ba) match {
      case Ack.Continue =>
        Success(())
      case Ack.Stop =>
        Failure(new IllegalStateException("realTimeUpdates stream sent Ack.Stop"))
    }
  }

  // UpdatesRepo.Read impl
  override def height: Try[Int] = Try {
    readLock {
      liquidState.map(_.keyBlock.toHeight).getOrElse {
        val iter = db.iterator()
        try {
          iter.seekToLast()
          if (iter.hasNext) {
            val blockBytes = iter.next.getValue
            val lastUpdate = PBBlockchainUpdated.parseFrom(blockBytes).vanilla.get
            lastUpdate.toHeight
          } else {
            0
          }
        } finally {
          iter.close()
        }
      }
    }
  }

  override def updateForHeight(height: Int): Try[Option[BlockAppended]] =
    readLock {
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
          val bytes = db.get(key(height))
          if (bytes == null || bytes.isEmpty) {
            Success(None)
          } else {
            for {
              pbParseResult <- Try(PBBlockchainUpdated.parseFrom(bytes))
              vanillaUpdate <- pbParseResult.vanilla
              blockAppended <- Try(vanillaUpdate.asInstanceOf[BlockAppended])
            } yield Some(blockAppended)
          }
      }
    }

  override def updatesRange(from: Int, to: Int): Try[Seq[BlockAppended]] =
    if (to - from > RANGE_REQUEST_MAX_SIZE) {
      Failure(new IllegalArgumentException(s"Maximum range length of $RANGE_REQUEST_MAX_SIZE exceeded"))
    } else {
      readLock {
        height.flatMap { h =>
          log.info(s"BlockchainUpdates request updatesRange from $from to $to")
          val cnt = to - from + 1
          Try(
            Await
              .result(
                stream(from).collect { case u: BlockAppended => u }.take(cnt).takeWhile(_.toHeight < h).bufferTumbling(cnt).runAsyncGetLast,
                RANGE_REQUEST_TIMEOUT
              )
              .get
          ).flatMap { appends =>
            if (appends.nonEmpty && appends.length < (appends.last.toHeight - appends.head.toHeight + 1)) {
              Failure(new IllegalStateException(s"Missing blocks found in range $from, $to"))
            } else {
              Success(appends)
            }
          }
        }
      }
    }

  // UpdatesRepo.Write impl
  override def appendBlock(blockAppended: BlockAppended): Try[Unit] = writeLock {
    Try {
      liquidState.foreach { ls =>
        val solidBlock = ls.solidify()
        db.put(
          key(solidBlock.toHeight),
          solidBlock.protobuf.toByteArray
        )
      }
      liquidState = Some(LiquidState(blockAppended, Seq.empty))
      sendRealTimeUpdate(blockAppended)
    }
  }

  override def appendMicroBlock(microBlockAppended: MicroBlockAppended): Try[Unit] = writeLock {
    liquidState match {
      case Some(LiquidState(keyBlock, microBlocks)) =>
        liquidState = Some(LiquidState(keyBlock, microBlocks :+ microBlockAppended))
        sendRealTimeUpdate(microBlockAppended)
      case None =>
        Failure(new IllegalStateException("BlockchainUpdates attempted to insert a microblock without a keyblock"))
    }
  }

  override def rollback(rollback: RollbackCompleted): Try[Unit] =
    writeLock {
      height
        .flatMap { h =>
          if (rollback.toHeight > h) {
            Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to a height higher than current"))
          } else if (rollback.toHeight <= 0) {
            Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to a non-positive height"))
          } else if (rollback.toHeight == h) {
            Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to current height"))
          } else if (rollback.toHeight == h - 1) {
            liquidState = None
            Success(())
          } else {
            val iter  = db.iterator()
            val batch = db.createWriteBatch()
            try {
              iter.seek(key(rollback.toHeight))
              iter.next
              while (iter.hasNext) {
                batch.delete(iter.next.getKey)
              }
              db.write(batch)
              Success(())
            } catch {
              case t: Throwable => Failure(t)
            } finally {
              iter.close()
              batch.close()
            }
          }
        }
        .flatMap(_ => sendRealTimeUpdate(rollback))
    }

  override def rollbackMicroBlock(microBlockRollback: MicroBlockRollbackCompleted): Try[Unit] =
    writeLock {
      height
        .flatMap { h =>
          if (microBlockRollback.toHeight != h) {
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
        .flatMap(_ => sendRealTimeUpdate(microBlockRollback))
    }

  // UpdatesRepo.Stream impl
  override def stream(fromHeight: Int): Observable[BlockchainUpdated] = {
    val realTimeUpdatesForCurrentSubscription = ConcurrentSubject.replay[BlockchainUpdated]

    /**
      * reads from level db by synchronous batches each using one iterator
      * each batch gets a read lock
      * @param startingFrom batch start height
      * @return Task to be consumed by Observable.unfoldEval
      */
    def readBatch(startingFrom: Option[Int]): Task[Option[(Seq[BlockchainUpdated], Option[Int])]] = Task.eval {
      startingFrom map { from =>
        readLock {
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
            realTimeUpdates.subscribe(realTimeUpdatesForCurrentSubscription)
            val liquidUpdates = liquidState match {
              case None                                     => Seq.empty
              case Some(LiquidState(keyBlock, microBlocks)) => Seq(keyBlock) ++ microBlocks
            }
            (res.result() ++ liquidUpdates, None)
          } else {
            val result       = res.result()
            val nextTickFrom = result.lastOption.map(_.toHeight + 1)
            (result, nextTickFrom)
          }
        }
      }
    }

    Observable.fromTry(height).flatMap { h =>
      if (h < fromHeight) {
        Observable.raiseError(new IllegalArgumentException("Requested start height exceeds current blockchain height"))
      } else {
        val historical = Observable
          .unfoldEval(fromHeight.some)(readBatch)
          .flatMap(Observable.fromIterable)

        historical ++ realTimeUpdatesForCurrentSubscription
      }
    }
  }
}

object UpdatesRepoImpl {
  private val LEVELDB_READ_BATCH_SIZE = 1024

  private val RANGE_REQUEST_MAX_SIZE = 1000
  private val RANGE_REQUEST_TIMEOUT  = 1.minute

  private def key(height: Int): Array[Byte] =
    ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putInt(height).array()
}
