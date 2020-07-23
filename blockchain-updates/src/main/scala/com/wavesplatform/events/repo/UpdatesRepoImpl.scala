package com.wavesplatform.events.repo

import java.nio.ByteBuffer

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.{BlockAppended, BlockchainUpdated, MicroBlockAppended}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.{Ack, Cancelable, Scheduler}
import monix.reactive.OverflowStrategy.BackPressure
import monix.reactive.observers.Subscriber
import monix.reactive.{Observable, OverflowStrategy}
import monix.reactive.subjects.ConcurrentSubject
import org.iq80.leveldb.DB

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

class UpdatesRepoImpl(db: DB, currentUpdates: Observable[BlockchainUpdated])(implicit val scheduler: Scheduler)
    extends UpdatesRepo
    with ScorexLogging {
  import UpdatesRepoImpl._

  // no need to persist liquid state. Keeping it mutable in a class
  private[this] var liquidState: Option[LiquidState] = None

  override def height: Int = liquidState.map(_.keyBlock.toHeight).getOrElse(0)

  override def getLiquidState(): Option[LiquidState] = liquidState

  override def appendMicroBlock(microBlockAppended: MicroBlockAppended): Try[Unit] =
    liquidState match {
      case Some(LiquidState(keyBlock, microBlocks)) =>
        liquidState = Some(LiquidState(keyBlock, microBlocks :+ microBlockAppended))
        Success()
      case None =>
        Failure(new IllegalStateException("Attempting to insert a microblock without a keyblock"))
    }

  override def dropLiquidState(afterId: Option[ByteStr]): Unit =
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

  override def removeAfter(height: Int): Try[Unit] = ???

  override def appendBlock(blockAppended: BlockAppended): Try[Unit] = Try {
    liquidState.foreach { ls =>
      val solidBlock = ls.solidify()
      val key        = blockKey(solidBlock.toHeight)
      db.put(key, solidBlock.protobuf.toByteArray)
    }
    liquidState = Some(LiquidState(blockAppended, Seq.empty))
  }

  override def getForHeight(height: Int): Try[Option[BlockAppended]] = {
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

  override def getRange(from: Int, to: Int): Try[Seq[BlockAppended]] = {
    // todo error handling, limits and timeouts
    log.info("Requesting stream")
    Try(Await.result(streamRangeOneIter(from, to).bufferTumbling(to - from + 1).runAsyncGetLast, Duration.Inf).get)
  }

  private[repo] def streamRangeOneIter(from: Int, to: Int): Observable[BlockAppended] = {
    Observable.create(OverflowStrategy.Unbounded) { sub =>
      val iterator = db.iterator()
      iterator.seek(blockKey(from))

      var cancelled = false
      val cancel = new Cancelable {
        override def cancel(): Unit = {
          cancelled = true
          sub.onComplete()
        }
      }

      @tailrec
      def go(remaining: Int): Unit = {
        if (remaining > 0 && iterator.hasNext && !cancelled) {
          val next       = iterator.next()
          val blockBytes = next.getValue
          if (blockBytes.nonEmpty) {
            PBBlockchainUpdated.parseFrom(blockBytes).vanilla match {
              case Success(value) =>
                value match {
                  case b: BlockAppended =>
                    sub.onNext(b) match {
                      case Ack.Continue => go(remaining - 1)
                      case Ack.Stop     => sub.onComplete()
                    }
                  case _ => sub.onError(new IllegalStateException(s"Incorrect type of update in the database at height ${fromBlockKey(next.getKey)}"))
                }
              case Failure(exception) => sub.onError(exception)
            }
          } else {
            sub.onError(new IllegalStateException(s"Empty block bytes in the database at height ${fromBlockKey(next.getKey)}"))
          }
        }
      }

      go(to - from + 1)

      iterator.close()
      sub.onComplete()

      cancel
    }
  }

  val BATCH_SIZE                = 10
  val BACK_PRESSURE_BUFFER_SIZE = 1000

  // I think the stream should be back-pressured
  // @todo (maybe) parameterize strategy or at least buffer size
  override def stream(from: Int): Observable[BlockchainUpdated] = {
    Observable.create(OverflowStrategy.Unbounded) { sub =>
      currentUpdates.subscribe(sub)
    }

//    val subject = ConcurrentSubject.publish[BlockchainUpdated]

//    todo async requests in Tasks
//    Task
//      .evalAsync {
//        def sendHistorical(from: Int): Unit = {
//          var batchStart = from
//          while (true) {
//            getRange(batchStart, batchStart + BATCH_SIZE - 1) match {
//              case Success(batch) =>
//                log.info(s"Requested batch from ${batch.head.toHeight} to ${batch.last.toHeight}")
//                for (ba <- batch) {
//                  val ack = subject.onNext(ba)
//                  ack match {
//                    case Ack.Continue => ()
//                    case Ack.Stop     => return
//                  }
//                }
//                if (batch.length < BATCH_SIZE) return
//                batchStart += BATCH_SIZE
//              case Failure(exception) =>
//                log.error("Failed to get range, [$from, ${from + BATCH_SIZE - 1}]")
//                subject.onError(exception)
//                return
//            }
//          }
//        }

//        sendHistorical(from)

    // subscribe to current events and put them into buffer, at the same time
//
//        log.info(s"Historical events sent")
//      }
//      .flatMap { _ =>
//        log.info(s"Subscribing stream to current events")
//        // todo will concurrent subscriptions work?
//        // will they be cancelled correctly?
//        Task.evalAsync(currentUpdates.subscribe(subject))
//      }
//      .runAsyncAndForget
//    @tailrec
//    def producerLoop(sub: Subscriber[BlockchainUpdated], from: Option[Int]): Cancelable /* Task[Unit] */ =
//      def send(upd: BlockchainUpdated): Task[Ack] = ???
//
//      from match {
//        case None => Task.from(currentUpdates.subscribe(sub))
//        case Some(h) => Task.defer {
//          getRange(h, h + BATCH_SIZE - 1) match {
//            case Success(batch) =>
//
//              Task.deferFuture(sub.onNext(n))
//                .flatMap {
//                case Ack.Continue => producerLoop(sub, n + 1)
//                case Ack.Stop => Task.unit
//                }
//
//              batch.foreach()
//              if (value.length < BATCH_SIZE) {
//                // then next time it's current events only
//                producerLoop()
//              }
//
//
//            case Failure(exception) =>
//              log.error("Failed to get range, [$from, ${from + BATCH_SIZE - 1}]")
//              sub.onError(exception)
//              Task.raiseError(exception)
//          }
//        }
//      }
  }
}

object UpdatesRepoImpl {
  private def blockKey(height: Int): Array[Byte] = ByteBuffer.allocate(8).putInt(height).array()

  private def fromBlockKey(key: Array[Byte]): Int = ByteBuffer.wrap(key).getInt
}
