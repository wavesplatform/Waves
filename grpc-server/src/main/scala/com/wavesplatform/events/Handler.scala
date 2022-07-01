package com.wavesplatform.events
import com.wavesplatform.events.repo.LiquidState
import com.wavesplatform.utils.ScorexLogging
import monix.execution.{Ack, Scheduler}
import monix.reactive.subjects.PublishToOneSubject

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

class Handler(id: String, maybeLiquidState: Option[LiquidState], subject: PublishToOneSubject[BlockchainUpdated], maxQueueSize: Int)(
    implicit s: Scheduler
) extends ScorexLogging {

  private[this] val queue = maybeLiquidState.fold(ArrayBuffer.empty[BlockchainUpdated])(ls => ArrayBuffer.from(ls.keyBlock +: ls.microBlocks))

  @volatile
  private[this] var cancelled = false

  subject.subscription.onComplete {
    case Success(Ack.Continue) => sendUpdate()
    case _                     => cancelled = true
  }

  def handleUpdate(ba: BlockchainUpdated): Unit = queue.synchronized {
    if (queue.size >= maxQueueSize) {
      log.debug(s"[$id] Queue is full, cancelling subscription")
      cancelled = true
      queue.clear()
      subject.onComplete()
    } else {
      queue.append(ba)
      sendUpdate()
    }
  }

  private def revertMicroBlock(rollbackEvent: MicroBlockRollbackCompleted): Unit = {
    queue.zipWithIndex.collectFirst {
      case (ba: BlockAppended, idx) if ba.id == rollbackEvent.id =>
        // block is found, remove all microblocks
        queue.takeInPlace(idx + 1)
      case (mba: MicroBlockAppended, idx) if mba.references(rollbackEvent) =>
        // first microblock is found, remove all microblocks with it
        queue.takeInPlace(idx)
    }.getOrElse {
        // some microblocks were sent, send rollback
        queue.append(rollbackEvent)
      }
  }

  def rollbackMicroBlock(rollbackEvent: MicroBlockRollbackCompleted): Unit = {
    queue.synchronized(revertMicroBlock(rollbackEvent))
    sendUpdate()
  }

  private def revertBlock(rollbackEvent: RollbackCompleted): Unit = {
    queue.zipWithIndex.reverseIterator
      .collectFirst {
        case (bu, idx) if bu.id == rollbackEvent.id => idx
      } match {
      case Some(idx) =>
        log.trace(s"[$id] Dropping ${queue.length - (idx + 1)} buffered events after ${rollbackEvent.ref}")
        queue.takeInPlace(idx + 1)
      case None =>
        log.trace(s"[$id] Buffering rollback ${rollbackEvent.ref})")
        queue.append(rollbackEvent)
    }
  }

  def rollbackBlock(revertedMicros: Seq[MicroBlockRollbackCompleted], revertedBlocks: Seq[RollbackCompleted]): Unit = queue.synchronized {
    revertedMicros.foreach(revertMicroBlock)
    revertedBlocks.foreach { rb =>
      queue.lastOption match {
        case Some(_: BlockAppended | _: MicroBlockAppended) => revertBlock(rb)
        case _                                              => queue.append(rb)
      }
    }

    sendUpdate()
  }

  def shutdown(): Unit = queue.synchronized {
    log.trace(s"[$id] Stopping subscription")
    cancelled = true
    queue.clear()
    subject.onComplete()
  }

  protected def sendUpdate(): Unit =
    if (queue.nonEmpty && subject.subscription.isCompleted && !cancelled)
      s.execute(
        () =>
          queue.synchronized {
            if (queue.nonEmpty) {
              val v = queue.remove(0)
              log.trace(s"[$id] Sending ${v.ref} to subscriber")
              subject.onNext(v).onComplete {
                case Success(Ack.Continue) =>
                  log.trace(s"[$id] Sent ${v.ref} to subscriber, attempting to send one more")
                  sendUpdate()
                case Success(Ack.Stop) =>
                  log.debug(s"[$id] Subscriber stopped")
                case Failure(exception) =>
                  log.error(s"[$id] Error sending ${v.ref}", exception)
              }
            }
          }
      )
}
