package com.wavesplatform.matcher.history

import akka.actor.{Actor, Cancellable}
import com.wavesplatform.matcher.history.HistoryRouter.{HistoryMsg, StopAccumulate}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.reflect.ClassTag

abstract class HistoryMessagesBatchSender[M <: HistoryMsg: ClassTag] extends Actor {

  val batchLinger: Long
  val batchEntries: Long

  def createAndSendBatch(batchBuffer: Iterable[M]): Unit

  private val batchBuffer: mutable.Set[M] = mutable.Set.empty[M]

  private def startAccumulating: Cancellable = context.system.scheduler.scheduleOnce(batchLinger.millis, self, StopAccumulate)

  private def sendBatch(): Unit = {
    if (batchBuffer.nonEmpty) {
      createAndSendBatch(batchBuffer)
      batchBuffer.clear()
    }
  }

  def receive: Receive = awaitingHistoryMessages

  private def awaitingHistoryMessages: Receive = {
    case msg: M =>
      startAccumulating
      context.become(accumulateBuffer)
      batchBuffer += msg
  }

  private def sendBatchAndAwait(): Unit = { sendBatch(); context.become(awaitingHistoryMessages) }

  private def accumulateBuffer: Receive = {
    case msg: M         => if (batchBuffer.size == batchEntries) sendBatch(); batchBuffer += msg
    case StopAccumulate => sendBatchAndAwait()
  }
}
