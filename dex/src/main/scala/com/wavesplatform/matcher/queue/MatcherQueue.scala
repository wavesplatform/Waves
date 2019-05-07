package com.wavesplatform.matcher.queue

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait MatcherQueue {
  def startConsume(fromOffset: QueueEventWithMeta.Offset, process: QueueEventWithMeta => Unit): Unit

  /**
    * @return Depending on settings and result:
    *         Future.successful(None)    if storing is disabled
    *         Future.successful(Some(x)) if storing is enabled and it was successful
    *         Future.failed(error)       if storing is enabled and it was failed
    */
  def storeEvent(payload: QueueEvent): Future[Option[QueueEventWithMeta]]

  /**
    * @return -1 if topic is empty or even it doesn't exist
    */
  def lastEventOffset: Future[QueueEventWithMeta.Offset]
  def close(timeout: FiniteDuration): Unit
}

object MatcherQueue {
  private val stored: Future[Option[QueueEventWithMeta]] = Future.successful(None)

  private[queue] trait Producer {
    def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]]
    def close(timeout: FiniteDuration): Unit
  }

  private[queue] object IgnoreProducer extends Producer {
    override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = stored
    override def close(timeout: FiniteDuration): Unit                              = {}
  }
}
