package com.wavesplatform.matcher.queue

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait MatcherQueue {
  def startConsume(fromOffset: QueueEventWithMeta.Offset, process: QueueEventWithMeta => Unit): Unit
  def storeEvent(payload: QueueEvent): Future[QueueEventWithMeta]
  def close(timeout: FiniteDuration): Unit
}
