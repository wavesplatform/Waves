package com.wavesplatform.matcher.queue

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait MatcherQueue {
  def startConsume(fromOffset: QueueEventWithMeta.Offset, process: QueueEventWithMeta => Future[Unit]): Unit
  def storeEvent(payload: QueueEvent): Future[QueueEventWithMeta.Offset]
  def close(timeout: FiniteDuration): Unit
}
