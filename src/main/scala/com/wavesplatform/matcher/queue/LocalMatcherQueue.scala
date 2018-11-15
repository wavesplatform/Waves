package com.wavesplatform.matcher.queue

import java.util.{Timer, TimerTask}

import com.wavesplatform.matcher.LocalQueueStore
import com.wavesplatform.matcher.queue.LocalMatcherQueue.Settings
import com.wavesplatform.utils.{ScorexLogging, Time}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

class LocalMatcherQueue(settings: Settings, store: LocalQueueStore, time: Time)(implicit ec: ExecutionContext)
    extends MatcherQueue
    with ScorexLogging {

  private var lastUnreadOffset: QueueEventWithMeta.Offset = 0
  private val timer                                       = new Timer("local-dex-queue", true)

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: QueueEventWithMeta => Future[Unit]): Unit = {
    lastUnreadOffset = fromOffset

    timer.schedule(
      new TimerTask {
        override def run(): Unit = {
          val requests = store.getFrom(lastUnreadOffset)
          lastUnreadOffset = requests.lastOption.fold(lastUnreadOffset) { x =>
            val r = x.offset + 1
            log.trace(s"Read $r events")
            r
          }
          requests.foreach(process)
        }
      },
      0,
      settings.pollingInterval.toMillis
    )
  }

  override def storeEvent(event: QueueEvent): Future[QueueEventWithMeta.Offset] = {
    val ts = time.correctedTime()
    log.trace(s"Store $event with timestamp=$ts")
    Future.successful(store.enqueue(event, time.correctedTime()))
  }

  override def close(timeout: FiniteDuration): Unit = timer.cancel()
}

object LocalMatcherQueue {
  case class Settings(pollingInterval: FiniteDuration)
}
