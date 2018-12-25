package com.wavesplatform.matcher.queue

import java.util.concurrent.Executors
import java.util.{Timer, TimerTask}

import com.wavesplatform.matcher.LocalQueueStore
import com.wavesplatform.matcher.queue.LocalMatcherQueue.Settings
import com.wavesplatform.utils.{ScorexLogging, Time}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}

class LocalMatcherQueue(settings: Settings, store: LocalQueueStore, time: Time)(implicit ec: ExecutionContext)
    extends MatcherQueue
    with ScorexLogging {

  private var lastUnreadOffset: QueueEventWithMeta.Offset = 0
  private val timer                                       = new Timer("local-dex-queue", true)
  private val thread                                      = Executors.newSingleThreadExecutor()

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: QueueEventWithMeta => Future[Unit]): Unit = {
    if (settings.cleanBeforeConsume) store.dropUntil(fromOffset)
    lastUnreadOffset = fromOffset

    timer.schedule(
      new TimerTask {
        override def run(): Unit = {
          val requests = store.getFrom(lastUnreadOffset, settings.maxElementsPerPoll)
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
    val p = Promise[QueueEventWithMeta.Offset]
    // Need to guarantee the order
    thread.submit(new Runnable {
      override def run(): Unit = {
        val ts = time.correctedTime()
        log.trace(s"Store $event with timestamp=$ts")
        p.success(store.enqueue(event, time.correctedTime()))
      }
    })
    p.future
  }

  override def close(timeout: FiniteDuration): Unit = timer.cancel()
}

object LocalMatcherQueue {
  case class Settings(pollingInterval: FiniteDuration, maxElementsPerPoll: Int, cleanBeforeConsume: Boolean)
}
