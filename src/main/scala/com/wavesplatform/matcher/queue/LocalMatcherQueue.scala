package com.wavesplatform.matcher.queue

import java.util.concurrent.Executors
import java.util.{Timer, TimerTask}

import com.wavesplatform.matcher.LocalQueueStore
import com.wavesplatform.matcher.queue.LocalMatcherQueue._
import com.wavesplatform.matcher.queue.MatcherQueue.{IgnoreProducer, Producer}
import com.wavesplatform.utils.{ScorexLogging, Time}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}

class LocalMatcherQueue(settings: Settings, store: LocalQueueStore, time: Time)(implicit ec: ExecutionContext)
    extends MatcherQueue
    with ScorexLogging {

  private var lastUnreadOffset: QueueEventWithMeta.Offset = 0
  private val timer                                       = new Timer("local-dex-queue", true)
  private val producer: Producer = {
    val r = if (settings.enableStoring) new LocalProducer(store, time) else IgnoreProducer
    log.info(s"Choosing ${r.getClass.getName} producer")
    r
  }

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: QueueEventWithMeta => Unit): Unit = {
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

  override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = producer.storeEvent(event)

  override def lastEventOffset: Future[QueueEventWithMeta.Offset] = Future.successful(store.newestOffset.getOrElse(-1L))

  override def close(timeout: FiniteDuration): Unit = {
    timer.cancel()
    producer.close(timeout)
  }
}

object LocalMatcherQueue {
  case class Settings(enableStoring: Boolean, pollingInterval: FiniteDuration, maxElementsPerPoll: Int, cleanBeforeConsume: Boolean)

  private class LocalProducer(store: LocalQueueStore, time: Time)(implicit ec: ExecutionContext) extends Producer {
    private val thread = Executors.newSingleThreadExecutor()

    override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = {
      val p = Promise[QueueEventWithMeta]
      // Need to guarantee the order
      thread.submit(new Runnable {
        override def run(): Unit = {
          val ts     = time.correctedTime()
          val offset = store.enqueue(event, time.correctedTime())
          p.success(QueueEventWithMeta(offset, ts, event))
        }
      })
      p.future.map(Some(_))
    }

    override def close(timeout: FiniteDuration): Unit = thread.shutdown()
  }
}
