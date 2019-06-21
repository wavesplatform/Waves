package com.wavesplatform.matcher.queue

import java.util.concurrent.Executors
import java.util.{Timer, TimerTask}

import com.wavesplatform.matcher.LocalQueueStore
import com.wavesplatform.matcher.queue.LocalMatcherQueue._
import com.wavesplatform.matcher.queue.MatcherQueue.{IgnoreProducer, Producer}
import com.wavesplatform.utils.{ScorexLogging, Time}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

class LocalMatcherQueue(settings: Settings, store: LocalQueueStore, time: Time)(implicit ec: ExecutionContext)
    extends MatcherQueue
    with ScorexLogging {

  @volatile private var lastUnreadOffset: QueueEventWithMeta.Offset = -1L

  private val timer = new Timer("local-dex-queue", true)
  private val producer: Producer = {
    val r = if (settings.enableStoring) new LocalProducer(store, time) else IgnoreProducer
    log.info(s"Choosing ${r.getClass.getName} producer")
    r
  }

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: Seq[QueueEventWithMeta] => Future[Unit]): Unit = {
    if (settings.cleanBeforeConsume) store.dropUntil(fromOffset)

    def runOnce(from: QueueEventWithMeta.Offset): Future[QueueEventWithMeta.Offset] = {
      val requests = store.getFrom(from, settings.maxElementsPerPoll)
      if (requests.isEmpty) Future.successful(from)
      else {
        val newOffset = requests.last.offset + 1
        log.trace(s"Read ${newOffset - from} events")
        process(requests).map(_ => newOffset)
      }
    }

    val pollingInterval = settings.pollingInterval.toNanos
    def loop(from: QueueEventWithMeta.Offset): Unit = {
      val start = System.nanoTime()
      runOnce(from)
        .recover {
          case NonFatal(e) =>
            // Actually this should not happen. The Future[_] type is not powerful to express error-less computations
            log.error("Can't process messages, trying again", e)
            from
        }
        .map { nextStartOffset =>
          lastUnreadOffset = nextStartOffset
          val diff  = System.nanoTime() - start
          val delay = math.max(pollingInterval - diff, 0L) / 1000000 // to millis
          timer.schedule(new TimerTask {
            override def run(): Unit = loop(lastUnreadOffset)
          }, delay)
        }
    }

    loop(fromOffset)
  }

  override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = producer.storeEvent(event)

  override def lastProcessedOffset: QueueEventWithMeta.Offset = lastUnreadOffset - 1

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
