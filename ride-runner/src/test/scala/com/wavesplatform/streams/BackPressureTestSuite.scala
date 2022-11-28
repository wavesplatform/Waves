package com.wavesplatform.streams

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.utils.ScorexLogging
import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.execution.{Ack, Cancelable, ChannelType, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}
import monix.reactive.subjects.{AsyncSubject, ConcurrentSubject, Subject}
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.observers.{BufferedSubscriber, Subscriber}
import org.reactivestreams
import org.reactivestreams.Publisher

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Random

class BackPressureTestSuite extends BaseTestSuite with ScorexLogging {
  "BackPressure" in {
    //    val upstream  = new Upstream {
    //      override def requestNext(): Unit = ???
    //    }

    val s = ConcurrentSubject.publish[Int]
    val items = new Upstream {
      val curr = new AtomicInteger(1)

      override def requestNext(): Unit = {
        if (curr.get() >= 10) s.onComplete()
        else s.onNext(curr.getAndIncrement())
      }
    }

    val source = s
      .doOnNextAck { (_, _) =>
        Task {
          log.info("doOnNextAck")
          items.requestNext()
        }
      }

    val r = source
      .asyncBoundary(OverflowStrategy.BackPressure(4))
      //      .bufferTumbling(2)
      //      .bufferIntrospective(2)
      .doOnError(e => Task(log.error("Failed", e)))
      .mapEval { x =>
        val w =
          if (x <= 2) Task(log.info(s"wait: $x")).flatMap(_ => Task.sleep(30.millis))
          else Task.unit

        w.map(_ => log.info(s"b: $x"))
      }
      .runAsyncGetLast

    items.requestNext()

    //    def loop(curr: Int, last: Int): Future[Unit] = {
    //      if (curr == last) Future(source.onComplete())
    //      else
    //        for {
    //          _ <- source.onNext(curr)
    //          _ <- loop(curr + 1, last)
    //        } yield ()
    //    }
    //
    //    loop(1, 9)
    Await.ready(r, Duration.Inf)
  }

  trait Upstream {
    def requestNext(): Unit
  }
}
