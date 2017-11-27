package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.network.RxScoreObserver.BestChannel
import io.netty.channel.Channel
import io.netty.channel.local.LocalChannel
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalatest.{DoNotDiscover, FreeSpec, Matchers}
import scorex.transaction.History.BlockchainScore

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

@DoNotDiscover
class RxScoreObserverSpec2 extends FreeSpec with Matchers with TransactionGen {

  implicit val scheduler = monix.execution.Scheduler.singleThread("test-worker")

  def buildObserver() = {
    val localScores = PublishSubject[BlockchainScore]
    val remoteScores = PublishSubject[(Channel, BlockchainScore)]
    val channelClosed = PublishSubject[Channel]
    val syncWith = RxScoreObserver(1.minute, 0, localScores, remoteScores, channelClosed)

    (syncWith, localScores, remoteScores, channelClosed)
  }

  trait Context[T] {

    def collect(waitTime: Duration = 1000.millis): Seq[T]

    def waitFor(desiredCount: Int): Seq[T]

    def waitForNext: T = waitFor(1).head
  }

  def test[T](target: Observable[T])(testBody: Context[T] => Future[Any]): Unit = {
    val l = new Object()
    @volatile var collected = Seq.empty[T]
    target.foreach(i => {
      collected :+= i
      l.synchronized {
        l.notifyAll()
      }
    })

    val context = new Context[T] {
      def waitFor(desiredCount: Int): Seq[T] = {
        l.synchronized {
          while (collected.length < desiredCount) {
            l.wait(1000 * 10)
          }
          val result = collected
          collected = Seq.empty
          result
        }
      }

      override def collect(waitTime: Duration): Seq[T] = {
        Thread.sleep(waitTime.toMillis)
        l.synchronized {
          val result = collected
          collected = Seq.empty
          result
        }
      }
    }

    Await.result(testBody(context), 10.seconds)
  }

  "should emit better channel" - {
    "when a new channel has the better score than the local one" in {
      val (syncWith, localScores, remoteScores, _) = buildObserver()
      val testChannel = new LocalChannel()

      test(syncWith) { context =>
        for {
          _ <- localScores.onNext(1)
          _ <- remoteScores.onNext((testChannel, 2))
          _ = context.collect() shouldBe List(None, Some(BestChannel(testChannel, 2)))
        } yield ()

      }
    }
  }
}