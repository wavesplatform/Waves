package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.network.RxScoreObserver.BestChannel
import io.netty.channel.Channel
import io.netty.channel.local.LocalChannel
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.History.BlockchainScore

import scala.concurrent.duration._

class RxScoreObserverSpec extends FreeSpec with Matchers with TransactionGen {

  implicit val scheduler = monix.execution.Scheduler.singleThread("test-worker")

  def buildObserver() = {
    val localScores = PublishSubject[BlockchainScore]
    val remoteScores = PublishSubject[(Channel, BlockchainScore)]
    val channelClosed = PublishSubject[Channel]
    val syncWith = RxScoreObserver(1.minute, 0, localScores, remoteScores, channelClosed)

    (syncWith, localScores, remoteScores, channelClosed)
  }

  trait Context[T] {
    def waitFor(desiredCount:Int):Seq[T]
    def waitForNext:T = waitFor(1).head
  }

  def test[T](target: Observable[T])(testBody: Context[T] => Any): Unit = {
    val l = new Object()
    @volatile var collected = Seq.empty[T]
    target.foreach(i => {
      collected :+= i
      l.synchronized {
        l.notifyAll()
      }
    })

    val context = new Context[T] {
      def waitFor(desiredCount:Int):Seq[T] = {
        l.synchronized {
          while (collected.length < desiredCount) {
            l.wait(1000*10)
          }
          val result = collected
          collected = Seq.empty
          result
        }
      }
    }

    testBody(context)
  }

  "should emit better channel" - {
    "when a new channel has the better score than the local one" in {
      val (syncWith, localScores, remoteScores, _) = buildObserver()
      val testChannel = new LocalChannel()

      test(syncWith) { context =>
          localScores.onNext(1)
          context.waitForNext shouldBe None
          remoteScores.onNext((testChannel, 2))
          context.waitForNext shouldBe Some(BestChannel(testChannel, 2))
      }
    }

    "when the connection with the best one is dropped" in {
//      val (syncWith, localScores, remoteScores, cc) = buildObserver()
//      val ch100 = new LocalChannel()
//      val ch200 = new LocalChannel()
//      val sub = syncWith.firstL.runAsync
//      localScores.onNext(1)
//      remoteScores.onNext((ch200, 200))
//      remoteScores.onNext((ch100, 100))
//      cc.onNext(ch200)
//      Await.result(sub, Duration.Inf) shouldBe Some(BestChannel(ch100, 100))
    }
  }

  "should not request a new extension if a previous one is not downloaded yet" ignore {
    "when the score of the best channel was changed" in {

    }
    "when new connection" - {

    }
  }

  "should re-request extensions" ignore {
    "when the local score is changed but still worse than the better one" in {

    }
  }

  "should propagate blocks from an expected extensions" ignore {}

  "should ignore blocks from an unexpected extensions" ignore {}
}
