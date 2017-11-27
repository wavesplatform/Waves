package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.network.RxScoreObserver.BestChannel
import io.netty.channel.Channel
import io.netty.channel.local.LocalChannel
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.History.BlockchainScore

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class RxScoreObserverSpec extends FreeSpec with Matchers with TransactionGen {

  implicit val scheduler = monix.execution.Scheduler.singleThread("rx-score-observer-test")

  def buildObserver() = {
    val localScores = PublishSubject[BlockchainScore]
    val remoteScores = PublishSubject[(Channel, BlockchainScore)]
    val channelClosed = PublishSubject[Channel]
    val syncWith = RxScoreObserver(1.minute, 0, localScores, remoteScores, channelClosed)

    (syncWith, localScores, remoteScores, channelClosed)
  }


  def test[T](target: Observable[T])(testBody: (() => T) => Future[Any]): Unit = {
    val replay = target.replay(1)
    replay.connect()

    //replay.take(1).lastL.coeval.value.fold(x => x, x => Future(x)).flatMap(x => f(Success(x)))

    Await.result(testBody(() => replay.take(1).lastL.coeval.value.right.get), 10.seconds)
  }

  "should emit better channel" - {
    "when a new channel has the better score than the local one" in {
      val (syncWith, localScores, remoteScores, _) = buildObserver()
      val testChannel = new LocalChannel()

      test(syncWith) { recent =>
        for {
          _ <- localScores.onNext(1)
          recent().isEmpty shouldBe true

          _ <- remoteScores.onNext((testChannel, 2))
          recent() shouldBe BestChannel(testChannel, 2)

        } yield {}
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
