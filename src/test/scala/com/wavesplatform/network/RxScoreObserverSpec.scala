package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import io.netty.channel.Channel
import monix.reactive.subjects.PublishSubject
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.History.BlockchainScore

import scala.concurrent.duration._

class RxScoreObserverSpec extends FreeSpec with Matchers with TransactionGen {

  def buildObserver() = {
    val localScores = PublishSubject[BlockchainScore]
    val remoteScores = PublishSubject[(Channel, BlockchainScore)]
    val channelClosed = PublishSubject[Channel]
    val syncWith = RxScoreObserver(1.minute, localScores, remoteScores, channelClosed)

    (syncWith, localScores, remoteScores, channelClosed)
  }

  "should emit better channel" - {
    "when a new channel has the better score than the local one" in {
//      val (syncWith, localScores, remoteScores, _) = buildObserver()
//      val ch = new LocalChannel()
//      val sub = syncWith.firstL.runAsync
//      localScores.onNext(1)
//      remoteScores.onNext((ch, 2))
//      Await.result(sub, Duration.Inf) shouldBe Some(BestChannel(ch, 2))
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
