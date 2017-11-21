package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.network.RxScoreObserver.BestChannel
import io.netty.channel.Channel
import io.netty.channel.local.LocalChannel
import monix.reactive.subjects.PublishSubject
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.History.BlockchainScore

import scala.concurrent.Await
import scala.concurrent.duration._

class RxScoreObserverSpec extends FreeSpec with Matchers with TransactionGen {

  import monix.execution.Scheduler.Implicits.global

  def buildObserver() = {
    val localScores = PublishSubject[BlockchainScore]
    val remoteScores = PublishSubject[(Channel, BlockchainScore)]
    val channelClosed = PublishSubject[Channel]
    val syncWith = RxScoreObserver(1.minute, localScores, remoteScores, channelClosed)

    (syncWith, localScores, remoteScores, channelClosed)
  }

  "should request an extension" - {
    "when a new channel has the better score than the local one in" - {
      val (syncWith, localScores, remoteScores, _) = buildObserver()
      val ch = new LocalChannel()
      val sub = syncWith.firstL.runAsync
      localScores.onNext(1)
      remoteScores.onNext((ch, 2))
      Await.result(sub, Duration.Inf) shouldBe Some(BestChannel(ch, 2))

    }
  }
}
