package com.wavesplatform.network

import com.wavesplatform.network.RxScoreObserver.BestChannel
import com.wavesplatform.{RxScheduler, TransactionGen}
import io.netty.channel.Channel
import io.netty.channel.local.LocalChannel
import monix.reactive.subjects.PublishSubject
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.History.BlockchainScore

import scala.concurrent.duration._

class RxScoreObserverSpec extends FreeSpec with Matchers with TransactionGen with RxScheduler{

  def buildObserver() = {
    val localScores = PublishSubject[BlockchainScore]
    val remoteScores = PublishSubject[(Channel, BlockchainScore)]
    val channelClosed = PublishSubject[Channel]
    val syncWith = RxScoreObserver(1.minute, 0, localScores, remoteScores, channelClosed)

    (newItems(syncWith.map(_.syncWith)), localScores, remoteScores, channelClosed)
  }

  "should emit better channel" - {
    "when a new channel has the better score than the local one" in {
      val (newSyncWith, localScores, remoteScores, _) = buildObserver()
      val testChannel = new LocalChannel()

      test(
        for {
          _ <- send(localScores)(1)
          _ = newSyncWith() shouldBe List(None)
          _ <- send(remoteScores)((testChannel, 2))
          _ = newSyncWith() shouldBe List(Some(BestChannel(testChannel, 2)))
        } yield ())
    }

    "when the connection with the best one is closed" in {
      val (newSyncWith, localScores, remoteScores, closed) = buildObserver()
      val ch100 = new LocalChannel()
      val ch200 = new LocalChannel()

      test(
        for {
          _ <- send(localScores)(1)
          _ <- send(remoteScores)((ch200, 200))
          _ = newSyncWith().last shouldBe Some(BestChannel(ch200, 200))
          _ <- send(remoteScores)((ch100, 100))
          _ <- send(closed)(ch200)
          _ = newSyncWith().last shouldBe Some(BestChannel(ch100, 100))
        } yield ())
    }

    "when the best channel upgrades score" in {
      val (newSyncWith, localScores, remoteScores, _) = buildObserver()
      val testChannel = new LocalChannel()

      test(
        for {
          _ <- send(localScores)(1)
          _ = newSyncWith() shouldBe List(None)
          _ <- send(remoteScores)((testChannel, 2))
          _ = newSyncWith() shouldBe List(Some(BestChannel(testChannel, 2)))
          _ <- send(remoteScores)((testChannel, 3))
          _ = newSyncWith() shouldBe List(Some(BestChannel(testChannel, 3)))
        } yield ())
    }


    "when the best channel downgrades score" in {
      val (newSyncWith, localScores, remoteScores, _) = buildObserver()
      val testChannel = new LocalChannel()

      test(
        for {
          _ <- send(localScores)(1)
          _ = newSyncWith() shouldBe List(None)
          _ <- send(remoteScores)((testChannel, 3))
          _ = newSyncWith() shouldBe List(Some(BestChannel(testChannel, 3)))
          _ <- send(remoteScores)((testChannel, 2))
          _ = newSyncWith() shouldBe List(Some(BestChannel(testChannel, 2)))
        } yield ())
    }


  }
  "should emit None" - {
    "stop when local score is as good as network's" in {
      val (newSyncWith, localScores, remoteScores, closed) = buildObserver()
      val ch100 = new LocalChannel()

      test(for {
        _ <- send(localScores)(1)
        _ <- send(remoteScores)((ch100, 100))
        _ = newSyncWith()
        _ <- send(localScores)(100)
        _ = newSyncWith().last shouldBe None
      } yield ())
    }

    "stop when local score is better than network's" in {
      val (newSyncWith, localScores, remoteScores, closed) = buildObserver()
      val ch100 = new LocalChannel()

      test(for {
        _ <- send(localScores)(1)
        _ <- send(remoteScores)((ch100, 100))
        _ = newSyncWith()
        _ <- send(localScores)(101)
        _ = newSyncWith().last shouldBe None
      } yield ())
    }
  }

  "should not emit anything" - {
    "when current best channel is not changed and its score is not changed" - {

      "directly" in {
        val (newSyncWith, localScores, remoteScores, closed) = buildObserver()
        val ch100 = new LocalChannel()

        test(for {
          _ <- send(localScores)(1)
          _ <- send(remoteScores)((ch100, 100))
          _ = newSyncWith()
          _ <- send(remoteScores)((ch100, 100))
          _ = newSyncWith() shouldBe 'empty
        } yield ())
      }

      "indirectly" in {
        val (newSyncWith, localScores, remoteScores, closed) = buildObserver()
        val ch100 = new LocalChannel()

        test(for {
          _ <- send(localScores)(1)
          _ <- send(remoteScores)((ch100, 100))
          _ = newSyncWith()
          _ <- send(localScores)(2)
          _ = newSyncWith() shouldBe 'empty
        } yield ())
      }
    }
  }
}