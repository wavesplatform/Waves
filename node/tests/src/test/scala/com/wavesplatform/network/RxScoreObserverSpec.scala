package com.wavesplatform.network

import com.wavesplatform.network.RxScoreObserver.SyncWith
import com.wavesplatform.RxScheduler
import com.wavesplatform.test.FreeSpec
import io.netty.channel.Channel
import io.netty.channel.local.LocalChannel
import monix.eval.Coeval
import monix.reactive.subjects.PublishSubject

import scala.concurrent.duration._

class RxScoreObserverSpec extends FreeSpec with RxScheduler {
  override def testSchedulerName = "test-rx-score-observer"

  private def withObserver(f: (Coeval[Seq[SyncWith]], PublishSubject[BigInt], PublishSubject[(Channel, BigInt)], PublishSubject[Channel]) => Any) = {
    val localScores   = PublishSubject[BigInt]()
    val remoteScores  = PublishSubject[(Channel, BigInt)]()
    val channelClosed = PublishSubject[Channel]()
    val timeout       = PublishSubject[Channel]()

    val (syncWith, _) = RxScoreObserver(1.minute, 0.seconds, 0, localScores, remoteScores, channelClosed, timeout, testScheduler)

    try {
      f(newItems(syncWith.map(_.syncWith))(implicitScheduler), localScores, remoteScores, channelClosed)
    } finally {
      localScores.onComplete()
      remoteScores.onComplete()
      channelClosed.onComplete()
      timeout.onComplete()
    }
  }

  "should emit better channel" - {
    "when a new channel has the better score than the local one" in withObserver { (newSyncWith, localScores, remoteScores, _) =>
      val testChannel = new LocalChannel()

      test(for {
        _ <- send(localScores)(1)
        _ = newSyncWith() shouldBe List(None)
        _ <- send(remoteScores)((testChannel, 2))
        _ = newSyncWith() shouldBe List(Some(BestChannel(testChannel, 2)))
      } yield ())
    }

    "when the connection with the best one is closed" in withObserver { (newSyncWith, localScores, remoteScores, closed) =>
      val ch100 = new LocalChannel()
      val ch200 = new LocalChannel()

      test(for {
        _ <- send(localScores)(1)
        _ <- send(remoteScores)((ch200, 200))
        _ = newSyncWith().last shouldBe Some(BestChannel(ch200, 200))
        _ <- send(remoteScores)((ch100, 100))
        _ <- send(closed)(ch200)
        _ = newSyncWith().last shouldBe Some(BestChannel(ch100, 100))
      } yield ())
    }

    "when the best channel upgrades score" in withObserver { (newSyncWith, localScores, remoteScores, _) =>
      val testChannel = new LocalChannel()

      test(for {
        _ <- send(localScores)(1)
        _ = newSyncWith() shouldBe List(None)
        _ <- send(remoteScores)((testChannel, 2))
        _ = newSyncWith() shouldBe List(Some(BestChannel(testChannel, 2)))
        _ <- send(remoteScores)((testChannel, 3))
        _ = newSyncWith() shouldBe List(Some(BestChannel(testChannel, 3)))
      } yield ())
    }

    "when the best channel downgrades score" in withObserver { (newSyncWith, localScores, remoteScores, _) =>
      val testChannel = new LocalChannel()

      test(for {
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
    "stop when local score is as good as network's" in withObserver { (newSyncWith, localScores, remoteScores, _) =>
      val ch100 = new LocalChannel()

      test(for {
        _ <- send(localScores)(1)
        _ <- send(remoteScores)((ch100, 100))
        _ = newSyncWith()
        _ <- send(localScores)(100)
        _ = newSyncWith().last shouldBe None
      } yield ())
    }

    "stop when local score is better than network's" in withObserver { (newSyncWith, localScores, remoteScores, _) =>
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

      "directly" in withObserver { (newSyncWith, localScores, remoteScores, _) =>
        val ch100 = new LocalChannel()

        test(for {
          _ <- send(localScores)(1)
          _ <- send(remoteScores)((ch100, 100))
          _ = newSyncWith()
          _ <- send(remoteScores)((ch100, 100))
          _ = newSyncWith() shouldBe empty
        } yield ())
      }

      "indirectly" in withObserver { (newSyncWith, localScores, remoteScores, _) =>
        val ch100 = new LocalChannel()

        test(for {
          _ <- send(localScores)(1)
          _ <- send(remoteScores)((ch100, 100))
          _ = newSyncWith()
          _ <- send(localScores)(2)
          _ = newSyncWith() shouldBe empty
        } yield ())
      }
    }
  }
}
