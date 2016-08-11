package scorex.network

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.network.ScoreObserver.{ConsideredValue, GetScore, UpdateScore}
import scorex.settings.SettingsMock
import scorex.transaction.History.BlockchainScore

import scala.concurrent.Await
import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.{implicitConversions, postfixOps}

class ScoreObserverSpecification extends ActorTestingCommons {

  val testCoordinator = TestProbe("Coordinator")

  object TestSettings extends SettingsMock {
    override val scoreTTL: FiniteDuration = 1 second
  }

  trait App extends ApplicationMock {
    override lazy val settings = TestSettings
    override lazy val coordinator: ActorRef = testCoordinator.ref
  }

  protected override val actorRef = system.actorOf(Props(classOf[ScoreObserver], stub[App]))

  testSafely {
    "no-score case" in {
      scores shouldBe empty
    }

    "consider incoming updates" - {

      def peer(port: Int) = ConnectedPeer(new InetSocketAddress(port), null)

      def expectScores(values: (ConnectedPeer, BlockchainScore)*): ConsideredValue =
        testCoordinator.expectMsg(ConsideredValue(values.toSeq))

      def updateScore(value: (ConnectedPeer, BlockchainScore)): Unit = actorRef ! UpdateScore(Some(value))

      updateScore(peer(1) -> BigInt(100))
      expectScores(peer(1) -> BigInt(100))

      updateScore(peer(3) -> BigInt(300))
      expectScores(peer(1) -> BigInt(100), peer(3) -> BigInt(300))

      "no update in case of lesser score" in {
        val lesserScore = BigInt(200)

        updateScore(peer(1) -> lesserScore)
        updateScore(peer(2) -> lesserScore)
        updateScore(peer(7) -> lesserScore)

        expectNoMsg(testDuration)
      }

      "clean old scores on timeout" in {
        def sleepHalfTTL(): Unit = Thread sleep TestSettings.scoreTTL.toMillis / 2 + 100

        sleepHalfTTL()

        scores should be(Seq(peer(1) -> BigInt(100), peer(3) -> BigInt(300)))

        val tinyScore = BigInt(10)

        updateScore(peer(5) -> tinyScore)
        sleepHalfTTL()

        scores shouldEqual Seq(peer(5) -> tinyScore)

        sleepHalfTTL()
        scores shouldBe empty

        val secondRoundScore = BigInt(1)

        updateScore(peer(1) -> secondRoundScore)
        expectScores(peer(1) -> secondRoundScore)
      }
    }
  }

  private def scores = Await.result((actorRef ? GetScore).mapTo[ConsideredValue], testDuration).scores
}
