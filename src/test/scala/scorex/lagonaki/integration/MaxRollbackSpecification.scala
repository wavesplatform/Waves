package scorex.lagonaki.integration

import java.net.InetSocketAddress

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.scalatest._
import scorex.consensus.mining.BlockGeneratorController
import scorex.consensus.mining.BlockGeneratorController.{StartGeneration, StopGeneration}
import scorex.lagonaki.TestingCommons
import scorex.lagonaki.server.{LagonakiApplication, LagonakiSettings}
import scorex.network.peer.PeerManager
import scorex.network.peer.PeerManager._
import scorex.network.{Broadcast, ConnectedPeer, HistorySynchronizer, PeerConnectionHandler}
import scorex.utils.{ScorexLogging, untilTimeout}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.language.postfixOps


class MaxRollbackSpecification extends FunSuite with Matchers with BeforeAndAfter with ScorexLogging {

  import TestingCommons.initialize

  val testTimeout = 100.seconds

  val peers = initialize(Seq("settings-local1.json", "settings-local2.json"))

  val peer = peers.head
  val state = peer.transactionModule.blockStorage.state
  val history = peer.transactionModule.blockStorage.history

  implicit val timeout = Timeout(1.second)

  implicit val executionContext = ExecutionContext.global
  implicit val consensusModule = peer.consensusModule
  implicit val transactionModule = peer.transactionModule

  test("short fork") {

    val bestPeer = setUp(maxRollback / 2, strict = false)

    val bestPeerLastBlock = bestPeer.history.lastBlock
    val bestPeerScore = bestPeer.history.score()

    val syncingPeer = peers.filter(_ != bestPeer).head

    log.debug(s"syncingPeer is ${syncingPeer.settings.nodeName}")

    checkStates(
      testTimeout,
      Seq(
        HistorySynchronizer.GettingExtension,
        HistorySynchronizer.GettingBlock,
        HistorySynchronizer.Synced),
      syncingPeer,
      waitForFirst = true)

    syncingPeer.blockStorage.history.contains(bestPeerLastBlock) shouldBe true
    syncingPeer.history.score() >= bestPeerScore shouldBe true
  }

  test("too long fork") {

    val bestPeer = setUp(maxRollback + 1, strict = true)

    val bestPeerLastBlock = bestPeer.history.lastBlock
    val bestPeerScore = bestPeer.history.score()

    val syncingPeer = peers.filter(_ != bestPeer).head

    checkStates(
      testTimeout + historySynchronizerTimeout,
      Seq(HistorySynchronizer.GettingExtension/*, HistorySynchronizer.Syncing*/),
      syncingPeer,
      waitForFirst = true)

    untilTimeout(10 seconds) {
      assert(Await.result(syncingPeer.historySynchronizer ? HistorySynchronizer.GetStatus, 100 milliseconds) ==
        HistorySynchronizer.GettingExtension.name)
    }

    syncingPeer.blockStorage.history.contains(bestPeerLastBlock) shouldBe false
    syncingPeer.history.score() >= bestPeerScore shouldBe false
  }

  @tailrec
  final def checkStates(timeout: FiniteDuration,
                        waitForFirst: Boolean,
                        current: Option[HistorySynchronizer.Status],
                        follow: Seq[HistorySynchronizer.Status],
                        historySynchronizer: ActorRef): Unit = {
    val delay = 100 milliseconds

    implicit val t = Timeout(delay)

    follow match {
      case nextExpectedStatus :: tail if timeout > delay =>
        val nextTimeout = timeout - delay

        val nextStatus = Await.result((historySynchronizer ? HistorySynchronizer.GetStatus).mapTo[String], delay)

        //log.debug(s"I see status $nextStatus")

        Thread.sleep(delay.toMillis)

        current match {
          case Some(currentStatus) if currentStatus.name == nextStatus =>
            checkStates(nextTimeout, waitForFirst, current, follow, historySynchronizer)
          case _ =>
            if (nextStatus != nextExpectedStatus.name) {
              if (waitForFirst) {
                checkStates(nextTimeout, waitForFirst = true, None, follow, historySynchronizer)
              } else {
                fail(s"Wrong state order: found [$nextStatus], but should [${nextExpectedStatus.name}]")
              }
            } else {
              checkStates(nextTimeout, waitForFirst = false, Some(nextExpectedStatus), tail, historySynchronizer)
            }
        }
      case _ =>
        assert(follow.isEmpty, s"Remaining events: ${follow.map(_.name)}")
    }
  }

  def checkStates(timeout: FiniteDuration, follow: Seq[HistorySynchronizer.Status], app: LagonakiApplication, waitForFirst: Boolean = false): Unit =
    checkStates(timeout, waitForFirst, None, follow, app.historySynchronizer)

  def historySynchronizerTimeout: FiniteDuration = valueOf(_.historySynchronizerTimeout)

  def maxRollback: Int = valueOf(_.MaxRollback)

  def valueOf[T](f: LagonakiSettings => T) = {
    val values = peers.map(_.settings).map(f).toSet
    values.size shouldBe 1

    values.head
  }

  def setUp(forkLength: Int, strict: Boolean): LagonakiApplication = {
    val h = waitGenerationOfBlocks(3, peers)

    val last = peers.head.blockStorage.history.lastBlock

    untilTimeout(testTimeout, 1.second) {
      peers.foreach(_.blockStorage.history.contains(last) shouldBe true)
    }

    val blacklisted = disconnectPeers()

    waitGenerationOfBlocks(forkLength, peers, all = strict, startingFrom = if (strict) maxHeight() else h)

    stopGeneration()

    val bestPeer = untilTimeout(5.seconds) {
      val one = peers.head
      val another = peers.last

      one.history.height() != another.history.height() shouldBe true

      if (one.history.score() > another.history.score()) one else another
    }

    connectPeers(blacklisted)

    bestPeer
  }

  def maxHeight(): Int = peers.map(_.blockStorage.history.height()).max

  def waitGenerationOfBlocks(howMany: Int,
                             participants: Seq[LagonakiApplication],
                             all: Boolean = true,
                             startingFrom: Int = maxHeight()): Int = {

    val h = startingFrom + howMany

    untilTimeout(testTimeout, 1.second) {
      val heights = participants.map(_.blockStorage.history.height())
      log.info(s"Current heights are: $heights. Waiting for $h")

      if (all) heights.foreach(_ should be >= h) else heights.exists( _ >= h) shouldBe true

      heights.max
    }
  }

  def startGeneration(): Unit = {
    peers.foreach(_.blockGenerator ! StartGeneration)
  }

  def stopGeneration(): Unit = {
    log.info("Stop generation for all peers")
    peers.foreach(_.blockGenerator ! StopGeneration)
    untilTimeout(5.seconds) {
      peers.foreach { p =>
        Await.result(p.blockGenerator ? BlockGeneratorController.GetStatus, timeout.duration) shouldBe BlockGeneratorController.Syncing.name
      }
    }
  }

  def disconnectPeers(): Map[LagonakiApplication, Iterable[InetSocketAddress]] = {
    peers map {
      p =>
        val peerManager = p.peerManager

        val peersToBlock = Await.result((peerManager ? PeerManager.FilterPeers(Broadcast)).mapTo[Seq[ConnectedPeer]], timeout.duration)

        peersToBlock.foreach { _.handlerRef ! PeerConnectionHandler.Blacklist }

        untilTimeout(5.seconds) {
          Await.result((peerManager ? GetBlacklistedPeers).mapTo[Seq[String]], timeout.duration).nonEmpty shouldBe true
        }

        p -> peersToBlock.map(_.socketAddress)
    } toMap
  }

  def connectPeers(blacklisted: Map[LagonakiApplication, Iterable[InetSocketAddress]]): Unit = {
    blacklisted foreach {
      p =>
        val peerManager = p._1.peerManager

        for (blockedPeer <- p._2) {
          peerManager ! RemoveFromBlacklist(blockedPeer)
        }

        untilTimeout(5.seconds) {
          Await.result((peerManager ? GetBlacklistedPeers).mapTo[Seq[String]], timeout.duration).isEmpty shouldBe true
          Await.result((peerManager ? GetAllPeers).mapTo[Map[InetSocketAddress, _]], timeout.duration).isEmpty shouldBe false
        }
    }
  }
}