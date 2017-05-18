package scorex.consensus.mining

import java.util.concurrent.locks.ReentrantReadWriteLock

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.{BlockchainSettings, MinerSettings}
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2._
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import scorex.consensus.mining.BlockGeneratorController._
import scorex.lagonaki.mocks.TestBlock
import scorex.network.ConnectedPeer
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.transaction.{HistoryWriter, UnconfirmedTransactionsStorage}
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.language.postfixOps

abstract class BlockGeneratorControllerSpecification extends TestKit(ActorSystem()) with ImplicitSender
  with fixture.FunSuiteLike with Matchers with MockFactory {

  val settings: MinerSettings

  case class F(blockGeneratorController: ActorRef, history: HistoryWriter, peerManager: TestProbe, time: TestTime)

  override type FixtureParam = F

  override protected def withFixture(test: OneArgTest): Outcome = {

    val history = HistoryWriterImpl(new MVStore.Builder().open(), new ReentrantReadWriteLock()).explicitGet()
    val testPeerManager: TestProbe = TestProbe("PeerManager")
    val testTime = new TestTime
    val blockGeneratorController: ActorRef = system.actorOf(Props(new BlockGeneratorController(
      settings,
      history,
      testTime,
      testPeerManager.ref,
      new Wallet(None, "", None),
      stub[StateReader],
      stub[BlockchainSettings],
      stub[UnconfirmedTransactionsStorage],
      null)))
    test(F(blockGeneratorController, history, testPeerManager, testTime))
  }

  def assertStatus(blockGeneratorController: ActorRef, status: BlockGeneratorController.Status): Unit = {
    blockGeneratorController ! GetStatus
    expectMsg(status.name)
  }
}


class EnabledBlockGeneratorControllerSpecification extends BlockGeneratorControllerSpecification {
  val settings = MinerSettings(
    enable = true,
    offline = true,
    quorum = 1,
    generationDelay = 1.second,
    intervalAfterLastBlockThenGenerationIsAllowed = 1.second,
    tfLikeScheduling = true)

  test("initial status is Idle") { f: F =>
    assertStatus(f.blockGeneratorController, Idle)
  }

  test("StopGeneration command stops") { f: F =>
    f.history.appendBlock(TestBlock.empty)
    f.blockGeneratorController ! StartGeneration
    f.blockGeneratorController ! StopGeneration
    assertStatus(f.blockGeneratorController, Idle)
    f.blockGeneratorController ! StopGeneration
    assertStatus(f.blockGeneratorController, Idle)
  }

  test("StartGeneration command starts generation when should generate because of genesis block") { f: F =>
    f.history.appendBlock(TestBlock.empty)
    assertStatus(f.blockGeneratorController, Idle)
    f.blockGeneratorController ! StartGeneration
    assertStatus(f.blockGeneratorController, Generating)
    f.blockGeneratorController ! StopGeneration
    assertStatus(f.blockGeneratorController, Idle)
  }

  test("StartGeneration command starts generation if intervalAfterLastBlockThenGenerationIsAllowed hasn't passed") { f: F =>
    f.history.appendBlock(TestBlock.empty)
    f.history.appendBlock(TestBlock.withReference(f.history.lastBlock.uniqueId))
    assertStatus(f.blockGeneratorController, Idle)
    f.time.setTime(500)
    f.blockGeneratorController ! StartGeneration
    assertStatus(f.blockGeneratorController, Generating)
  }

  test("StartGeneration command do not start generation if intervalAfterLastBlockThenGenerationIsAllowed has passed") { f: F =>
    f.history.appendBlock(TestBlock.empty)
    f.history.appendBlock(TestBlock.withReference(f.history.lastBlock.uniqueId))
    f.time.setTime(1001)
    f.blockGeneratorController ! StartGeneration
    assertStatus(f.blockGeneratorController, Idle)
  }

  test("Generating on no peers connected") { f: F =>
    f.history.appendBlock(TestBlock.empty)
    f.blockGeneratorController ! StartGeneration
    f.peerManager.expectMsg(GetConnectedPeersTyped)
    f.peerManager.reply(ConnectedPeers(Set.empty))
    assertStatus(f.blockGeneratorController, Generating)
  }
}

class DisabledBlockGeneratorControllerSpecification extends BlockGeneratorControllerSpecification {
  val settings = MinerSettings(
    enable = true,
    offline = false,
    quorum = 1,
    generationDelay = 1.second,
    intervalAfterLastBlockThenGenerationIsAllowed = 1.second,
    tfLikeScheduling = true)

  test("Suspended on no peers connected") { f: F =>
    f.history.appendBlock(TestBlock.empty)
    f.blockGeneratorController ! StartGeneration
    f.peerManager.expectMsg(GetConnectedPeersTyped)
    f.peerManager.reply(ConnectedPeers(Set.empty))
    assertStatus(f.blockGeneratorController, Suspended)
  }

  test("Generating on no peers connected") { f: F =>
    f.history.appendBlock(TestBlock.empty)
    f.blockGeneratorController ! StartGeneration
    f.peerManager.expectMsg(GetConnectedPeersTyped)
    f.peerManager.reply(ConnectedPeers(Set(stub[ConnectedPeer])))
    assertStatus(f.blockGeneratorController, Generating)
  }
}
