package scorex.lagonaki.unit

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import org.h2.mvstore.MVStore
import org.scalatest.DoNotDiscover
import play.api.libs.json.JsObject
import scorex.ActorTestingCommons
import scorex.account.PrivateKeyAccount
import scorex.app.Application
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, WavesConsensusModule}
import scorex.network.BlockchainSynchronizer.GetExtension
import scorex.network.Coordinator.{AddBlock, ClearCheckpoint, SyncFinished}
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.ScoreObserver.CurrentScore
import scorex.network._
import scorex.network.message.{BasicMessagesRepo, Message}
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.settings.{ChainParameters, SettingsMock}
import scorex.transaction.SimpleTransactionModule.StoredInBlock
import scorex.transaction._

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps
import scala.util.Random
import scala.concurrent.duration._

class CoordinatorCheckpointSpecification extends ActorTestingCommons {

  val pk = new PrivateKeyAccount(Array.fill(32)(Random.nextInt(100).toByte))

  object TestSettings extends SettingsMock {
    override lazy val quorum: Int = 1
    override lazy val scoreBroadcastDelay: FiniteDuration = 1000.seconds
    override lazy val MaxRollback: Int = 10

    override lazy val checkpointPublicKey: Option[Array[Byte]] = Some(pk.publicKey)

  }

  val testBlockGenerator = TestProbe("blockGenerator")
  val testBlockchainSynchronizer = TestProbe("BlockChainSynchronizer")
  val testScoreObserver = TestProbe("ScoreObserver")
  val testPeerManager = TestProbe("PeerManager")
  val connectedPeer: ConnectedPeer = stub[ConnectedPeer]

  val db = new MVStore.Builder().open()

  trait TestAppMock extends Application {
    lazy implicit val consensusModule: ConsensusModule[NxtLikeConsensusBlockData] = new WavesConsensusModule(ChainParameters.Disabled, 5.seconds) {
      override def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean = true
    }
    lazy implicit val transactionModule: TransactionModule[StoredInBlock] = new SimpleTransactionModule(ChainParameters.Disabled)(TestSettings, this)
    lazy val basicMessagesSpecsRepo: BasicMessagesRepo = new BasicMessagesRepo()
    lazy val networkController: ActorRef = networkControllerMock
    lazy val settings = TestSettings
    lazy val blockGenerator: ActorRef = testBlockGenerator.ref
    lazy val blockchainSynchronizer: ActorRef = testBlockchainSynchronizer.ref
    lazy val peerManager: ActorRef = testPeerManager.ref
    lazy val history: History = transactionModule.blockStorage.history

    lazy val blockStorage: BlockStorage = transactionModule.blockStorage
    lazy val scoreObserver: ActorRef = testScoreObserver.ref
  }

  lazy val app = stub[TestAppMock]

  override lazy protected val actorRef: ActorRef = system.actorOf(Props(classOf[Coordinator], app))
  val gen = new PrivateKeyAccount(Array(0.toByte))
  var score: Int = 10000

  def createBlock(reference: Array[Byte]): Block = {
    val version = 1: Byte
    val timestamp = System.currentTimeMillis()
    //val reference = Array.fill(Block.BlockIdLength)(id.toByte)
    val cbd = new NxtLikeConsensusBlockData {
      override val generationSignature: Array[Byte] = Array.fill(WavesConsensusModule.GeneratorSignatureLength)(
        Random.nextInt(100).toByte)
      override val baseTarget: Long = score + 1
    }
    Block.buildAndSign(version, timestamp, reference, cbd, Seq[Transaction](), gen)
  }

  implicit val consensusModule: ConsensusModule[NxtLikeConsensusBlockData] = app.consensusModule
  implicit val transactionModule: TransactionModule[StoredInBlock] = app.transactionModule
  private lazy val repo = app.basicMessagesSpecsRepo
  val genesisTimestamp: Long = System.currentTimeMillis()
  if (transactionModule.blockStorage.history.isEmpty) {
    transactionModule.blockStorage.appendBlock(Block.genesis(genesisTimestamp))
  }

  def before(): Unit = {
    app.blockStorage.removeAfter(app.history.genesis.uniqueId)
    actorRef ! ClearCheckpoint
    networkController.ignoreMsg {
      case SendToNetwork(m, _) => m.spec == repo.ScoreMessageSpec
      case m => true
    }
  }

  def genNBlocks(n: Int): Unit = {
    genNBlocks(1, n)
  }

  def genNBlocks(fromHeight: Int = 1, n: Int): Unit = {
    var ref = app.history.blockAt(fromHeight).get.uniqueId

    (2 to n).foreach { i =>
      val b = createBlock(ref)
      actorRef ! AddBlock(b, Some(connectedPeer))
      ref = b.uniqueId
    }

    awaitCond(app.history.height() == n, max = 20.seconds)
  }

  def genCheckpoint(historyPoints: Seq[Int]): Checkpoint = {
    val items = historyPoints.map(h => BlockCheckpoint(h, app.history.blockAt(h).get.signerDataField.value.signature))
    val checkpoint = Checkpoint(items, Array()).signedBy(pk.privateKey)
    checkpoint
  }

  "rollback if block doesn't match checkPoint" in {
    before()
    genNBlocks(9)

    val toRollback = 9
    val chpBlock = createBlock(app.history.blockAt(toRollback - 1).get.uniqueId)
    val p = BlockCheckpoint(toRollback, chpBlock.signerDataField.value.signature)
    val firstChp = genCheckpoint(Seq(7, 5, 3))

    val checkpoint = Checkpoint(p +: firstChp.items, Array()).signedBy(pk.privateKey)

    actorRef ! DataFromPeer(repo.CheckpointMessageSpec.messageCode, checkpoint: Checkpoint, connectedPeer)

    networkController.awaitCond(app.history.height() == 7)
  }

  "blacklist peer if it sends block that is different from checkPoint" in {
    before()
    genNBlocks(9)

    sendCheckpoint(Seq(9, 7, 5, 3))

    val parentId = app.history.blockAt(8).get.uniqueId
    app.blockStorage.removeAfter(parentId)
    val difBloc = createBlock(parentId)
    val badPeer = stub[ConnectedPeer]

    actorRef ! AddBlock(difBloc, Some(badPeer))

    Thread.sleep(1000)

    (badPeer.blacklist _).verify().once
    verifyExpectations
  }

  def sendCheckpoint(historyPoints: Seq[Int]): Unit = {
    val checkpoint = genCheckpoint(historyPoints)

    actorRef ! DataFromPeer(repo.CheckpointMessageSpec.messageCode, checkpoint, connectedPeer)

    networkController.expectMsgPF() {
      case SendToNetwork(Message(spec, _, _), _) => spec should be(repo.CheckpointMessageSpec)
      case _ => fail("Checkpoint hasn't been sent")
    }
  }

  def moveCoordinatorToSyncState(): Unit = {
    actorRef ! CurrentScore(Seq(connectedPeer -> Long.MaxValue))
    testPeerManager.expectMsg(GetConnectedPeersTyped)
    actorRef ! ConnectedPeers(Set(connectedPeer))
    testBlockchainSynchronizer.expectMsgType[GetExtension]
  }

  "validate checkpoint on receiving new fork" in {
    before()
    genNBlocks(10)
    sendCheckpoint(Seq(7, 4, 3))

    var parent = app.history.blockAt(5).get.uniqueId
    val lastCommonBlockId = parent
    val fork = Seq.fill(5) {
      val b = createBlock(parent)
      parent = b.uniqueId
      b
    }

    moveCoordinatorToSyncState()

    val forkPeer = stub[ConnectedPeer]

    actorRef ! SyncFinished(success = true, Some(lastCommonBlockId, fork.iterator, Some(forkPeer)))

    Thread.sleep(1000)

    (forkPeer.blacklist _).verify
  }

  "accept fork if it matches checkpoint" in {
    before()
    genNBlocks(10)
    sendCheckpoint(Seq(7, 4, 3))

    var parent = app.history.blockAt(7).get.uniqueId
    val lastCommonBlockId = parent
    val fork = Seq.fill(5) {
      val b = createBlock(parent)
      parent = b.uniqueId
      b
    }

    moveCoordinatorToSyncState()

    val goodPeer = mock[ConnectedPeer]
    (goodPeer.blacklist _).expects().returning().never()

    actorRef ! SyncFinished(success = true, Some(lastCommonBlockId, fork.iterator, Some(goodPeer)))

    Thread.sleep(1000)
    awaitCond(app.history.height() == 12)

  }

}