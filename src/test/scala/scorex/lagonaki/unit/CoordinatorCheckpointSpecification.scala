package scorex.lagonaki.unit

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import org.h2.mvstore.MVStore
import org.scalatest.{BeforeAndAfter, BeforeAndAfterEach}
import scorex.ActorTestingCommons
import scorex.account.PrivateKeyAccount
import scorex.app.Application
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.network.Coordinator.AddBlock
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network._
import scorex.network.message.{BasicMessagesRepo, Message}
import scorex.settings.SettingsMock
import scorex.transaction.SimpleTransactionModule.StoredInBlock
import scorex.transaction._

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps
import scala.util.Random

class CoordinatorCheckpointSpecification extends ActorTestingCommons with BeforeAndAfter {

  val pk = new PrivateKeyAccount(Array.fill(32)(Random.nextInt(100).toByte))

  before {
    println("NEFORE")
    app.blockStorage.removeAfter(app.history.genesis.uniqueId)
  }

  object TestSettings extends SettingsMock with TransactionSettings {
    var checkpoint: Option[Array[Byte]] = Some(pk.privateKey)
    override lazy val quorum: Int = 1
    override lazy val scoreBroadcastDelay: FiniteDuration = 1000 seconds
    override lazy val MaxRollback: Int = 10

    override lazy val checkpointPublicKey: Option[Array[Byte]] = Some(pk.publicKey)
    override lazy val checkpointPrivateKey: Option[Array[Byte]] = checkpoint
  }

  val testblockGenerator = TestProbe("blockGenerator")
  val testBlockchainSynchronizer = TestProbe("BlockChainSynchronizer")
  val testPeerManager = TestProbe("PeerManager")
  val connectedPeer = stub[ConnectedPeer]

  val db = new MVStore.Builder().open()

  trait TestAppMock extends Application {
    implicit val consensusModule: ConsensusModule[NxtLikeConsensusBlockData] = new NxtLikeConsensusModule() {
      override def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean = true
    }
    implicit val transactionModule: TransactionModule[StoredInBlock] = new SimpleTransactionModule()(TestSettings, this)
    lazy val basicMessagesSpecsRepo: BasicMessagesRepo = new BasicMessagesRepo()
    lazy val networkController: ActorRef = networkControllerMock
    lazy val settings = TestSettings
    lazy val blockGenerator: ActorRef = testblockGenerator.ref
    lazy val blockchainSynchronizer: ActorRef = testBlockchainSynchronizer.ref
    lazy val peerManager: ActorRef = testPeerManager.ref
    lazy val history: History = transactionModule.blockStorage.history

    lazy val blockStorage: BlockStorage = transactionModule.blockStorage
  }

  lazy val app = stub[TestAppMock]

  override lazy protected val actorRef: ActorRef = system.actorOf(Props(classOf[Coordinator], app))
  val gen = new PrivateKeyAccount(Array(0.toByte))
  var score = 10000

  def createBlock(reference: Array[Byte]): Block = {
    val version = 1: Byte
    val timestamp = System.currentTimeMillis()
    //val reference = Array.fill(Block.BlockIdLength)(id.toByte)
    val cbd = new NxtLikeConsensusBlockData {
      override val generationSignature: Array[Byte] = Array.fill(NxtLikeConsensusModule.GeneratorSignatureLength)(
        Random.nextInt(100).toByte)
      override val baseTarget: Long = score + 1
    }
    Block.buildAndSign(version, timestamp, reference, cbd, Seq[Transaction](), gen)
  }

  implicit val consensusModule = app.consensusModule
  implicit val transactionModule = app.transactionModule
  private lazy val repo = app.basicMessagesSpecsRepo
  val genesisTimestamp = System.currentTimeMillis()
  if (transactionModule.blockStorage.history.isEmpty) {
    transactionModule.blockStorage.appendBlock(Block.genesis(genesisTimestamp))
  }


  "broadcast checkPoint" in {
    var ref = app.history.genesis.uniqueId

    (2 to TestSettings.MaxBlocksChunks).foreach { i =>
      val b = createBlock(ref)
      actorRef ! AddBlock(b, Some(connectedPeer))
      ref = b.uniqueId
    }
    networkController.ignoreMsg {
      case SendToNetwork(m, Broadcast) => m.spec.messageCode != 100
      case m => true
    }

    networkController.expectMsgPF() {
      case SendToNetwork(Message(spec, _, _), Broadcast) => spec should be(repo.CheckpointMessageSpec)
      case _ => fail("Checkpoint hasn't been sent")
    }

    networkController.ignoreNoMsg()
   }

  "rollback if block doesn't match checkPoint" in {
    TestSettings.checkpoint = None
    var ref = app.history.genesis.uniqueId

    (2 to 10).foreach { i =>
      val b = createBlock(ref)
      actorRef ! AddBlock(b, Some(connectedPeer))
      ref = b.uniqueId
    }

    awaitCond(app.history.height() ==  10)

    val historyPoints = Seq(7, 5, 3)
    val toRolback = 9
    val chpBlock = createBlock(app.history.blockAt(toRolback - 1).get.uniqueId)
    val items = historyPoints.map(h => BlockCheckpoint(h, app.history.blockAt(h).get.signerDataField.value.signature))
    val p = BlockCheckpoint(toRolback, chpBlock.signerDataField.value.signature)

    val checkpoint = Checkpoint(p +: items, Array()).signedBy(pk.privateKey)

    actorRef ! DataFromPeer(repo.CheckpointMessageSpec.messageCode, checkpoint: Checkpoint, connectedPeer)

    networkController.awaitCond(app.history.height() ==  7)
  }

  "blacklist peer if it sends block that is different from checkPoint" in {
    TestSettings.checkpoint = None
    var ref = app.history.genesis.uniqueId

    networkController.ignoreMsg {
      case SendToNetwork(m, _) => m.spec.messageCode == 24
      case m => true
    }

    (2 to 9).foreach { i =>
      val b = createBlock(ref)
      actorRef ! AddBlock(b, Some(connectedPeer))
      ref = b.uniqueId
    }

    awaitCond(app.history.height() ==  9)

    val historyPoints = Seq(9, 7, 5, 3)
    val items = historyPoints.map(h => BlockCheckpoint(h, app.history.blockAt(h).get.signerDataField.value.signature))
    val checkpoint = Checkpoint(items, Array()).signedBy(pk.privateKey)

    actorRef ! DataFromPeer(repo.CheckpointMessageSpec.messageCode, checkpoint: Checkpoint, connectedPeer)

    networkController.expectMsgPF() {
      case SendToNetwork(Message(spec, _, _), _) => spec should be(repo.CheckpointMessageSpec)
      case m => println(m); fail("Checkpoint hasn't been sent")
    }

    val parentId = app.history.blockAt(8).get.uniqueId
    app.blockStorage.removeAfter(parentId)
    val difBloc = createBlock(parentId)
    val badPeer = mock[ConnectedPeer]
    (badPeer.blacklist _).expects
    actorRef ! AddBlock(difBloc, Some(badPeer))

    expectNoMsg()

    verifyExpectations
  }
}
