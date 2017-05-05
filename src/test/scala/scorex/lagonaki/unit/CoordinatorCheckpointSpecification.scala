package scorex.lagonaki.unit

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import com.wavesplatform.Application
import com.wavesplatform.history.BlockStorageImpl
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.StateReader
import org.h2.mvstore.MVStore
import scorex.ActorTestingCommons
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.network.BlockchainSynchronizer.GetExtension
import scorex.network.Coordinator.{AddBlock, ClearCheckpoint, SyncFinished}
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.ScoreObserver.CurrentScore
import scorex.network._
import scorex.network.message._
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.transaction._
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.utils.{NTP, Time}
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

class CoordinatorCheckpointSpecification extends ActorTestingCommons {

  val pk: PrivateKeyAccount = PrivateKeyAccount(Array.fill(32)(Random.nextInt(100).toByte))

  private val localConfig = ConfigFactory.parseString(
    s"""
       |waves {
       |  synchronization {
       |    max-rollback: 10
       |    max-chain-length: 11
       |    score-broadcast-interval: 1000s
       |  }
       |  checkpoints {
       |    public-key: "${Base58.encode(pk.publicKey)}"
       |  }
       |}
    """.stripMargin).withFallback(baseTestConfig).resolve()

  val wavesSettings: WavesSettings = WavesSettings.fromConfig(localConfig)

  val testBlockGenerator = TestProbe("blockGenerator")
  val testBlockchainSynchronizer = TestProbe("BlockChainSynchronizer")
  val testScoreObserver = TestProbe("ScoreObserver")
  val testPeerManager = TestProbe("PeerManager")
  val connectedPeer: ConnectedPeer = stub[ConnectedPeer]

  val db: MVStore = new MVStore.Builder().open()
  val blockStorage1 = new BlockStorageImpl(wavesSettings.blockchainSettings)
  val utxStorage1 = new UnconfirmedTransactionsDatabaseImpl(wavesSettings.utxSettings.size)

  class TestAppMock extends scorex.app.Application {
    lazy val networkController: ActorRef = networkControllerMock
    lazy val settings = wavesSettings
    lazy val blockGenerator: ActorRef = testBlockGenerator.ref
    lazy val blockchainSynchronizer: ActorRef = testBlockchainSynchronizer.ref
    lazy val peerManager: ActorRef = testPeerManager.ref

    lazy val scoreObserver: ActorRef = testScoreObserver.ref

    override def coordinator: ActorRef = system.actorOf(Props(classOf[Coordinator], this), "Coordinator")

    override def wallet: Wallet = new Wallet(None, "", None)

    override def time: Time = NTP

    override def utxStorage: UnconfirmedTransactionsStorage = utxStorage1

    override def history: History = blockStorage1.history

    override def stateReader: StateReader = blockStorage1.stateReader

    override def blockchainUpdater: BlockchainUpdater = blockStorage1.blockchainUpdater

    override def checkpoints: CheckpointService = blockStorage1.checkpointService

    override def messagesHandler: MessageHandler = ???

    override def upnp: UPnP = ???
  }

  val app = new TestAppMock()

  override lazy protected val actorRef: ActorRef = system.actorOf(Props(classOf[Coordinator], app))
  val gen = PrivateKeyAccount(Array(0.toByte))
  var score: Int = 10000

  def createBlock(reference: Array[Byte]): Block = {
    val version = 1: Byte
    val timestamp = System.currentTimeMillis()
    //val reference = Array.fill(Block.BlockIdLength)(id.toByte)
    val cbd = NxtLikeConsensusBlockData(score + 1, Array.fill(Block.GeneratorSignatureLength)(Random.nextInt(100).toByte))
    Block.buildAndSign(version, timestamp, reference, cbd, Seq[Transaction](), gen)
  }

  val genesisTimestamp: Long = System.currentTimeMillis()
  app.blockchainUpdater.processBlock(
    Block.genesis(
      NxtLikeConsensusBlockData(app.settings.blockchainSettings.genesisSettings.initialBaseTarget, Array.fill(DigestSize)(0: Byte)),
      Application.genesisTransactions(app.settings.blockchainSettings.genesisSettings), genesisTimestamp)).explicitGet()

  def before(): Unit = {
    app.blockchainUpdater.removeAfter(blockStorage1.history.genesis.uniqueId)
    actorRef ! ClearCheckpoint
    networkController.ignoreMsg {
      case SendToNetwork(m, _) => m.spec == ScoreMessageSpec
      case m => true
    }
  }

  def genNBlocks(n: Int): Unit = {
    genNBlocks(1, n)
  }

  def genNBlocks(fromHeight: Int = 1, n: Int): Unit = {
    var ref = blockStorage1.history.blockAt(fromHeight).get.uniqueId

    (2 to n).foreach { i =>
      val b = createBlock(ref)
      actorRef ! AddBlock(b, Some(connectedPeer))
      ref = b.uniqueId
    }

    awaitCond(blockStorage1.history.height() == n, max = 20.seconds)
  }

  def genCheckpoint(historyPoints: Seq[Int]): Checkpoint = {
    val items = historyPoints.map(h => BlockCheckpoint(h, blockStorage1.history.blockAt(h).get.signerData.signature))
    val checkpoint = Checkpoint(items, Array()).signedBy(pk.privateKey)
    checkpoint
  }

  "rollback if block doesn't match checkPoint" in {
    before()
    genNBlocks(9)

    val toRollback = 9
    val chpBlock = createBlock(blockStorage1.history.blockAt(toRollback - 1).get.uniqueId)
    val p = BlockCheckpoint(toRollback, chpBlock.signerData.signature)
    val firstChp = genCheckpoint(Seq(7, 5, 3))

    val checkpoint = Checkpoint(p +: firstChp.items, Array()).signedBy(pk.privateKey)

    actorRef ! DataFromPeer(CheckpointMessageSpec.messageCode, checkpoint: Checkpoint, connectedPeer)

    networkController.awaitCond(app.history.height() == 7)
  }

  "blacklist peer if it sends block that is different from checkPoint" in {
    before()
    genNBlocks(9)

    sendCheckpoint(Seq(9, 7, 5, 3))

    val parentId = blockStorage1.history.blockAt(8).get.uniqueId
    app.blockchainUpdater.removeAfter(parentId)
    val difBloc = createBlock(parentId)
    val badPeer = stub[ConnectedPeer]

    actorRef ! AddBlock(difBloc, Some(badPeer))

    Thread.sleep(1000)

    (badPeer.blacklist _).verify().once
    verifyExpectations
  }

  def sendCheckpoint(historyPoints: Seq[Int]): Unit = {
    val checkpoint = genCheckpoint(historyPoints)

    actorRef ! DataFromPeer(CheckpointMessageSpec.messageCode, checkpoint, connectedPeer)

    networkController.expectMsgPF() {
      case SendToNetwork(Message(spec, _, _), _) => spec should be(CheckpointMessageSpec)
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
    (goodPeer.blacklist _).expects().returning(()).never()

    actorRef ! SyncFinished(success = true, Some(lastCommonBlockId, fork.iterator, Some(goodPeer)))

    Thread.sleep(1000)
    awaitCond(app.history.height() == 12)

  }

}
