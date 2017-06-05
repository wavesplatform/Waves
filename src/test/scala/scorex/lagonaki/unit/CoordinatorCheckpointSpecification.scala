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
  val (checkpoints1, history1, stateReader1, blockchainUpdater1) = BlockStorageImpl(wavesSettings.blockchainSettings)
  val utxStorage1 = new UnconfirmedTransactionsDatabaseImpl(wavesSettings.utxSettings.size)
  class ValidBlockCoordinator extends Coordinator(networkControllerMock, testBlockchainSynchronizer.ref,
    testBlockGenerator.ref, testPeerManager.ref, testScoreObserver.ref, blockchainUpdater1, NTP, utxStorage1, history1, stateReader1, checkpoints1, wavesSettings) {
    override def isBlockValid(b: Block): Either[ValidationError, Unit] = Right(())
  }

  override lazy protected val actorRef: ActorRef = system.actorOf(Props(new ValidBlockCoordinator))
  val gen = PrivateKeyAccount(Array(0.toByte))
  var score: Int = 10000

  def createBlock(reference: ByteStr): Block = {
    val version = 1: Byte
    val timestamp = System.currentTimeMillis()
    //val reference = Array.fill(Block.BlockIdLength)(id.toByte)
    val cbd = NxtLikeConsensusBlockData(score + 1, Array.fill(Block.GeneratorSignatureLength)(Random.nextInt(100).toByte))
    Block.buildAndSign(version, timestamp, reference, cbd, Seq[Transaction](), gen)
  }

  val genesisTimestamp: Long = System.currentTimeMillis()
  blockchainUpdater1.processBlock(
    Block.genesis(
      NxtLikeConsensusBlockData(wavesSettings.blockchainSettings.genesisSettings.initialBaseTarget, Array.fill(DigestSize)(0: Byte)),
      Application.genesisTransactions(wavesSettings.blockchainSettings.genesisSettings), genesisTimestamp)).explicitGet()

  def before(): Unit = {
    blockchainUpdater1.removeAfter(history1.genesis.uniqueId)
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
    var ref = history1.blockAt(fromHeight).get.uniqueId

    (2 to n).foreach { i =>
      val b = createBlock(ref)
      actorRef ! AddBlock(b, Some(connectedPeer))
      ref = b.uniqueId
    }

    awaitCond(history1.height() == n, max = 20.seconds)
  }

  def genCheckpoint(historyPoints: Seq[Int]): Checkpoint = {
    val items = historyPoints.map(h => BlockCheckpoint(h, history1.blockAt(h).get.uniqueId.arr))
    val checkpoint = Checkpoint(items, Array()).signedBy(pk.privateKey)
    checkpoint
  }

  "rollback if block doesn't match checkPoint" in {
    before()
    genNBlocks(9)

    val toRollback = 9
    val chpBlock = createBlock(history1.blockAt(toRollback - 1).get.uniqueId)
    val p = BlockCheckpoint(toRollback, chpBlock.uniqueId.arr)
    val firstChp = genCheckpoint(Seq(7, 5, 3))

    val checkpoint = Checkpoint(p +: firstChp.items, Array()).signedBy(pk.privateKey)

    actorRef ! DataFromPeer(CheckpointMessageSpec.messageCode, checkpoint: Checkpoint, connectedPeer)

    networkController.awaitCond(history1.height() == 7)
  }

  "blacklist peer if it sends block that is different from checkPoint" in {
    before()
    genNBlocks(9)

    sendCheckpoint(Seq(9, 7, 5, 3))

    val parentId = history1.blockAt(8).get.uniqueId
    blockchainUpdater1.removeAfter(parentId)
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

    var parent = history1.blockAt(5).get.uniqueId
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

    var parent = history1.blockAt(7).get.uniqueId
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
    awaitCond(history1.height() == 12)
  }
}
