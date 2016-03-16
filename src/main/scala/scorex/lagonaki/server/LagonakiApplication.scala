package scorex.lagonaki.server

import java.io.{File, RandomAccessFile}
import java.nio.file.{Files, Paths}

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import scorex.api.http._
import scorex.app.{Application, ApplicationVersion}
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.consensus.qora.QoraLikeConsensusModule
import scorex.consensus.qora.api.http.QoraConsensusApiRoute
import scorex.crypto.ads.merkle.{AuthDataBlock, MerkleTree}
import scorex.crypto.hash.FastCryptographicHash
import scorex.lagonaki.api.http.{DebugApiRoute, PaymentApiRoute, PeersHttpService, ScorexApiRoute}
import scorex.network._
import scorex.perma.api.http.PermaConsensusApiRoute
import scorex.perma.consensus.PermaConsensusModule
import scorex.perma.network.{PermacoinMessagesRepo, SegmentsSynchronizer}
import scorex.perma.settings.PermaConstants
import scorex.perma.settings.PermaConstants._
import scorex.perma.storage.AuthDataStorage
import scorex.storage.Storage
import scorex.transaction._

import scala.reflect.runtime.universe._

class LagonakiApplication(val settingsFilename: String) extends Application {

  override val applicationName = "lagonaki"

  private val appConf = ConfigFactory.load().getConfig("app")

  override val appVersion = {
    val raw = appConf.getString("version")
    val parts = raw.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }

  override implicit lazy val settings = new LagonakiSettings(settingsFilename)

  override implicit lazy val consensusModule =
    appConf.getString("consensusAlgo") match {
      case s: String if s.equalsIgnoreCase("nxt") =>
        new NxtLikeConsensusModule
      case s: String if s.equalsIgnoreCase("qora") =>
        new QoraLikeConsensusModule
      case s: String if s.equalsIgnoreCase("perma") =>
        val treeDir = new File(settings.treeDir)
        treeDir.mkdirs()
        val authDataStorage: Storage[Long, AuthDataBlock[DataSegment]] = new AuthDataStorage(settings.authDataStorage)
        if (settings.isTrustedDealer) {
          log.info("TrustedDealer node")
          val tree = if (Files.exists(Paths.get(settings.treeDir + MerkleTree.TreeFileName + "0.mapDB"))) {
            log.info("Get existing tree")
            new MerkleTree(settings.treeDir, PermaConstants.n, PermaConstants.segmentSize, FastCryptographicHash)
          } else {
            val datasetFile = settings.treeDir + "/data.file"
            if (!Files.exists(Paths.get(datasetFile))) {
              log.info("Generating random data set")
              val f = new RandomAccessFile(datasetFile, "rw")
              val padding: Array[Byte] = Array.fill(PermaConstants.segmentSize - 8)(0: Byte)
              f.setLength(PermaConstants.n * PermaConstants.segmentSize)

              (0L until PermaConstants.n) foreach { i =>
                f.writeLong(i)
                f.write(padding)
              }
            }
            log.info("Calculate tree")
            val tree = MerkleTree.fromFile(datasetFile, settings.treeDir, PermaConstants.segmentSize, FastCryptographicHash)
            require(tree.nonEmptyBlocks == PermaConstants.n, s"${tree.nonEmptyBlocks} == ${PermaConstants.n}")

            log.info("Put ALL data to local storage")
            new File(settings.treeDir).mkdirs()
            def addBlock(i: Long): Unit = {
              authDataStorage.set(i, tree.byIndex(i).get)
              if (i > 0) addBlock(i - 1)
            }
            addBlock(PermaConstants.n - 1)
            authDataStorage.commit()

            tree
          }
          log.info("Test tree")
          require(settings.rootHash sameElements tree.rootHash, "Tree root hash differs from root hash in settings")
          require(tree.byIndex(PermaConstants.n - 1).isDefined)
          require(tree.byIndex(PermaConstants.n).isEmpty)
          val index = PermaConstants.n - 3
          val leaf = tree.byIndex(index).get
          require(leaf.check(index, tree.rootHash)(FastCryptographicHash))

        }
        val rootHash = settings.rootHash

        actorSystem.actorOf(Props(classOf[SegmentsSynchronizer], this, rootHash, authDataStorage))

        log.info("Create consensus module")
        new PermaConsensusModule(rootHash, Some(networkController))(authDataStorage)
      case nonsense =>
        sys.error(s"Unknown consensus algo: $nonsense")
    }

  override implicit lazy val transactionModule: SimpleTransactionModule = new SimpleTransactionModule()(settings, this)

  override lazy val blockStorage = transactionModule.blockStorage

  lazy val consensusApiRoute = consensusModule match {
    case ncm: NxtLikeConsensusModule =>
      new NxtConsensusApiRoute(this)
    case qcm: QoraLikeConsensusModule =>
      new QoraConsensusApiRoute(this)
    case pcm: PermaConsensusModule =>
      new PermaConsensusApiRoute(pcm, blockStorage)
  }

  override lazy val apiRoutes = Seq(
    BlocksApiRoute(this),
    TransactionsApiRoute(this),
    consensusApiRoute,
    WalletApiRoute(this),
    PaymentApiRoute(this),
    ScorexApiRoute(this),
    SeedApiRoute(this),
    PeersHttpService(this),
    AddressApiRoute(this),
    DebugApiRoute(this)
  )

  override lazy val apiTypes = Seq(
    typeOf[BlocksApiRoute],
    typeOf[TransactionsApiRoute],
    consensusApiRoute match {
      case nxt: NxtConsensusApiRoute => typeOf[NxtConsensusApiRoute]
      case qora: QoraConsensusApiRoute => typeOf[QoraConsensusApiRoute]
      case pcm: PermaConsensusApiRoute => typeOf[PermaConsensusApiRoute]
    },
    typeOf[WalletApiRoute],
    typeOf[PaymentApiRoute],
    typeOf[ScorexApiRoute],
    typeOf[SeedApiRoute],
    typeOf[PeersHttpService],
    typeOf[AddressApiRoute],
    typeOf[DebugApiRoute]
  )

  override lazy val additionalMessageSpecs = TransactionalMessagesRepo.specs ++ PermacoinMessagesRepo.specs

  //checks
  require(transactionModule.balancesSupport)
  require(transactionModule.accountWatchingSupport)

  actorSystem.actorOf(Props(classOf[UnconfirmedPoolSynchronizer], this))
}
