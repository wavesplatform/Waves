package scorex.lagonaki.server

import java.io.{File, RandomAccessFile}
import java.nio.file.{Files, Paths}

import akka.actor.Props
import akka.io.IO
import com.typesafe.config.ConfigFactory
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.api.http._
import scorex.app.Application
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.consensus.qora.QoraLikeConsensusModule
import scorex.consensus.qora.api.http.QoraConsensusApiRoute
import scorex.crypto.ads.merkle.{AuthDataBlock, MerkleTree}
import scorex.lagonaki.api.http.{PaymentApiRoute, PeersHttpService, ScorexApiRoute}
import scorex.lagonaki.network.message._
import scorex.lagonaki.network.{BlockchainSyncer, NetworkController}
import scorex.perma.Storage.AuthDataStorage
import scorex.perma.consensus.PermaConsensusModule
import scorex.perma.consensus.http.PermaConsensusApiRoute
import scorex.perma.settings.Constants
import scorex.perma.settings.Constants._
import scorex.storage.Storage
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction._
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.transaction.state.wallet.{Payment, Wallet}
import scorex.utils.{NTP, ScorexLogging}
import spray.can.Http

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe._

class LagonakiApplication(val settingsFilename: String)
  extends Application with ScorexLogging {

  override val applicationName = "lagonaki"

  private val appConf = ConfigFactory.load().getConfig("app")

  override implicit val settings = new LagonakiSettings(settingsFilename)

  override implicit val consensusModule =
    appConf.getString("consensusAlgo") match {
      case s: String if s.equalsIgnoreCase("nxt") =>
        new NxtLikeConsensusModule
      case s: String if s.equalsIgnoreCase("qora") =>
        new QoraLikeConsensusModule
      case s: String if s.equalsIgnoreCase("perma") =>
        val tree = if (Files.exists(Paths.get(settings.treeDir + "/tree0.mapDB"))) {
          log.info("Get existing tree")
          new MerkleTree(settings.treeDir, Constants.n, Constants.segmentSize, Constants.hash)
        } else {
          log.info("Generating random data set")
          val treeDir = new File(settings.treeDir)
          treeDir.mkdirs()
          val datasetFile = settings.treeDir + "/data.file"
          new RandomAccessFile(datasetFile, "rw").setLength(Constants.n * Constants.segmentSize)
          log.info("Calculate tree")
          val tree = MerkleTree.fromFile(datasetFile, settings.treeDir, Constants.segmentSize, Constants.hash)
          require(tree.nonEmptyBlocks == Constants.n, s"${tree.nonEmptyBlocks} == ${Constants.n}")
          tree
        }

        log.info("Test tree")
        val index = Constants.n - 3
        val leaf = tree.byIndex(index).get
        require(leaf.check(index, tree.rootHash)(Constants.hash))

        log.info("Put ALL data to local storage")
        new File(settings.treeDir).mkdirs()
        val authDataStorage: Storage[Long, AuthDataBlock[DataSegment]] = new AuthDataStorage(settings.authDataStorage)
        def addBlock(i: Long): Unit = {
          authDataStorage.set(i, tree.byIndex(i).get)
          if (i > 0) {
            addBlock(i - 1)
          }
        }
        addBlock(Constants.n - 1)

        log.info("Create consensus module")
        new PermaConsensusModule(tree.rootHash)(authDataStorage)
      case algo =>
        log.error(s"Unknown consensus algo: $algo. Use NxtLikeConsensusModule instead.")
        new NxtLikeConsensusModule
    }

  override implicit val transactionModule: SimpleTransactionModule = new SimpleTransactionModule

  lazy val networkController = actorSystem.actorOf(Props(classOf[NetworkController], this))
  lazy val blockchainSyncer = actorSystem.actorOf(Props(classOf[BlockchainSyncer], this, networkController, settings))

  private lazy val walletFileOpt = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
  implicit lazy val wallet = new Wallet(walletFileOpt, settings.walletPassword, settings.walletSeed.get)

  lazy val storedState = transactionModule.state
  lazy val blockchainImpl = transactionModule.history

  val consensusApiRoute = consensusModule match {
    case ncm: NxtLikeConsensusModule =>
      new NxtConsensusApiRoute(ncm, blockchainImpl)
    case qcm: QoraLikeConsensusModule =>
      new QoraConsensusApiRoute(qcm, blockchainImpl)
    case pcm: PermaConsensusModule =>
      new PermaConsensusApiRoute(pcm, blockchainImpl)
  }

  override lazy val apiRoutes = Seq(
    BlocksApiRoute(blockchainImpl, wallet),
    TransactionsApiRoute(storedState),
    consensusApiRoute,
    WalletApiRoute(wallet),
    PaymentApiRoute(this),
    ScorexApiRoute(this),
    SeedApiRoute(),
    PeersHttpService(this),
    AddressApiRoute(wallet, storedState)
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
    typeOf[AddressApiRoute]
  )

  def checkGenesis(): Unit = {
    if (blockchainImpl.isEmpty) {
      val genesisBlock = Block.genesis()
      storedState.processBlock(genesisBlock)
      blockchainImpl.appendBlock(genesisBlock).ensuring(_.height() == 1)
      log.info("Genesis block has been added to the state")
    }
  }.ensuring(blockchainImpl.height() >= 1)

  def run() {
    require(transactionModule.balancesSupport)
    require(transactionModule.accountWatchingSupport)

    checkGenesis()

    blockchainSyncer ! Unit //initializing

    IO(Http) ! Http.Bind(apiActor, interface = "0.0.0.0", port = settings.rpcPort)

    //CLOSE ON UNEXPECTED SHUTDOWN
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        stopAll()
      }
    })
  }

  def stopAll() = synchronized {
    log.info("Stopping message processor")
    networkController ! NetworkController.ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>
      //CLOSE WALLET
      log.info("Closing wallet")
      wallet.close()

      //FORCE CLOSE
      log.info("Exiting from the app...")
      System.exit(0)
    }

  }

  def onNewOffchainTransaction(transaction: LagonakiTransaction) =
    if (UnconfirmedTransactionsDatabaseImpl.putIfNew(transaction)) {
      networkController ! NetworkController.BroadcastMessage(TransactionMessage(transaction))
    }

  def createPayment(payment: Payment): Option[PaymentTransaction] = {
    wallet.privateKeyAccount(payment.sender).map { sender =>
      createPayment(sender, new Account(payment.recipient), payment.amount, payment.fee)
    }
  }

  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): PaymentTransaction = {
    val time = NTP.correctedTime()
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
    val payment = new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, sig)
    if (payment.validate() == ValidationResult.ValidateOke) {
      onNewOffchainTransaction(payment)
    }
    payment
  }
}