package scorex.lagonaki.server

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import com.typesafe.config.ConfigFactory
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.api.http._
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.consensus.qora.api.http.QoraConsensusApiRoute
import scorex.lagonaki.api.http.{PaymentApiRoute, ScorexApiRoute}
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.consensus.qora.QoraLikeConsensusModule
import scorex.lagonaki.network.message._
import scorex.lagonaki.network.{BlockchainSyncer, NetworkController}
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction._
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.transaction.state.wallet.Wallet
import scorex.utils.{NTP, ScorexLogging}
import spray.can.Http

import scala.concurrent.ExecutionContext.Implicits.global


class LagonakiApplication(val settingsFilename: String) extends ScorexLogging {
  private val appConf = ConfigFactory.load().getConfig("app")

  implicit val consensusModule: ConsensusModule[_] =
    appConf.getString("consensusAlgo") match {
    case s:String if s.equalsIgnoreCase("nxt") =>
      new NxtLikeConsensusModule
    case s:String if s.equalsIgnoreCase("qora") =>
      new QoraLikeConsensusModule
    case algo =>
      log.error(s"Unknown consensus algo: $algo. Use NxtLikeConsensusModule instead.")
      new NxtLikeConsensusModule
  }

  implicit val settings = new LagonakiSettings(settingsFilename)
  implicit val transactionModule = new SimpleTransactionModule

  lazy val storedState = transactionModule.state
  lazy val blockchainImpl = transactionModule.history

  private implicit lazy val actorSystem = ActorSystem("lagonaki")
  lazy val networkController = actorSystem.actorOf(Props(classOf[NetworkController], this))
  lazy val blockchainSyncer = actorSystem.actorOf(Props(classOf[BlockchainSyncer], this, networkController))

  private lazy val walletFileOpt = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
  implicit lazy val wallet = new Wallet(walletFileOpt, settings.walletPassword, settings.walletSeed.get)

  val consensusApiRoute = consensusModule match {
    case ncm: NxtLikeConsensusModule =>
      new NxtConsensusApiRoute(ncm, blockchainImpl)
    case qcm: QoraLikeConsensusModule =>
      new QoraConsensusApiRoute(qcm, blockchainImpl)
  }

  lazy val routes = Seq(
    AddressApiRoute()(wallet, storedState),
    BlocksApiRoute()(blockchainImpl, wallet),
    TransactionsApiRoute(storedState),
    consensusApiRoute,
    WalletApiRoute()(wallet),
    PaymentApiRoute(this),
    PaymentApiRoute(this),
    ScorexApiRoute(this),
    SeedApiRoute
  )

  lazy val apiActor = actorSystem.actorOf(Props(classOf[CompositeHttpServiceActor], routes), "api")

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