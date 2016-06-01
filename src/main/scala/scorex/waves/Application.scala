package scorex.waves

import akka.actor.Props
import akka.http.scaladsl.Http
import com.typesafe.config.ConfigFactory
import scorex.account.Account
import scorex.api.http._
import scorex.app.ApplicationVersion
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.network.{PeerSynchronizer, TransactionalMessagesRepo, UnconfirmedPoolSynchronizer}
import scorex.transaction.{BalanceSheet, GenesisTransaction, SimpleTransactionModule, Transaction}
import scorex.utils.ScorexLogging
import scorex.waves.http.{DebugApiRoute, ScorexApiRoute, WavesApiRoute}
import scorex.waves.settings._
import scorex.waves.transaction.WavesTransactionModule

import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.Random

class Application(val settingsFilename: String) extends scorex.app.Application {

  override val applicationName = "waves"

  private val appConf = ConfigFactory.load().getConfig("app")

  override lazy val appVersion = {
    val raw = appConf.getString("version")
    val parts = raw.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }

  override implicit lazy val settings = new WavesSettings(settingsFilename)

  override implicit lazy val consensusModule = new NxtLikeConsensusModule(Constants.AvgBlockDelay)

  override implicit lazy val transactionModule: SimpleTransactionModule = new WavesTransactionModule()(settings, this)

  override lazy val blockStorage = transactionModule.blockStorage

  lazy val consensusApiRoute = new NxtConsensusApiRoute(this)

  override lazy val apiRoutes = Seq(
    BlocksApiRoute(this),
    TransactionsApiRoute(this),
    consensusApiRoute,
    WalletApiRoute(this),
    PaymentApiRoute(this),
    ScorexApiRoute(this),
    UtilsApiRoute(this),
    PeersApiRoute(this),
    AddressApiRoute(this),
    DebugApiRoute(this),
    WavesApiRoute(this)
  )

  override lazy val apiTypes = Seq(
    typeOf[BlocksApiRoute],
    typeOf[TransactionsApiRoute],
    typeOf[NxtConsensusApiRoute],
    typeOf[WalletApiRoute],
    typeOf[PaymentApiRoute],
    typeOf[ScorexApiRoute],
    typeOf[UtilsApiRoute],
    typeOf[PeersApiRoute],
    typeOf[AddressApiRoute],
    typeOf[DebugApiRoute],
    typeOf[WavesApiRoute]
  )

  override lazy val additionalMessageSpecs = TransactionalMessagesRepo.specs

  //checks
  require(transactionModule.balancesSupport)
  require(transactionModule.accountWatchingSupport)

  actorSystem.actorOf(Props(classOf[UnconfirmedPoolSynchronizer], this))

  // TODO: temporary overriding, remove after migration to scorex 1.2.8
  override def run() {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    checkGenesis()

    Http().bindAndHandle(combinedRoute, settings.rpcAddress, settings.rpcPort)

    historySynchronizer ! Unit
    historyReplier ! Unit
    actorSystem.actorOf(Props(classOf[PeerSynchronizer], this), "PeerSynchronizer")

    //on unexpected shutdown
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        log.error("Unexpected shutdown")
        stopAll()
      }
    })
  }

}

object Application extends App with ScorexLogging {

  log.debug("Start server with args: {} ", args)
  val filename = args.headOption.getOrElse("settings.json")

  val application = new Application(filename)

  log.debug("Waves has been started")
  application.run()

  if (application.wallet.privateKeyAccounts().isEmpty)
    application.wallet.generateNewAccounts(1)


  def testingScript(application: Application): Unit = {
    log.info("Going to execute testing scenario")
    log.info("Current state is:" + application.blockStorage.state)
    val wallet = application.wallet

    if (wallet.privateKeyAccounts().isEmpty) {
      wallet.generateNewAccounts(3)
      log.info("Generated Accounts:\n" + wallet.privateKeyAccounts().toList.map(_.address).mkString("\n"))
    }

    log.info("Executing testing scenario with accounts" +
      s"(${wallet.privateKeyAccounts().size}) : "
      + wallet.privateKeyAccounts().mkString(" "))

    require(wallet.privateKeyAccounts().nonEmpty)

    Thread.sleep(3.seconds.toMillis)

    val genesisBlock = application.blockStorage.history.genesis
    val genesisAccs = genesisBlock.transactions.flatMap(_ match {
      case gtx: GenesisTransaction =>
        Some(gtx.recipient)
      case _ =>
        log.error("Non-genesis tx in the genesis block!")
        None
    })

    def genPayment(recipient: Option[Account] = None, amtOpt: Option[Long] = None): Option[Transaction] = {
      val pkAccs = wallet.privateKeyAccounts().ensuring(_.nonEmpty)
      val senderAcc = pkAccs(Random.nextInt(pkAccs.size))
      val senderBalance = application.blockStorage.state.asInstanceOf[BalanceSheet].generationBalance(senderAcc)
      val recipientAcc = recipient.getOrElse(pkAccs(Random.nextInt(pkAccs.size)))
      val fee = Random.nextInt(5).toLong + 1
      if (senderBalance - fee > 0) {
        val amt = amtOpt.getOrElse(Math.abs(Random.nextLong() % (senderBalance - fee)))
        Some(application.transactionModule.createPayment(senderAcc, recipientAcc, amt, fee))
      } else None
    }

    log.info("Generate 200 transactions")
    (1 to 200) foreach (_ => genPayment())

    (1 to Int.MaxValue).foreach { _ =>
      Thread.sleep(Random.nextInt(5.seconds.toMillis.toInt))
      log.info(s"Payment created: ${genPayment()}")
    }
  }
}
