package scorex.waves

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import scorex.account.Account
import scorex.api.http._
import scorex.app.ApplicationVersion
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.network.{TransactionalMessagesRepo, UnconfirmedPoolSynchronizer}
import scorex.transaction.{BalanceSheet, GenesisTransaction, SimpleTransactionModule, Transaction}
import scorex.utils.ScorexLogging
import scorex.waves.block.WavesBlock
import scorex.waves.http.{DebugApiRoute, WavesApiRoute, ScorexApiRoute}
import scorex.waves.settings.WavesSettings
import scorex.waves.transaction.WavesTransactionModule

import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.Random

class Application(val settingsFilename: String) extends scorex.app.Application {

  override val applicationName = "waves"

  private val appConf = ConfigFactory.load("waves").getConfig("app")

  override lazy val appVersion = {
    val raw = appConf.getString("version")
    val parts = raw.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }

  override implicit lazy val settings = new WavesSettings(settingsFilename)

  override implicit lazy val consensusModule = new NxtLikeConsensusModule

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

  override def checkGenesis(): Unit = {

    if (transactionModule.blockStorage.history.isEmpty) {
      transactionModule.blockStorage.appendBlock(WavesBlock.genesis())
      log.info("Genesis block has been added to the state")
    }
  }

}

object Application extends App with ScorexLogging {

  log.debug("Start server with args: {} ", args)
  val filename = args.headOption.getOrElse("settings.json")

  val application = new Application(filename)

  log.debug("Waves has been started")
  application.run()

  if (application.wallet.privateKeyAccounts().isEmpty) application.wallet.generateNewAccounts(1)


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
