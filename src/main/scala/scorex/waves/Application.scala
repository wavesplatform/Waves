package scorex.waves

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import com.wavesplatform.consensus.WavesConsensusModule
import com.wavesplatform.{ChainParameters, MainNetParams, TestNetParams}
import scorex.account.AddressScheme
import scorex.api.http._
import scorex.app.ApplicationVersion
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.network.{TransactionalMessagesRepo, UnconfirmedPoolSynchronizer}
import scorex.utils.ScorexLogging
import scorex.waves.http.{DebugApiRoute, ScorexApiRoute, WavesApiRoute}
import scorex.waves.settings._
import scorex.waves.transaction.WavesTransactionModule

import scala.reflect.runtime.universe._

class Application(val settingsFilename: String) extends {
  override val applicationName = "waves"
  private val appConf = ConfigFactory.load().getConfig("app")
  override val appVersion = {
    val raw = appConf.getString("version")
    val parts = raw.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }
} with scorex.app.Application {

  override implicit lazy val settings = new WavesSettings(settingsFilename)

  implicit lazy val chainParams: ChainParameters = if (settings.isTestNet) TestNetParams else MainNetParams

  // Initialize global var with actual address scheme
  AddressScheme.current = chainParams.addressScheme

  override implicit lazy val consensusModule = new WavesConsensusModule()

  override implicit lazy val transactionModule = new WavesTransactionModule()(settings, this, chainParams)

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

  actorSystem.actorOf(Props(classOf[UnconfirmedPoolSynchronizer], transactionModule, settings, networkController))
}

object Application extends App with ScorexLogging {
  log.debug("Start server with args: {} ", args)

  val filename = args.headOption.getOrElse("settings.json")

  val application = new Application(filename)

  application.run()

  log.debug("Waves has been started")

  if (application.wallet.privateKeyAccounts().isEmpty)
    application.wallet.generateNewAccounts(1)
}
