package com.wavesplatform

import akka.actor.Props

import com.wavesplatform.consensus.WavesConsensusModule
import com.wavesplatform.http.NodeApiRoute
import scorex.account.AddressScheme
import scorex.api.http._
import scorex.app.ApplicationVersion
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.network.{TransactionalMessagesRepo, UnconfirmedPoolSynchronizer}
import scorex.utils.ScorexLogging
import scorex.waves.http.{DebugApiRoute, ScorexApiRoute, WavesApiRoute}
import com.wavesplatform.settings._
import scorex.waves.transaction.WavesTransactionModule

import scala.reflect.runtime.universe._

class Application(appSettings: WavesSettings) extends {
  override implicit val settings = appSettings
  override val applicationName = "waves"
  override val appVersion = {
    val parts = Constants.VersionString.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }

} with scorex.app.RunnableApplication {

  override implicit lazy val consensusModule = new WavesConsensusModule()

  override implicit lazy val transactionModule = new WavesTransactionModule()(settings, this, settings.chainParams)

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
    WavesApiRoute(this),
    NodeApiRoute(this)
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
    typeOf[WavesApiRoute],
    typeOf[NodeApiRoute]
  )

  override lazy val additionalMessageSpecs = TransactionalMessagesRepo.specs

  //checks
  require(transactionModule.balancesSupport)
  require(transactionModule.accountWatchingSupport)

  actorSystem.actorOf(Props(classOf[UnconfirmedPoolSynchronizer], transactionModule, settings, networkController))
}

object Application extends App with ScorexLogging {
  //TODO: gagarin55-change to info cuz default log level is info
  log.debug("Starting with args: {} ", args)

  private val filename = args.headOption.getOrElse("settings.json")
  private val settings = new WavesSettings(filename)

  configureLogging(settings)

  // Initialize global var with actual address scheme
  AddressScheme.current = settings.chainParams.addressScheme

  log.info(s"${Constants.AgentName} Blockchain Id: ${settings.chainParams.addressScheme.chainId}")

  val application = new Application(settings)
  application.run()

  if (application.wallet.privateKeyAccounts().isEmpty)
    application.wallet.generateNewAccounts(1)

  /**
    * Configure logback logging level according to settings
    */
  def configureLogging(settings: WavesSettings) = {
    import ch.qos.logback.classic.LoggerContext
    import org.slf4j._
    import ch.qos.logback.classic.Level

    val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val rootLogger = lc.getLogger(Logger.ROOT_LOGGER_NAME)
    settings.loggingLevel match {
      case "info" => rootLogger.setLevel(Level.INFO)
      case "debug" => rootLogger.setLevel(Level.DEBUG)
      case "error" => rootLogger.setLevel(Level.ERROR)
      case "warn" => rootLogger.setLevel(Level.WARN)
      case "trace" => rootLogger.setLevel(Level.TRACE)
      case _ =>
        log.warn(s"Unknown loggingLevel = ${settings.loggingLevel}. Going to set INFO level")
        rootLogger.setLevel(Level.INFO)
    }
  }
}
