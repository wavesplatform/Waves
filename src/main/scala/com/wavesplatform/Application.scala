package com.wavesplatform

import akka.actor.{ActorSystem, Props}
import com.wavesplatform.http.NodeApiRoute
import scorex.account.AddressScheme
import scorex.api.http._
import scorex.app.ApplicationVersion
import scorex.network.{TransactionalMessagesRepo, UnconfirmedPoolSynchronizer}
import scorex.utils.ScorexLogging
import scorex.waves.http.{DebugApiRoute, WavesApiRoute}
import com.wavesplatform.settings._
import scorex.waves.transaction.WavesTransactionModule

import scala.reflect.runtime.universe._
import com.wavesplatform.actor.RootActorSystem
import scorex.api.http.assets.SignedAssetsApiRoute
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.settings.Settings
import com.wavesplatform.matcher.MatcherApplication
import scorex.consensus.nxt.WavesConsensusModule

class Application(as: ActorSystem, appSettings: WavesSettings) extends {
  override implicit val settings = appSettings
  override val applicationName = Constants.ApplicationName + appSettings.chainParams.addressScheme.chainId.toChar
  override val appVersion = {
    val parts = Constants.VersionString.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }
  override implicit val actorSystem = as
} with scorex.app.RunnableApplication
  with MatcherApplication {

  override implicit lazy val consensusModule = new WavesConsensusModule(settings.chainParams, Constants.AvgBlockDelay)

  override implicit lazy val transactionModule = new WavesTransactionModule(settings.chainParams)(settings, this)

  override lazy val blockStorage = transactionModule.blockStorage

  lazy val consensusApiRoute = new NxtConsensusApiRoute(this)

  override lazy val apiRoutes = Seq(
    BlocksApiRoute(this),
    TransactionsApiRoute(this),
    consensusApiRoute,
    WalletApiRoute(this),
    PaymentApiRoute(this),
    UtilsApiRoute(this),
    PeersApiRoute(this),
    AddressApiRoute(this),
    DebugApiRoute(this),
    WavesApiRoute(this),
    AssetsApiRoute(this),
    NodeApiRoute(this),
    SignedAssetsApiRoute(settings, transactionModule),
    LeaseApiRoute(this),
    BroadcastLeaseApiRoute(this)
  )

  override lazy val apiTypes = Seq(
    typeOf[BlocksApiRoute],
    typeOf[TransactionsApiRoute],
    typeOf[NxtConsensusApiRoute],
    typeOf[WalletApiRoute],
    typeOf[PaymentApiRoute],
    typeOf[UtilsApiRoute],
    typeOf[PeersApiRoute],
    typeOf[AddressApiRoute],
    typeOf[DebugApiRoute],
    typeOf[WavesApiRoute],
    typeOf[AssetsApiRoute],
    typeOf[NodeApiRoute],
    typeOf[SignedAssetsApiRoute],
    typeOf[LeaseApiRoute],
    typeOf[BroadcastLeaseApiRoute]
  )

  override lazy val additionalMessageSpecs = TransactionalMessagesRepo.specs

  //checks
  require(transactionModule.balancesSupport)
  require(transactionModule.accountWatchingSupport)

  actorSystem.actorOf(Props(classOf[UnconfirmedPoolSynchronizer], transactionModule, settings, networkController))

  override def run(): Unit = {
    super.run()

    if (settings.isRunMatcher) runMatcher()
  }
}

object Application extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    log.info("Starting with args: {} ", args)
    val filename = args.headOption.getOrElse("settings.json")
    val settings = new WavesSettings(Settings.readSettingsJson(filename))
    RootActorSystem.start("wavesplatform", settings) { actorSystem =>
      configureLogging(settings)

      // Initialize global var with actual address scheme
      AddressScheme.current = settings.chainParams.addressScheme

      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.chainParams.addressScheme.chainId}")

      val application = new Application(actorSystem, settings)
      application.run()

      if (application.wallet.privateKeyAccounts().isEmpty)
        application.wallet.generateNewAccounts(1)
    }
  }

  /**
    * Configure logback logging level according to settings
    */
  private def configureLogging(settings: WavesSettings) = {
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
        log.warn(s"Unknown loggingLevel = ${settings.loggingLevel }. Going to set INFO level")
        rootLogger.setLevel(Level.INFO)
    }
  }
}
