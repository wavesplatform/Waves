package com.wavesplatform

import java.io.File

import akka.actor.{ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.http.NodeApiRoute
import com.wavesplatform.matcher.{MatcherApplication, MatcherSettings}
import com.wavesplatform.settings.BlockchainSettingsExtension._
import com.wavesplatform.settings._
import scorex.account.AddressScheme
import scorex.api.http._
import scorex.api.http.alias.{AliasApiRoute, AliasBroadcastApiRoute}
import scorex.api.http.assets.{AssetsApiRoute, AssetsBroadcastApiRoute}
import scorex.api.http.leasing.{LeaseApiRoute, LeaseBroadcastApiRoute}
import scorex.app.ApplicationVersion
import scorex.consensus.nxt.WavesConsensusModule
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.network.{TransactionalMessagesRepo, UnconfirmedPoolSynchronizer}
import scorex.transaction.SimpleTransactionModule
import scorex.utils.ScorexLogging
import scorex.waves.http.{DebugApiRoute, WavesApiRoute}

import scala.reflect.runtime.universe._

class Application(as: ActorSystem, wavesSettings: WavesSettings) extends {
  val matcherSettings: MatcherSettings = wavesSettings.matcherSettings
  val restAPISettings: RestAPISettings = wavesSettings.restAPISettings
  override implicit val settings = wavesSettings

  override val applicationName = Constants.ApplicationName +
    wavesSettings.blockchainSettings.addressSchemeCharacter
  override val appVersion = {
    val parts = Constants.VersionString.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }
  override implicit val actorSystem = as
} with scorex.app.RunnableApplication
  with MatcherApplication {

  override implicit lazy val consensusModule = new WavesConsensusModule(settings.blockchainSettings.asChainParameters, Constants.AvgBlockDelay)

  override implicit lazy val transactionModule = new SimpleTransactionModule(settings.blockchainSettings.asChainParameters)(settings, this)

  override lazy val blockStorage = transactionModule.blockStorage

  lazy val consensusApiRoute = new NxtConsensusApiRoute(this)

  override lazy val apiRoutes = Seq(
    BlocksApiRoute(settings.restAPISettings, settings.checkpointsSettings, history, coordinator),
    TransactionsApiRoute(settings.restAPISettings, blockStorage.state, history, transactionModule),
    consensusApiRoute,
    WalletApiRoute(settings.restAPISettings, wallet),
    PaymentApiRoute(settings.restAPISettings, wallet, transactionModule),
    UtilsApiRoute(settings.restAPISettings),
    PeersApiRoute(settings.restAPISettings, peerManager, networkController),
    AddressApiRoute(settings.restAPISettings, wallet, blockStorage.state),
    DebugApiRoute(settings.restAPISettings, wallet, blockStorage),
    WavesApiRoute(settings.restAPISettings, wallet, transactionModule),
    AssetsApiRoute(settings.restAPISettings, wallet, blockStorage.state, transactionModule),
    NodeApiRoute(this),
    AssetsBroadcastApiRoute(settings.restAPISettings, transactionModule),
    LeaseApiRoute(settings.restAPISettings, wallet, blockStorage.state, transactionModule),
    LeaseBroadcastApiRoute(settings.restAPISettings, transactionModule),
    AliasApiRoute(settings.restAPISettings, wallet, transactionModule, blockStorage.state),
    AliasBroadcastApiRoute(settings.restAPISettings, transactionModule)
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
    typeOf[AssetsBroadcastApiRoute],
    typeOf[LeaseApiRoute],
    typeOf[LeaseBroadcastApiRoute],
    typeOf[AliasApiRoute],
    typeOf[AliasBroadcastApiRoute]
  )

  override lazy val additionalMessageSpecs = TransactionalMessagesRepo.specs

  actorSystem.actorOf(Props(classOf[UnconfirmedPoolSynchronizer], transactionModule, settings.utxSettings, networkController))

  override def run(): Unit = {
    super.run()

    if (matcherSettings.enable) runMatcher()
  }
}

object Application extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    log.info("Starting...")

    val maybeConfigFile = for {
      maybeFilename <- args.headOption
      file = new File(maybeFilename)
      if file.exists
    } yield file

    if (maybeConfigFile.isEmpty) log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")

    val maybeUserConfig = maybeConfigFile collect {
      case file if file.getName.endsWith(".json") =>
        log.warn("JSON configuration file is deprecated and will be removed in the future version. " +
          s"The node will try to read settings from ${file.getAbsolutePath} for now.")
        LegacyConfigTransformer.transform(ConfigFactory.parseFile(file))
      case file => ConfigFactory.parseFile(file)
    }

    val config = maybeUserConfig.foldLeft(ConfigFactory.defaultApplication().withFallback(ConfigFactory.load())) {
      (default, user) => user.withFallback(default)
    }

    val settings = WavesSettings.fromConfig(config.resolve)

    RootActorSystem.start("wavesplatform", settings.matcherSettings) { actorSystem =>
      configureLogging(settings)

      // Initialize global var with actual address scheme
      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
      }

      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.blockchainSettings.addressSchemeCharacter}")

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
    import ch.qos.logback.classic.{Level, LoggerContext}
    import org.slf4j._

    val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val rootLogger = lc.getLogger(Logger.ROOT_LOGGER_NAME)
    settings.loggingLevel match {
      case LogLevel.DEBUG => rootLogger.setLevel(Level.DEBUG)
      case LogLevel.INFO => rootLogger.setLevel(Level.INFO)
      case LogLevel.WARN => rootLogger.setLevel(Level.WARN)
      case LogLevel.ERROR => rootLogger.setLevel(Level.ERROR)
    }
  }
}
