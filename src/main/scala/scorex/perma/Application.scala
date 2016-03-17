package scorex.perma

import java.io.File

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import scorex.api.http._
import scorex.app.ApplicationVersion
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.lagonaki.api.http.{DebugApiRoute, PaymentApiRoute, PeersHttpService, ScorexApiRoute}
import scorex.lagonaki.server.LagonakiSettings
import scorex.network.{TransactionalMessagesRepo, UnconfirmedPoolSynchronizer}
import scorex.perma.api.http.PermaConsensusApiRoute
import scorex.perma.consensus.PermaConsensusModule
import scorex.perma.network.{PermacoinMessagesRepo, SegmentsSynchronizer}
import scorex.perma.settings.PermaConstants._
import scorex.perma.storage.AuthDataStorage
import scorex.storage.Storage
import scorex.transaction.SimpleTransactionModule
import scorex.utils.ScorexLogging

import scala.reflect.runtime.universe._


object Application extends App with ScorexLogging {

  log.debug("Start server with args: {} ", args)
  val filename = args.headOption.getOrElse("settings.json")

  val application = new Application(filename)

  log.debug("PermaScorex has been started")
  application.run()

  if (application.wallet.privateKeyAccounts().isEmpty) application.wallet.generateNewAccounts(1)

}

class Application(val settingsFilename: String) extends scorex.app.Application {

  override val applicationName = "lagonaki"

  private val appConf = ConfigFactory.load().getConfig("app")

  override val appVersion = {
    val raw = appConf.getString("version")
    val parts = raw.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }

  override implicit lazy val settings = new LagonakiSettings(settingsFilename)

  override implicit lazy val consensusModule = {
    new File(settings.treeDir).mkdirs()
    val authDataStorage: Storage[Long, AuthDataBlock[DataSegment]] = new AuthDataStorage(settings.authDataStorage)
    val rootHash = settings.rootHash
    actorSystem.actorOf(Props(classOf[SegmentsSynchronizer], this, rootHash, authDataStorage))
    new PermaConsensusModule(rootHash, Some(networkController))(authDataStorage)
  }

  override implicit lazy val transactionModule: SimpleTransactionModule = new SimpleTransactionModule()(settings, this)

  override lazy val blockStorage = transactionModule.blockStorage

  lazy val consensusApiRoute = new PermaConsensusApiRoute(this)

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
    typeOf[PermaConsensusApiRoute],
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