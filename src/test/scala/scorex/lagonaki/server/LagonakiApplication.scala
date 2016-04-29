package scorex.lagonaki.server

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import scorex.api.http._
import scorex.app.{Application, ApplicationVersion}
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.lagonaki.mocks.ConsensusMock
import scorex.network._
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

  override implicit lazy val consensusModule = new ConsensusMock

  override implicit lazy val transactionModule: SimpleTransactionModule = new SimpleTransactionModule()(settings, this)

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
    AddressApiRoute(this)
  )

  override lazy val apiTypes = Seq(
    typeOf[BlocksApiRoute],
    typeOf[TransactionsApiRoute],
    typeOf[NxtConsensusApiRoute],
    typeOf[WalletApiRoute],
    typeOf[PaymentApiRoute],
    typeOf[UtilsApiRoute],
    typeOf[PeersApiRoute],
    typeOf[AddressApiRoute]
  )

  override lazy val additionalMessageSpecs = TransactionalMessagesRepo.specs

  //checks
  require(transactionModule.balancesSupport)
  require(transactionModule.accountWatchingSupport)

  actorSystem.actorOf(Props(classOf[UnconfirmedPoolSynchronizer], this))
}
