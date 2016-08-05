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
import scorex.waves.settings._
import scorex.waves.transaction.WavesTransactionModule

import scala.reflect.runtime.universe._

class Application(val chainParams: ChainParameters, appSettings: WavesSettings) extends {
  override implicit val settings = appSettings
  override val applicationName = "waves"
  override val appVersion = {
    val parts = Constants.VersionString.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }

} with scorex.app.RunnableApplication {

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
  log.debug("Start server with args: {} ", args)

  private val filename = args.headOption.getOrElse("settings.json")
  private val settings = new WavesSettings(filename)
  private val chainParams: ChainParameters = if (settings.isTestNet) TestNetParams else MainNetParams

  // Initialize global var with actual address scheme
  AddressScheme.current = chainParams.addressScheme

  val application = new Application(chainParams, settings)
  application.run()

  log.debug("Waves has been started")

  if (application.wallet.privateKeyAccounts().isEmpty)
    application.wallet.generateNewAccounts(1)
}
