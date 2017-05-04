package scorex.app

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.wavesplatform.Shutdownable
import com.wavesplatform.settings.GenesisSettings
import scorex.account.Account
import scorex.api.http.{ApiRoute, CompositeHttpService}
import scorex.block.Block
import scorex.consensus.mining.BlockGeneratorController
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.network._
import scorex.network.message._
import scorex.network.peer.PeerManager
import scorex.transaction._
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.reflect.runtime.universe.Type
import scala.util.{Left, Try}


trait RunnableApplication extends Application with Shutdownable with ScorexLogging {

  protected val apiRoutes: Seq[ApiRoute]
  protected val apiTypes: Seq[Type]

  protected def actorSystem: ActorSystem

  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  // wallet, needs strict evaluation
  override val wallet: Wallet = {
    val maybeWalletFilename = Option(settings.walletSettings.file).filter(_.trim.nonEmpty)
    val seed = Base58.decode(settings.walletSettings.seed).toOption
    new Wallet(maybeWalletFilename, settings.walletSettings.password, seed)
  }

  //p2p
  lazy val upnp = new UPnP(settings.networkSettings.uPnPSettings)

  if (settings.networkSettings.uPnPSettings.enable) upnp.addPort(settings.networkSettings.port)

  lazy val messagesHandler: MessageHandler = MessageHandler(BasicMessagesRepo.specs ++ additionalMessageSpecs)

  lazy val history: History = blockStorage.history

  lazy override val networkController = actorSystem.actorOf(Props(new NetworkController(this)), "NetworkController")
  lazy override val peerManager = actorSystem.actorOf(
    PeerManager.props(settings.networkSettings, networkController, settings.blockchainSettings.addressSchemeCharacter), "PeerManager")

  lazy override val blockGenerator = actorSystem.actorOf(Props(classOf[BlockGeneratorController], this),
    "BlockGenerator")
  lazy override val scoreObserver = actorSystem.actorOf(Props(classOf[ScoreObserver], this), "ScoreObserver")
  lazy override val blockchainSynchronizer = actorSystem.actorOf(Props(classOf[BlockchainSynchronizer], this),
    "BlockchainSynchronizer")
  lazy override val coordinator = actorSystem.actorOf(Props(classOf[Coordinator], this), "Coordinator")
  private lazy val historyReplier = actorSystem.actorOf(Props(classOf[HistoryReplier], this), "HistoryReplier")

  @volatile private var shutdownInProgress = false

  @volatile var serverBinding: ServerBinding = _

  def run() {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    checkGenesis()

    implicit val as = actorSystem
    implicit val materializer = ActorMaterializer()

    if (settings.restAPISettings.enable) {
      val combinedRoute: Route = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings.restAPISettings).compositeRoute
      val httpFuture = Http().bindAndHandle(combinedRoute, settings.restAPISettings.bindAddress, settings.restAPISettings.port)
      serverBinding = Await.result(httpFuture, 10.seconds)
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
    }

    Seq(scoreObserver, blockGenerator, blockchainSynchronizer, historyReplier, coordinator) foreach {
      _ => // de-lazyning process :-)
    }

    actorSystem.actorOf(Props(classOf[PeerSynchronizer], this), "PeerSynchronizer")

    //on unexpected shutdown
    sys.addShutdownHook {
      shutdown()
    }
  }

  def shutdown(): Unit = {
    if (!shutdownInProgress) {
      log.info("Stopping network services")
      shutdownInProgress = true
      if (settings.restAPISettings.enable) {
        Try(Await.ready(serverBinding.unbind(), 60.seconds)).failed.map(e => log.error("Failed to unbind REST API port: " + e.getMessage))
      }
      if (settings.networkSettings.uPnPSettings.enable) upnp.deletePort(settings.networkSettings.port)

      implicit val askTimeout = Timeout(60.seconds)
      Try(Await.result(networkController ? NetworkController.ShutdownNetwork, 60.seconds))
        .failed.map(e => log.error("Failed to shutdown network: " + e.getMessage))
      Try(Await.result(actorSystem.terminate(), 60.seconds))
        .failed.map(e => log.error("Failed to terminate actor system: " + e.getMessage))
      log.debug("Closing wallet")
      wallet.close()
      log.info("Shutdown complete")
    }
  }

  private def checkGenesis(): Unit = {
    if (blockStorage.history.isEmpty) {
      val maybeGenesisSignature = Option(settings.blockchainSettings.genesisSettings.signature).filter(_.trim.nonEmpty)
      blockStorage.blockchainUpdater.processBlock(Block.genesis(
        NxtLikeConsensusBlockData(settings.blockchainSettings.genesisSettings.initialBaseTarget, Array.fill(DigestSize)(0: Byte)),
        RunnableApplication.genesisTransactions(settings.blockchainSettings.genesisSettings),
        settings.blockchainSettings.genesisSettings.blockTimestamp, maybeGenesisSignature)) match {
        case Left(value) =>
          log.error(value.toString)
          System.exit(1)
        case _ =>
      }

      log.info("Genesis block has been added to the state")
    }
  }
}

object RunnableApplication {
  def genesisTransactions(gs: GenesisSettings): Seq[GenesisTransaction] = {
    gs.transactions.map { ts =>
      val acc = Account.fromString(ts.recipient).right.get
      GenesisTransaction.create(acc, ts.amount, gs.transactionsTimestamp).right.get
    }
  }
}
