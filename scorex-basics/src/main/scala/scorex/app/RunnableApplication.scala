package scorex.app

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import scorex.api.http.{ApiRoute, CompositeHttpService}
import scorex.block.Block
import scorex.consensus.mining.BlockGeneratorController
import scorex.network._
import scorex.network.message.{BasicMessagesRepo, MessageHandler, MessageSpec}
import scorex.network.peer.PeerManager
import scorex.settings.Settings
import scorex.transaction.{BlockStorage, History}
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.reflect.runtime.universe.Type
import scala.util.Try
import akka.util.Timeout

trait RunnableApplication extends Application with ScorexLogging {

  //settings
  implicit override val settings: Settings

  //api
  protected val apiRoutes: Seq[ApiRoute]
  protected val apiTypes: Seq[Type]

  protected implicit val actorSystem: ActorSystem

  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  lazy override val basicMessagesSpecsRepo: BasicMessagesRepo = new BasicMessagesRepo()

  // wallet, needs strict evaluation
  override val wallet = {
    val walletFileOpt = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
    new Wallet(walletFileOpt, settings.walletPassword, settings.walletSeed)
  }

  //p2p
  lazy val upnp = new UPnP(settings)
  if (settings.upnpEnabled) upnp.addPort(settings.port)

  lazy val messagesHandler: MessageHandler = MessageHandler(basicMessagesSpecsRepo.specs ++ additionalMessageSpecs)

  lazy override val peerManager = actorSystem.actorOf(Props(classOf[PeerManager], this))

  //interface to append log and state
  lazy override val blockStorage: BlockStorage = transactionModule.blockStorage

  lazy override val history: History = blockStorage.history

  lazy override val networkController = actorSystem.actorOf(Props(classOf[NetworkController], this),
    "NetworkController")
  lazy override val blockGenerator = actorSystem.actorOf(Props(classOf[BlockGeneratorController], this),
    "BlockGenerator")
  lazy override val scoreObserver = actorSystem.actorOf(Props(classOf[ScoreObserver], this), "ScoreObserver")
  lazy override val blockchainSynchronizer = actorSystem.actorOf(Props(classOf[BlockchainSynchronizer], this),
    "BlockchainSynchronizer")
  lazy override val coordinator = actorSystem.actorOf(Props(classOf[Coordinator], this), "Coordinator")
  private lazy val historyReplier = actorSystem.actorOf(Props(classOf[HistoryReplier], this), "HistoryReplier")

  def run() {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors }")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory }")

    checkGenesis()

    implicit val materializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute
    Http().bindAndHandle(combinedRoute, settings.rpcAddress, settings.rpcPort)

    Seq(scoreObserver, blockGenerator, blockchainSynchronizer, historyReplier, coordinator) foreach {
      _ => // de-lazyning process :-)
    }

    actorSystem.actorOf(Props(classOf[PeerSynchronizer], this), "PeerSynchronizer")

    //on unexpected shutdown
    sys.addShutdownHook {
      shutdown()
    }
    actorSystem.registerOnTermination {
      stopWallet()
    }
  }

  private def stopWallet() = synchronized {
    log.info("Closing wallet")
    wallet.close()
  }

  def shutdown(): Future[Unit] = synchronized {
    Try { stopNetwork() }.failed.foreach(e => log.warn("Stop network error", e))
    actorSystem.terminate().map(_ => ())
  }

  private def stopNetwork(): Unit = synchronized {
    log.info("Stopping network services")
    if (settings.upnpEnabled) upnp.deletePort(settings.port)
    implicit val askTimeout = Timeout(10 seconds)
    Await.result(networkController ? NetworkController.ShutdownNetwork, 10 seconds)
  }

  private def checkGenesis(): Unit = {
    if (transactionModule.blockStorage.history.isEmpty) {
      transactionModule.blockStorage.appendBlock(Block.genesis(settings.genesisTimestamp, settings.genesisSignature))
      log.info("Genesis block has been added to the state")
    }
  }.ensuring(transactionModule.blockStorage.history.height() >= 1)
}
