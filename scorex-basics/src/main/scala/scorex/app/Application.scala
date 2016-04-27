package scorex.app

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import scorex.api.http.{ApiRoute, CompositeHttpService}
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.consensus.mining.BlockGeneratorController
import scorex.network._
import scorex.network.message.{BasicMessagesRepo, MessageHandler, MessageSpec}
import scorex.network.peer.PeerManager
import scorex.settings.Settings
import scorex.transaction.{BlockStorage, History, TransactionModule}
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.Type

trait Application extends ScorexLogging {
  val ApplicationNameLimit = 50

  val applicationName: String

  val appVersion: ApplicationVersion

  //settings
  implicit val settings: Settings

  //modules
  implicit val consensusModule: ConsensusModule[_]
  implicit val transactionModule: TransactionModule[_]

  //api
  val apiRoutes: Seq[ApiRoute]
  val apiTypes: Seq[Type]

  protected implicit lazy val actorSystem = ActorSystem("lagonaki")

  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  lazy val basicMessagesSpecsRepo = new BasicMessagesRepo()

  //p2p
  lazy val upnp = new UPnP(settings)
  if (settings.upnpEnabled) upnp.addPort(settings.port)

  lazy val messagesHandler: MessageHandler = MessageHandler(basicMessagesSpecsRepo.specs ++ additionalMessageSpecs)

  lazy val peerManager = actorSystem.actorOf(Props(classOf[PeerManager], this))

  lazy val networkController = actorSystem.actorOf(Props(classOf[NetworkController], this), "networkController")
  lazy val blockGenerator = actorSystem.actorOf(Props(classOf[BlockGeneratorController], this), "blockGenerator")

  //wallet
  private lazy val walletFileOpt = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
  implicit lazy val wallet = new Wallet(walletFileOpt, settings.walletPassword, settings.walletSeed)

  //interface to append log and state
  val blockStorage: BlockStorage

  lazy val history: History = blockStorage.history

  lazy val historySynchronizer = actorSystem.actorOf(Props(classOf[HistorySynchronizer], this), "HistorySynchronizer")
  lazy val historyReplier = actorSystem.actorOf(Props(classOf[HistoryReplier], this), "HistoryReplier")


  implicit val materializer = ActorMaterializer()
  val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute


  def run() {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    checkGenesis()

    Http().bindAndHandle(combinedRoute, "0.0.0.0", settings.rpcPort)

    historySynchronizer ! Unit
    historyReplier ! Unit
    actorSystem.actorOf(Props(classOf[PeerSynchronizer], this), "PeerSynchronizer")

    //on unexpected shutdown
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        log.error("Unexpected shutdown")
        stopAll()
      }
    })
  }

  def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    if (settings.upnpEnabled) upnp.deletePort(settings.port)
    networkController ! NetworkController.ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>
      log.info("Closing wallet")
      wallet.close()

      log.info("Exiting from the app...")
      System.exit(0)
    }
  }

  def checkGenesis(): Unit = {
    if (transactionModule.blockStorage.history.isEmpty) {
      transactionModule.blockStorage.appendBlock(Block.genesis(settings.genesisTimestamp))
      log.info("Genesis block has been added to the state")
    }
  }.ensuring(transactionModule.blockStorage.history.height() >= 1)
}
