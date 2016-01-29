package scorex.app

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import scorex.api.http.{ApiRoute, CompositeHttpServiceActor}
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.crypto.encode.Base58
import scorex.network._
import scorex.network.message.{BasicMessagesRepo, MessageHandler, MessageSpec}
import scorex.network.peer.PeerManager
import scorex.settings.Settings
import scorex.transaction.{BlockStorage, History, TransactionModule}
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet
import spray.can.Http

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.Type
import scala.util.{Success, Random}

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
  lazy val apiActor = actorSystem.actorOf(Props(classOf[CompositeHttpServiceActor], apiTypes, apiRoutes), "api")

  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  lazy val basicMessagesSpecsRepo = new BasicMessagesRepo()

  //p2p
  lazy val nodeNonce: Long = (Random.nextInt() + 1000) * Random.nextInt() + Random.nextInt()

  lazy val upnp = new UPnP(settings)
  if (settings.upnpEnabled) upnp.addPort(settings.port)

  lazy val messagesHandler: MessageHandler = MessageHandler(basicMessagesSpecsRepo.specs ++ additionalMessageSpecs)

  lazy val peerManager = actorSystem.actorOf(Props(classOf[PeerManager], this))

  lazy val networkController = actorSystem.actorOf(Props(classOf[NetworkController], this), "networkController")
  lazy val blockGenerator = actorSystem.actorOf(Props(classOf[BlockGenerator], this), "blockGenerator")

  //wallet
  private lazy val walletFileOpt = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
  val wSeed = settings.walletSeed.getOrElse {
    println("Please type your wallet seed")
    def readSeed(): Array[Byte] = Base58.decode(scala.io.StdIn.readLine()) match {
      case Success(seed) => seed
      case _ =>
        println("Wallet seed should be correct Base58 encoded string.")
        readSeed()
    }
    readSeed()
  }
  implicit lazy val wallet = new Wallet(walletFileOpt, settings.walletPassword, wSeed)

  //interface to append log and state
  val blockStorage: BlockStorage

  lazy val history: History = blockStorage.history

  lazy val historySynchronizer = actorSystem.actorOf(Props(classOf[HistorySynchronizer], this), "HistorySynchronizer")
  lazy val historyReplier = actorSystem.actorOf(Props(classOf[HistoryReplier], this), "HistoryReplier")

  def run() {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    checkGenesis()

    IO(Http) ! Http.Bind(apiActor, interface = "0.0.0.0", port = settings.rpcPort)

    historySynchronizer ! Unit
    historyReplier ! Unit
    actorSystem.actorOf(Props(classOf[PeerSynchronizer], this), "PeerSynchronizer")

    //CLOSE ON UNEXPECTED SHUTDOWN
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
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
      //CLOSE WALLET
      log.info("Closing wallet")
      wallet.close()

      //FORCE CLOSE
      log.info("Exiting from the app...")
      System.exit(0)
    }
  }

  def checkGenesis(): Unit = {
    if (transactionModule.blockStorage.history.isEmpty) {
      transactionModule.blockStorage.appendBlock(Block.genesis())
      log.info("Genesis block has been added to the state")
    }
  }.ensuring(transactionModule.blockStorage.history.height() >= 1)
}
