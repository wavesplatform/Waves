package scorex

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import scorex.api.http.HttpServiceActor
import scorex.block.BlockchainSyncer$
import scorex.database.UnconfirmedTransactionsDatabaseImpl
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.network.NetworkController
import scorex.network.message._
import scorex.settings.{Constants, Settings}
import scorex.transaction.Transaction
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet
import spray.can.Http

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Controller extends ScorexLogging {
  private implicit lazy val actorSystem = ActorSystem("lagonaki")

  lazy val networkController = actorSystem.actorOf(Props[NetworkController])
  lazy val blockchainController = actorSystem.actorOf(Props(classOf[BlockchainSyncer], networkController))

  private lazy val walletFileOpt = Settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
  lazy val wallet = new Wallet(walletFileOpt, Settings.walletPassword, Settings.walletSeed.get)

  lazy val blockchainStorage = new PrunableBlockchainStorage(Settings.dataDirOpt)

  def init() {
    if (blockchainStorage.isEmpty) {
      val genesisBlock = Constants.ConsensusAlgo.genesisBlock
      genesisBlock.process()
      blockchainStorage.appendBlock(genesisBlock)
    }.ensuring(blockchainStorage.height() >= 1)

    val httpServiceActor = actorSystem.actorOf(Props[HttpServiceActor], "http-service")
    val bindCommand = Http.Bind(httpServiceActor, interface = "0.0.0.0", port = Settings.rpcPort)
    IO(Http) ! bindCommand

    blockchainController ! BlockchainSyncer.CheckState //just to init lazy val

    //CLOSE ON UNEXPECTED SHUTDOWN
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        stopAll()
      }
    })
  }

  def stopAll() = synchronized {
    log.info("Stopping message processor")
    networkController ! NetworkController.ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate() onComplete {
      case t =>
        //CLOSE WALLET
        log.info("Closing wallet")
        wallet.close()

        //TODO catch situations when we need this and remove
        Future {
          Thread.sleep(10000)
          log.error("Halt app!")
          Runtime.getRuntime.halt(0)
        }

        //FORCE CLOSE
        log.info("Exiting from the app...")
        System.exit(0)
    }
  }



  def onNewOffchainTransaction(transaction: Transaction) =
    if (UnconfirmedTransactionsDatabaseImpl.putIfNew(transaction)) {
      networkController ! NetworkController.BroadcastMessage(TransactionMessage(transaction))
    }
}