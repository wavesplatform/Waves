package controller

import java.util.logging.Logger

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import api.http.HttpServiceActor
import scorex.account.{Account, PrivateKeyAccount}
import scorex.block.BlockchainController
import scorex.consensus.qora.QoraBlockGenerationFunctions
import scorex.database.UnconfirmedTransactionsDatabaseImpl
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.network.NetworkController
import scorex.network.message._
import scorex.transaction.{Transaction, TransactionCreator}
import scorex.wallet.Wallet
import settings.{Constants, Settings}
import spray.can.Http


object Controller {
  private implicit lazy val actorSystem = ActorSystem("lagonaki")

  lazy val networkController = actorSystem.actorOf(Props[NetworkController])
  lazy val blockchainController = actorSystem.actorOf(Props(classOf[BlockchainController], networkController))

  def init() {
    if (PrunableBlockchainStorage.isEmpty()) {
      val genesisBlock = Constants.ConsensusAlgo.genesisBlock
      genesisBlock.process()
      PrunableBlockchainStorage.appendBlock(genesisBlock)
    }.ensuring(PrunableBlockchainStorage.height() >= 1)

    val httpServiceActor = actorSystem.actorOf(Props[HttpServiceActor], "http-service")
    val bindCommand = Http.Bind(httpServiceActor, interface = "0.0.0.0", port = Settings.rpcPort)
    IO(Http) ! bindCommand

    blockchainController ! BlockchainController.CheckState //just to init lazy val

    //CLOSE ON UNEXPECTED SHUTDOWN
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        stopAll()
      }
    })
  }

  def stopAll() = this.synchronized {
    Logger.getGlobal.info("Stopping message processor")
    networkController ! NetworkController.ShutdownNetwork

    Logger.getGlobal.info("Stopping actors (incl. block generator)")
    actorSystem.shutdown()

    //CLOSE WALLET
    Logger.getGlobal.info("Closing wallet")
    Wallet.close()

    //FORCE CLOSE
    System.exit(10)
  }

  //FORGE
  def onTransactionCreate(transaction: Transaction) {
    UnconfirmedTransactionsDatabaseImpl.put(transaction)
    networkController ! NetworkController.BroadcastMessage(TransactionMessage(transaction))
  }

  //todo: proxy methods below are not needed probably

  def sendPayment(sender: PrivateKeyAccount, recipient: Account, amount: BigDecimal, fee: BigDecimal) =
    TransactionCreator.synchronized {
      TransactionCreator.createPayment(sender, recipient, amount, fee)
    }
}