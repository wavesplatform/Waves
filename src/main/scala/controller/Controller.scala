package controller

import java.util.logging.Logger

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import api.HttpServiceActor
import scorex.account.{Account, PrivateKeyAccount}
import scorex.block.{Block, BlockGenerator, BlockchainController, GenesisBlock}
import scorex.database.{PrunableBlockchainStorage, UnconfirmedTransactionsDatabaseImpl}
import scorex.network.NetworkController
import scorex.network.message._
import scorex.transaction.{Transaction, TransactionCreator}
import scorex.wallet.Wallet
import settings.Settings
import spray.can.Http


object Controller {
  private implicit lazy val actorSystem = ActorSystem("lagonaki")

  lazy val networkController = actorSystem.actorOf(Props[NetworkController])
  lazy val blockchainController = actorSystem.actorOf(Props(classOf[BlockchainController], networkController))

  def init() {
    if (PrunableBlockchainStorage.isEmpty()) {
      GenesisBlock.process()
      PrunableBlockchainStorage.appendBlock(GenesisBlock)
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

    Logger.getGlobal.info("Stopping block generator")
    actorSystem.shutdown()

    //CLOSE WALLET
    Logger.getGlobal.info("Closing wallet")
    Wallet.close()

    //FORCE CLOSE
    System.exit(0)
  }

  //FORGE
  def onTransactionCreate(transaction: Transaction) {
    //ADD TO UNCONFIRMED TRANSACTIONS
    UnconfirmedTransactionsDatabaseImpl.put(transaction)

    //BROADCAST
    networkController ! NetworkController.BroadcastMessage(TransactionMessage(transaction))
  }

  //todo: proxy methods below are not needed probably

  def sendPayment(sender: PrivateKeyAccount, recipient: Account, amount: BigDecimal, fee: BigDecimal) =
    TransactionCreator.synchronized {
      TransactionCreator.createPayment(sender, recipient, amount, fee)
    }

  def nextBlockGeneratingBalance() = BlockGenerator.getNextBlockGeneratingBalance(PrunableBlockchainStorage.lastBlock)

  def nextBlockGeneratingBalance(parent: Block) = BlockGenerator.getNextBlockGeneratingBalance(parent)

  //NETWORK

  /*
  def update() {
    //UPDATE STATUS
    status = STATUS_SYNCHRONIZING

    //WHILE NOT UPTODATE
    while (!isUpToDate()) {
      val peer = maxHeightPeer()
      blockchainController ! Synchronize(peer)
    }

    if (peerHeights.isEmpty) {
      //UPDATE STATUS
      this.status = STATUS_NO_CONNECTIONS
    } else {
      //UPDATE STATUS
      this.status = STATUS_OKE
    }
  }

  def isUpToDate() = peerHeights.isEmpty || maxPeerHeight() <= PrunableBlockchainStorage.height()


  def onConnect(peer: ConnectedPeer) {
    if (this.status == STATUS_NO_CONNECTIONS) {
      //UPDATE STATUS
      this.status = STATUS_OKE
    }
  }

  def onDisconnect(peer: ConnectedPeer) {
    if (peerHeights.isEmpty) {
      //UPDATE STATUS
      status = STATUS_NO_CONNECTIONS
    }
  }

  //todo: not used
  def broadcastBlock(newBlock: Block) {
    val message = BlockMessage(newBlock.height().get, newBlock)
    Network.broadcast(message, List[Peer]())
  }


  //BLOCKCHAIN


  def newBlockGenerated(newBlock: Block) = {
    val valid = Block.isNewBlockValid(newBlock)
    if (valid) blockchainController ! NewBlock(newBlock, None)
    valid
  }

  */
}