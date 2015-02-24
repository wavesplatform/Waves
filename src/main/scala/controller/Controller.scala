package controller

import java.util.logging.Logger
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.io.IO
import api.HttpServiceActor
import database.{UnconfirmedTransactionsDatabaseImpl, PrunableBlockchainStorage}
import scorex._
import scorex.account.Account
import scorex.account.PrivateKeyAccount
import scorex.block.{GenesisBlock, Block}
import scorex.transaction.Transaction
import scorex.transaction.Transaction.TransactionType
import scorex.wallet.Wallet
import settings.Settings
import spray.can.Http
import network.{PeerManager, ConnectedPeer, Network, Peer}
import network.message._
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Controller {

  val STATUS_NO_CONNECTIONS = 0
  val STATUS_SYNCHRONIZING = 1
  val STATUS_OKE = 2

  //todo: avoid var
  private var status = STATUS_NO_CONNECTIONS

  def getStatus = status

  private val transactionCreator = new TransactionCreator()

  val peerHeights = TrieMap[ConnectedPeer, Int]()

  private var blockActorRef:ActorRef = _

  def init() {
    //OPENING DATABASES
    require(Network.isPortAvailable(Settings.rpcPort), "Rpc port " + Settings.rpcPort + " already in use!")

    if (PrunableBlockchainStorage.isEmpty()){
      GenesisBlock.process()
      PrunableBlockchainStorage.appendBlock(GenesisBlock)
    }

    require(PrunableBlockchainStorage.height() >= 1)

    implicit val actorSystem = ActorSystem("lagonaki")
    val httpServiceActor = actorSystem.actorOf(Props[HttpServiceActor], "http-service")
    val bindCommand = Http.Bind(httpServiceActor, interface = "0.0.0.0", port = Settings.rpcPort)
    IO(Http) ! bindCommand

    blockActorRef = actorSystem.actorOf(Props[BlockActor])

    val blockGenerator = actorSystem.actorOf(Props[BlockGenerator])

    //CLOSE ON UNEXPECTED SHUTDOWN
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        stopAll()
      }
    })

    update()
    actorSystem.scheduler.schedule(2.seconds, 500.milliseconds)(blockGenerator ! TryToGenerateBlock)
  }

  private var isStopping = false

  def stopAll() = {
    //PREVENT MULTIPLE CALLS
    if (!isStopping) {
      isStopping = true

      //STOP MESSAGE PROCESSOR
      Logger.getGlobal.info("Stopping message processor")
      Network.stop()

      //CLOSE WALLET
      Logger.getGlobal.info("Closing wallet")
      Wallet.close()

      //FORCE CLOSE
      System.exit(0)
    }
  }

  //NETWORK

  def activePeers() = Network.getActiveConnections()

  def onConnect(peer: ConnectedPeer) {
    val height = PrunableBlockchainStorage.height()

    //SEND VERSION MESSAGE
    peer.sendMessage(VersionMessage(height))

    if (this.status == STATUS_NO_CONNECTIONS) {
      //UPDATE STATUS
      this.status = STATUS_OKE
    }
  }

  def onDisconnect(peer: ConnectedPeer) {
    peerHeights.remove(peer)

    if (peerHeights.isEmpty) {
      //UPDATE STATUS
      status = STATUS_NO_CONNECTIONS
    }
  }

  def onError(peer: ConnectedPeer) = onDisconnect(peer)

  //SYNCHRONIZED DO NOT PROCESSS MESSAGES SIMULTANEOUSLY
  def onMessage(message: Message) = this.synchronized {

    message match {
      case PingMessage(Some(sender), id: Some[_]) =>
        sender.sendMessage(PingMessage(mbId = id))

      case GetPeersMessage(Some(sender), id: Some[_]) =>
        sender.sendMessage(PeersMessage(PeerManager.getKnownPeers, mbId = id))

      case VersionMessage(height, Some(sender), _) => peerHeights.put(sender, height)

      case GetSignaturesMessage(parent, Some(sender), id: Some[_]) =>
        val headers = PrunableBlockchainStorage.getSignatures(parent)
        sender.sendMessage(SignaturesMessage(headers, mbId = id))

      case GetBlockMessage(signature, Some(sender), id: Some[_]) =>
        val block = PrunableBlockchainStorage.blockByHeader(signature).get
        sender.sendMessage(BlockMessage(block.height().get, block, mbId = id))

      case BlockMessage(height, block, Some(sender), _) =>
        require(block != null)

        if (Block.isNewBlockValid(block)) {
          blockActorRef ! NewBlock(block, Some(sender))
        } else peerHeights.put(sender, height)

      case TransactionMessage(transaction, Some(sender), _) =>
        //CHECK IF SIGNATURE IS VALID OR GENESIS TRANSACTION
        if (!transaction.isSignatureValid || transaction.transactionType == TransactionType.GENESIS_TRANSACTION) {
          //DISHONEST PEER
          Network.onError(sender)
        } else if (transaction.hasMinimumFee && transaction.hasMinimumFeePerByte) {
          UnconfirmedTransactionsDatabaseImpl.put(transaction)

          //BROADCAST
          Network.broadcast(message, List(sender))
        }
    }
  }

  def broadcastBlock(newBlock: Block) {
    val message = BlockMessage(newBlock.height().get, newBlock)
    Network.broadcast(message, List[Peer]())
  }

  private def broadcastTransaction(transaction: Transaction) {
    val message = TransactionMessage(transaction)
    Network.broadcast(message, List[Peer]())
  }

  //SYNCHRONIZE

  def isUpToDate() = peerHeights.isEmpty || maxPeerHeight() <= PrunableBlockchainStorage.height()

  def update() {
    //UPDATE STATUS
    status = STATUS_SYNCHRONIZING

    //WHILE NOT UPTODATE
    while (!isUpToDate()) {
      val peer = maxHeightPeer()
      blockActorRef ! Synchronize(peer)
    }

    if (peerHeights.isEmpty) {
      //UPDATE STATUS
      this.status = STATUS_NO_CONNECTIONS
    } else {
      //UPDATE STATUS
      this.status = STATUS_OKE
    }
  }

  private def maxHeightPeer() = peerHeights.maxBy(_._2)._1

  private def maxPeerHeight() = peerHeights.maxBy(_._2)._2

  //WALLET

  def createWallet(seed: Array[Byte], password: String, amount: Int) = Wallet.create(seed, password, amount, false)

  def recoverWallet(seed: Array[Byte], password: String, amount: Int) = Wallet.create(seed, password, amount, true)

  //BLOCKCHAIN

  def scanTransactions(block: Block, blockLimit: Int, transactionLimit: Int, txType: Int, service: Int, account: Account) =
    PrunableBlockchainStorage.scanTransactions(block, blockLimit, transactionLimit, txType, service, account)

  def nextBlockGeneratingBalance() = BlockGenerator.getNextBlockGeneratingBalance(PrunableBlockchainStorage.lastBlock)

  def nextBlockGeneratingBalance(parent: Block) = BlockGenerator.getNextBlockGeneratingBalance(parent)

  //FORGE

  def newBlockGenerated(newBlock: Block) = {
    val valid = Block.isNewBlockValid(newBlock)
    if (valid) blockActorRef ! NewBlock(newBlock, None)
    valid
  }

  //TRANSACTIONS

  def onTransactionCreate(transaction: Transaction) {
    //ADD TO UNCONFIRMED TRANSACTIONS
    UnconfirmedTransactionsDatabaseImpl.put(transaction)

    //BROADCAST
    this.broadcastTransaction(transaction)
  }

  def sendPayment(sender: PrivateKeyAccount, recipient: Account, amount: BigDecimal, fee: BigDecimal) =
  //CREATE ONLY ONE TRANSACTION AT A TIME
    transactionCreator.synchronized {
      transactionCreator.createPayment(sender, recipient, amount, fee)
    }
}