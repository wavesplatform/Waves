package controller

import java.math.BigDecimal
import java.util.Observable
import java.util.Observer
import java.util.logging.Logger
import api.ApiService
import scorex.BlockChain
import scorex.BlockGenerator
import scorex.Synchronizer
import scorex.TransactionCreator
import scorex.account.Account
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.crypto.Curve25519Impl
import scorex.transaction.Transaction
import scorex.wallet.Wallet
import settings.Settings
import utils.ObserverMessage
import database.DBSet
import network.{PeerManager, ConnectedPeer, Network, Peer}
import network.message._

import scala.collection.concurrent.TrieMap
import scala.collection.JavaConverters._
import scala.util.Try


object Controller extends Observable {

  val STATUS_NO_CONNECTIONS = 0
  val STATUS_SYNCHRONIZING = 1
  val STATUS_OKE = 2

  //todo: avoid var
  private var status = STATUS_NO_CONNECTIONS

  def getStatus = status

  private val blockChain = BlockChain

  private val rpcService = {
    if (!Network.isPortAvailable(Settings.getRpcPort))
      throw new Exception("Rpc port " + Settings.getRpcPort + " already in use!")

    new ApiService()
  }

  private val wallet = new Wallet()
  private val transactionCreator = new TransactionCreator()

  private val peerHeight = TrieMap[ConnectedPeer, Integer]()

  def getPeerHeights = peerHeight.asJava //todo: remove


  def init() {
    //OPENING DATABASES
    DBSet.getInstance()
    if (DBSet.getInstance().getBlockMap.isProcessing)
      throw new Exception("The application was not closed correctly!")

    this.rpcService.start()

    //START BLOCKGENERATOR
    BlockGenerator.start()

    //CLOSE ON UNEXPECTED SHUTDOWN
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        stopAll()
      }
    })

    //REGISTER DATABASE OBSERVER
    this.addObserver(DBSet.getInstance().getTransactionMap)
    this.addObserver(DBSet.getInstance())
  }

  override def addObserver(o: Observer) {
    //ADD OBSERVER TO SYNCHRONIZER
    //this.synchronizer.addObserver(o)
    DBSet.getInstance().getBlockMap.addObserver(o)

    //ADD OBSERVER TO BLOCKGENERATOR
    //this.blockGenerator.addObserver(o)
    DBSet.getInstance().getTransactionMap.addObserver(o)

    //ADD OBSERVER TO BALANCES
    DBSet.getInstance().getBalanceMap.addObserver(o)

    //ADD OBSERVER TO CONTROLLER
    super.addObserver(o)
    o.update(this, new ObserverMessage(ObserverMessage.NETWORK_STATUS, status))
  }

  override def deleteObserver(o: Observer) {
    DBSet.getInstance().getBlockMap.deleteObserver(o)
    super.deleteObserver(o)
  }

  def deleteWalletObserver(o: Observer) {
    wallet.deleteObserver(o)
  }

  private var isStopping = false

  def stopAll() = {
    //PREVENT MULTIPLE CALLS
    if (!isStopping) {
      isStopping = true

      //STOP MESSAGE PROCESSOR
      Logger.getGlobal.info("Stopping message processor")
      Network.stop()

      //STOP BLOCK PROCESSOR
      Logger.getGlobal.info("Stopping block processor")
      Synchronizer.stop()

      //CLOSE DATABABASE
      Logger.getGlobal.info("Closing database")
      DBSet.getInstance().close()

      //CLOSE WALLET
      Logger.getGlobal.info("Closing wallet")
      wallet.close()

      //FORCE CLOSE
      System.exit(0)
    }
  }

  //NETWORK

  def getActivePeers = Network.getActiveConnections()


  def onConnect(peer: ConnectedPeer) {
    val height = blockChain.getHeight

    //SEND VERSION MESSAGE
    peer.sendMessage(VersionMessage(height))

    if (this.status == STATUS_NO_CONNECTIONS) {
      //UPDATE STATUS
      this.status = STATUS_OKE

      //NOTIFY
      this.setChanged()
      this.notifyObservers(new ObserverMessage(ObserverMessage.NETWORK_STATUS, this.status))
    }
  }

  def onDisconnect(peer: ConnectedPeer) {
    peerHeight.remove(peer)

    if (peerHeight.isEmpty) {
      //UPDATE STATUS
      status = STATUS_NO_CONNECTIONS

      //NOTIFY
      setChanged()
      notifyObservers(new ObserverMessage(ObserverMessage.NETWORK_STATUS, status))
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

      case VersionMessage(height, Some(sender), _) => peerHeight.put(sender, height)

      case GetSignaturesMessage(parent, Some(sender), id: Some[_]) =>
        val headers = blockChain.getSignatures(parent)
        sender.sendMessage(SignaturesMessage(headers, mbId = id))

      case GetBlockMessage(signature, Some(sender), id: Some[_]) =>
        val block = blockChain.getBlock(signature)
        sender.sendMessage(BlockMessage(block.getHeight(), block, mbId = id))

      case BlockMessage(height, block, Some(sender), _) =>
        require(block != null)

        if (blockChain.isNewBlockValid(block)) {
          Logger.getGlobal.info("received new valid block")

          //PROCESS
          Synchronizer.process(block)

          //BROADCAST
          Network.broadcast(message, List(sender))

        } else peerHeight.put(sender, height)

      case TransactionMessage(transaction, Some(sender), _) =>
        //CHECK IF SIGNATURE IS VALID OR GENESIS TRANSACTION
        if (!transaction.isSignatureValid || transaction.getType == Transaction.GENESIS_TRANSACTION) {
          //DISHONEST PEER
          Network.onError(sender)
        } else if (transaction.hasMinimumFee && transaction.hasMinimumFeePerByte) {
          //ADD TO UNCONFIRMED TRANSACTIONS
          BlockGenerator.addUnconfirmedTransaction(transaction)

          setChanged()
          notifyObservers(new ObserverMessage(ObserverMessage.ADD_TRANSACTION_TYPE, transaction))

          //BROADCAST
          Network.broadcast(message, List(sender))
        }
    }
  }

  def addActivePeersObserver(o: Observer) = Network.addObserver(o)

  def removeActivePeersObserver(o: Observer) = Network.deleteObserver(o)

  def broadcastBlock(newBlock: Block) {
    val message = BlockMessage(newBlock.getHeight(), newBlock)
    Network.broadcast(message, List[Peer]())
  }

  private def broadcastTransaction(transaction: Transaction) {
    val message = TransactionMessage(transaction)
    Network.broadcast(message, List[Peer]())
  }

  //SYNCHRONIZE

  def isUpToDate() = peerHeight.isEmpty || getMaxPeerHeight() <= blockChain.getHeight

  def update() {
    //UPDATE STATUS
    this.status = STATUS_SYNCHRONIZING

    //NOTIFY
    this.setChanged()
    this.notifyObservers(new ObserverMessage(ObserverMessage.NETWORK_STATUS, this.status))

    //WHILE NOT UPTODATE
    while (!isUpToDate()) {
      //START UPDATE FROM HIGHEST HEIGHT PEER
      val peer = getMaxHeightPeer()

      //SYNCHRONIZE FROM PEER
      Try(Synchronizer.synchronize(peer)).recover { case e: Exception =>
        e.printStackTrace()
        Network.onError(peer)
      }
    }


    if (peerHeight.isEmpty) {
      //UPDATE STATUS
      this.status = STATUS_NO_CONNECTIONS

      //NOTIFY
      this.setChanged()
      this.notifyObservers(new ObserverMessage(ObserverMessage.NETWORK_STATUS, this.status))
    } else {
      //UPDATE STATUS
      this.status = STATUS_OKE

      //NOTIFY
      this.setChanged()
      this.notifyObservers(new ObserverMessage(ObserverMessage.NETWORK_STATUS, this.status))
    }
  }

  private def getMaxHeightPeer() = peerHeight.maxBy(_._2)._1

  private def getMaxPeerHeight() = peerHeight.maxBy(_._2)._2

  //WALLET

  def doesWalletExists() = wallet.exists()

  def createWallet(seed: Array[Byte], password: String, amount: Int) = wallet.create(seed, password, amount, false)

  def recoverWallet(seed: Array[Byte], password: String, amount: Int) = wallet.create(seed, password, amount, true)

  def getAccounts() = wallet.getAccounts()

  def getPrivateKeyAccounts() = wallet.getprivateKeyAccounts()

  def generateNewAccount() = wallet.generateNewAccount()

  def getPrivateKeyAccountByAddress(address: String) = wallet.getPrivateKeyAccount(address)

  def getAccountByAddress(address: String) = wallet.getAccount(address)

  def getUnconfirmedBalance(address: String) = wallet.getUnconfirmedBalance(address)

  def addWalletListener(o: Observer) = wallet.addObserver(o)

  def importAccountSeed(accountSeed: Array[Byte]) = wallet.importAccountSeed(accountSeed)

  def exportAccountSeed(address: String) = wallet.exportAccountSeed(address)

  def exportSeed() = wallet.exportSeed()


  def deleteAccount(account: PrivateKeyAccount) = wallet.deleteAccount(account)

  def synchronizeWallet() = wallet.synchronize()

  def isWalletUnlocked() = wallet.isUnlocked()

  def lockWallet = wallet.lock()

  def unlockWallet(password: String) = wallet.unlock(password)

  def getLastTransactions(limit: Int) = wallet.getLastTransactions(limit)

  def getTransaction(signature: Array[Byte]) = {
    //CHECK IF IN BLOCK
    Option(DBSet.getInstance().getTransactionParentMap.getParent(signature)) match {
      case Some(block) => block.getTransaction(signature).get
      case None => DBSet.getInstance().getTransactionMap().get(signature)
    }
  }

  def getLastTransactions(account: Account, limit: Int) = wallet.getLastTransactions(account, limit)

  def getLastBlocks() = wallet.getLastBlocks()

  def getLastBlocks(account: Account) = wallet.getLastBlocks(account)

  def onDatabaseCommit() = wallet.commit()

  //BLOCKCHAIN

  def getHeight() = blockChain.getHeight

  def getLastBlock() = blockChain.getLastBlock

  def getBlock(header: Array[Byte]) = blockChain.getBlock(header)


  def scanTransactions(block: Block, blockLimit: Int, transactionLimit: Int, txType: Int, service: Int, account: Account) = {
    val t = blockChain.scanTransactions(block, blockLimit, transactionLimit, txType, service, account)
    new utils.Pair(t._1, t._2.asJava) //todo:fix
  }


  def getNextBlockGeneratingBalance =
    BlockGenerator.getNextBlockGeneratingBalance(DBSet.getInstance(), DBSet.getInstance().getBlockMap.getLastBlock)


  def getNextBlockGeneratingBalance(parent: Block) =
    BlockGenerator.getNextBlockGeneratingBalance(DBSet.getInstance(), parent)


  //FORGE

  def newBlockGenerated(newBlock: Block) =
    if (Synchronizer.process(newBlock)) {
      //add to the blockchain
      Logger.getGlobal.info(s"Block generated $newBlock (height: ${newBlock.getHeight()}) with ${newBlock.transactions.size} transactions inside, going to broadcast it")
      broadcastBlock(newBlock)
      true
    } else false


  def getUnconfirmedTransactions = BlockGenerator.getUnconfirmedTransactions

  //BALANCES

  def getBalances(key: Long) = DBSet.getInstance().getBalanceMap().getBalancesSortableList(key)


  def getBalances(account: Account) = DBSet.getInstance().getBalanceMap().getBalancesSortableList(account)

  //TRANSACTIONS

  def onTransactionCreate(transaction: Transaction) {
    //ADD TO UNCONFIRMED TRANSACTIONS
    BlockGenerator.addUnconfirmedTransaction(transaction)

    //NOTIFY OBSERVERS
    this.setChanged()
    this.notifyObservers(new ObserverMessage(ObserverMessage.LIST_TRANSACTION_TYPE, DBSet.getInstance().getTransactionMap.getValues))

    this.setChanged()
    this.notifyObservers(new ObserverMessage(ObserverMessage.ADD_TRANSACTION_TYPE, transaction))

    //BROADCAST
    this.broadcastTransaction(transaction)
  }

  def sendPayment(sender: PrivateKeyAccount, recipient: Account, amount: BigDecimal, fee: BigDecimal) =
  //CREATE ONLY ONE TRANSACTION AT A TIME
    transactionCreator.synchronized {
      transactionCreator.createPayment(sender, recipient, amount, fee)
    }
}