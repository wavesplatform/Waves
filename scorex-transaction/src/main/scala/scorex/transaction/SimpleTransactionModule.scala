package scorex.transaction

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Ints}
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.app.Application
import scorex.block.{Block, BlockField}
import scorex.crypto.encode.Base58
import scorex.consensus.TransactionsOrdering
import scorex.network.message.Message
import scorex.network.{Broadcast, NetworkController, TransactionalMessagesRepo}
import scorex.settings.{Settings, WavesHardForkParameters}
import scorex.transaction.SimpleTransactionModule.StoredInBlock
import scorex.transaction.ValidationResult.ValidationResult
import scorex.transaction.assets.{DeleteTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.assets.exchange.{Order, OrderMatch}
import scorex.transaction.state.database.{BlockStorageImpl, UnconfirmedTransactionsDatabaseImpl}
import scorex.transaction.state.wallet._
import scorex.utils._
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

import scorex.transaction.assets.exchange.{Order, OrderMatch}

@SerialVersionUID(3044437555808662124L)
case class TransactionsBlockField(override val value: Seq[Transaction])
  extends BlockField[Seq[Transaction]] {

  import SimpleTransactionModule.MaxTransactionsPerBlock

  override val name = "transactions"

  override lazy val json: JsObject = Json.obj(name -> JsArray(value.map(_.json)))

  override lazy val bytes: Array[Byte] = {
    val txCount = value.size.ensuring(_ <= MaxTransactionsPerBlock).toByte
    value.foldLeft(Array(txCount)) { case (bs, tx) =>
      val txBytes = tx.bytes
      bs ++ Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0) ++ txBytes
    }
  }
}


class SimpleTransactionModule(hardForkParams: WavesHardForkParameters)(implicit val settings: TransactionSettings with Settings,
                                                                       application: Application)
  extends TransactionModule[StoredInBlock] with ScorexLogging {

  import SimpleTransactionModule._

  val networkController = application.networkController
  private val feeCalculator = new FeeCalculator(settings)

  val TransactionSizeLength = 4
  val InitialBalance = 10000000000000L

  override val utxStorage: UnconfirmedTransactionsStorage = new UnconfirmedTransactionsDatabaseImpl

  override val blockStorage = new BlockStorageImpl(settings, hardForkParams)(application.consensusModule, this)

  /**
    * In Lagonaki, transaction-related data is just sequence of transactions. No Merkle-tree root of txs / state etc
    *
    * @param bytes - serialized sequence of transaction
    * @return
    */
  override def parseBytes(bytes: Array[Byte]): Try[TransactionsBlockField] = Try {
    bytes.isEmpty match {
      case true => TransactionsBlockField(Seq())
      case false =>
        val txData = bytes.tail
        val txCount = bytes.head // so 255 txs max
        formBlockData((1 to txCount).foldLeft((0: Int, Seq[TypedTransaction]())) { case ((pos, txs), _) =>
          val transactionLengthBytes = txData.slice(pos, pos + TransactionSizeLength)
          val transactionLength = Ints.fromByteArray(transactionLengthBytes)
          val transactionBytes = txData.slice(pos + TransactionSizeLength, pos + TransactionSizeLength + transactionLength)
          val transaction = TypedTransaction.parseBytes(transactionBytes).get

          (pos + TransactionSizeLength + transactionLength, txs :+ transaction)
        }._2)
    }
  }

  override def formBlockData(transactions: StoredInBlock): TransactionsBlockField = TransactionsBlockField(transactions)

  //TODO asInstanceOf
  override def transactions(block: Block): StoredInBlock =
    block.transactionDataField.asInstanceOf[TransactionsBlockField].value

  override def unconfirmedTxs: Seq[Transaction] = utxStorage.all()

  override def putUnconfirmedIfNew(tx: Transaction): Boolean = synchronized {
    if (feeCalculator.enoughFee(tx)) {
      utxStorage.putIfNew(tx, isValid(_, tx.timestamp))
    } else false
  }

  override def packUnconfirmed(heightOpt: Option[Int]): StoredInBlock = synchronized {
    clearIncorrectTransactions()

    val txs = utxStorage.all().sorted(TransactionsOrdering).take(MaxTransactionsPerBlock)
    val valid = blockStorage.state.validate(txs, heightOpt, NTP.correctedTime())

    if (valid.size != txs.size) {
      log.debug(s"Txs for new block do not match: valid=${valid.size} vs all=${txs.size}")
    }

    valid
  }

  override def clearFromUnconfirmed(data: StoredInBlock): Unit = synchronized {
    data.foreach(tx => utxStorage.getBySignature(tx.id) match {
      case Some(unconfirmedTx) => utxStorage.remove(unconfirmedTx)
      case None =>
    })

    clearIncorrectTransactions() // todo makes sence to remove expired only at this point
  }

  /**
    * Removes too old or invalid transactions from UnconfirmedTransactionsPool
    */
  def clearIncorrectTransactions(): Unit = {
    val currentTime = NTP.correctedTime()
    val txs = utxStorage.all()
    val notExpired = txs.filter { tx => (currentTime - tx.timestamp).millis <= MaxTimeForUnconfirmed }
    val notFromFuture = notExpired.filter { tx => (tx.timestamp - currentTime).millis <= MaxTimeDrift }
    val valid = blockStorage.state.validate(notFromFuture, blockTime = currentTime)
    // remove non valid or expired from storage
    txs.diff(valid).foreach(utxStorage.remove)
  }

  override def onNewOffchainTransaction(transaction: Transaction): Unit =
    if (putUnconfirmedIfNew(transaction)) {
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(transaction), None)
      networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
    }

  @deprecated("Use transferAsset()")
  def createPayment(payment: Payment, wallet: Wallet): Option[PaymentTransaction] = {
    wallet.privateKeyAccount(payment.sender).map { sender =>
      createPayment(sender, new Account(payment.recipient), payment.amount, payment.fee)
    }
  }

  def transferAsset(request: TransferRequest, wallet: Wallet): Try[TransferTransaction] = {
    for {
      transfer <- transferRequestToTransaction(request, wallet)
    } yield {
      if (isValid(transfer, transfer.timestamp)) {
        onNewOffchainTransaction(transfer)
      } else {
        throw new StateCheckFailed("Invalid transfer transaction generated: " + transfer.json)
      }
      transfer
    }
  }

  private def transferRequestToTransaction(request: TransferRequest, wallet: Wallet): Try[TransferTransaction] = Try {
    val sender = wallet.privateKeyAccount(request.sender).get

    val transfer: TransferTransaction = TransferTransaction.create(request.assetId.map(s => Base58.decode(s).get),
      sender: PrivateKeyAccount,
      new Account(request.recipient),
      request.amount,
      getTimestamp,
      request.feeAssetId.map(s => Base58.decode(s).get),
      request.fee,
      Option(request.attachment).filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray))

    transfer
  }

  def issueAsset(request: IssueRequest, wallet: Wallet): Try[IssueTransaction] = Try {
    val sender = wallet.privateKeyAccount(request.sender).get
    val issue = IssueTransaction.create(sender,
      request.name.getBytes(Charsets.UTF_8),
      request.description.getBytes(Charsets.UTF_8),
      request.quantity,
      request.decimals,
      request.reissuable,
      request.fee,
      getTimestamp)
    if (isValid(issue, issue.timestamp)) onNewOffchainTransaction(issue)
    else throw new StateCheckFailed("Invalid issue transaction generated: " + issue.json)
    issue
  }

  /**
    * Validate transaction according to the State and send it to network
    */
  def broadcastTransaction(tx: SignedTransaction): ValidationResult = {
    tx.validate match {
      case ValidationResult.ValidateOke =>
        if (isValid(tx, tx.timestamp)) {
          onNewOffchainTransaction(tx)
          ValidationResult.ValidateOke
        } else ValidationResult.StateCheckFailed
      case error: ValidationResult => error
    }
  }

  /**
    * Validate transactions according to the State and send it to network
    */
  def broadcastTransactions(txs: Seq[SignedTransaction]): ValidationResult = {
    if(txs.nonEmpty && isValid(txs, txs.map(_.timestamp).max)) {
      txs.foreach(onNewOffchainTransaction)
      ValidationResult.ValidateOke
    } else {
      ValidationResult.StateCheckFailed
    }
  }

  def reissueAsset(request: ReissueRequest, wallet: Wallet): Try[ReissueTransaction] = Try {
    val sender = wallet.privateKeyAccount(request.sender).get
    val reissue = ReissueTransaction.create(sender,
      Base58.decode(request.assetId).get,
      request.quantity,
      request.reissuable,
      request.fee,
      getTimestamp)
    if (isValid(reissue, reissue.timestamp)) onNewOffchainTransaction(reissue)
    else throw new StateCheckFailed("Invalid reissue transaction generated: " + reissue.json)
    reissue
  }

  def deleteAsset(request: DeleteRequest, wallet: Wallet): Try[DeleteTransaction] = Try {
    val sender = wallet.privateKeyAccount(request.sender).get
    val tx = DeleteTransaction.create(sender,
      Base58.decode(request.assetId).get,
      request.quantity,
      request.fee,
      getTimestamp)
    if (isValid(tx, tx.timestamp)) onNewOffchainTransaction(tx)
    else throw new StateCheckFailed("Invalid reissue transaction generated: " + tx.json)
    tx
  }

  private var txTime: Long = 0

  private def getTimestamp: Long = synchronized {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): PaymentTransaction = {
    val time = getTimestamp
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
    val payment = new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, sig)
    if (isValid(payment, payment.timestamp)) onNewOffchainTransaction(payment)
    payment
  }

  def createOrderMatch(buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
                       buyMatcherFee: Long, sellMatcherFee: Long, fee: Long, wallet: Wallet): Try[OrderMatch] = Try {
    val matcher = wallet.privateKeyAccount(buyOrder.matcher.address).get
    val om = OrderMatch.create(matcher, buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, getTimestamp)
    if (isValid(om, om.timestamp)) onNewOffchainTransaction(om)
    else throw new StateCheckFailed("Invalid ordermatch transaction  generated: " + om.json)
    om
  }

  override def genesisData: BlockField[StoredInBlock] = {
    val ipoMembers = List(
      "3Mb4mR4taeYS3wci78SntztFwLoaS6Wbg81",
      "3MbWTyn6Tg7zL6XbdN8TLcFMfhWX77hKcmc",
      "3Mn3UAtrpGY3cwiqLYf973q29oDR2LpnMYv"
    )

    val timestamp = 0L
    val totalBalance = InitialBalance

    val txs = ipoMembers.map { addr =>
      val recipient = new Account(addr)
      GenesisTransaction(recipient, totalBalance / ipoMembers.length, timestamp)
    }

    TransactionsBlockField(txs)
  }

  /** Check whether tx is valid on current state and not expired yet
    */
  override def isValid(tx: Transaction, blockTime: Long): Boolean = try {
    val lastBlockTs = blockStorage.history.lastBlock.timestampField.value
    val notExpired = (lastBlockTs - tx.timestamp).millis <= MaxTimeForUnconfirmed
    notExpired && blockStorage.state.isValid(tx, blockTime)
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      false
    case NonFatal(t) =>
      log.error(s"Unexpected error during validation", t)
      throw t
  }

  /** Check whether txs is valid on current state and not expired yet
    */
  def isValid(txs: Seq[Transaction], blockTime: Long): Boolean = try {
    val notExpiredForAll = txs.forall(tx => {
      val lastBlockTs = blockStorage.history.lastBlock.timestampField.value
      (lastBlockTs - tx.timestamp).millis <= MaxTimeForUnconfirmed
    })
    notExpiredForAll && blockStorage.state.isValid(txs, None, blockTime)
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      false
    case NonFatal(t) =>
      log.error(s"Unexpected error during validation", t)
      throw t
  }

  override def isValid(block: Block): Boolean = try {
    val lastBlockTs = blockStorage.history.lastBlock.timestampField.value
    lazy val txsAreNew = block.transactions.forall { tx => (lastBlockTs - tx.timestamp).millis <= MaxTxAndBlockDiff }
    lazy val blockIsValid = blockStorage.state.isValid(block.transactions, blockStorage.history.heightOf(block), block.timestampField.value)
    if (!txsAreNew) log.debug(s"Invalid txs in block ${block.encodedId}: txs from the past")
    if (!blockIsValid) log.debug(s"Invalid txs in block ${block.encodedId}: not valid txs")
    txsAreNew && blockIsValid
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      false
    case NonFatal(t) =>
      log.error(s"Unexpected error during validation", t)
      throw t
  }
}

object SimpleTransactionModule {
  type StoredInBlock = Seq[Transaction]

  val MaxTimeDrift = 15.seconds
  val MaxTimeForUnconfirmed = 90.minutes
  val MaxTxAndBlockDiff = 2.hour
  val MaxTransactionsPerBlock = 100
}
