package scorex.transaction

import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Left, Right}
import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.app.Application
import scorex.block.{Block, BlockField}
import scorex.consensus.TransactionsOrdering
import scorex.crypto.encode.Base58
import scorex.network.message.Message
import scorex.network.{Broadcast, NetworkController, TransactionalMessagesRepo}
import scorex.settings.ChainParameters
import scorex.transaction.ValidationError.{InvalidAddress, StateCheckFailed}
import scorex.transaction.assets.{BurnTransaction, _}
import scorex.transaction.state.database.{BlockStorageImpl, UnconfirmedTransactionsDatabaseImpl}
import scorex.transaction.state.wallet._
import scorex.utils._
import scorex.wallet.Wallet
import scorex.waves.transaction.SignedPayment

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

class SimpleTransactionModule(hardForkParams: ChainParameters)(implicit val settings: WavesSettings,
                                                               application: Application)
  extends TransactionModule with TransactionOperations with ScorexLogging {

  import SimpleTransactionModule._

  val networkController = application.networkController
  private val feeCalculator = new FeeCalculator(settings.feesSettings)

  val InitialBalance = hardForkParams.initialBalance

  val utxStorage: UnconfirmedTransactionsStorage = new UnconfirmedTransactionsDatabaseImpl(settings.utxSettings)

  override val blockStorage = new BlockStorageImpl(settings.blockchainSettings)(application.consensusModule, this)

  override def unconfirmedTxs: Seq[Transaction] = utxStorage.all()

  override def putUnconfirmedIfNew(tx: Transaction): Boolean = synchronized {
    if (feeCalculator.enoughFee(tx)) {
      utxStorage.putIfNew(tx, isValid(_, tx.timestamp))
    } else false
  }

  override def packUnconfirmed(heightOpt: Option[Int]): Seq[Transaction] = synchronized {
    clearIncorrectTransactions()

    val txs = utxStorage.all().sorted(TransactionsOrdering).take(MaxTransactionsPerBlock)
    val valid = blockStorage.state.validate(txs, heightOpt, NTP.correctedTime())

    if (valid.size != txs.size) {
      log.debug(s"Txs for new block do not match: valid=${valid.size} vs all=${txs.size}")
    }

    valid
  }

  override def clearFromUnconfirmed(data: Seq[Transaction]): Unit = synchronized {
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

  override def onNewOffchainTransaction(transaction: Transaction): Boolean =
    if (putUnconfirmedIfNew(transaction)) {
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(transaction), None)
      networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
      true
    } else false


  override def createPayment(payment: Payment, wallet: Wallet): Either[ValidationError, PaymentTransaction] = {
    createPayment(wallet.privateKeyAccount(payment.sender).get, new Account(payment.recipient), payment.amount, payment.fee)
  }

  override def transferAsset(request: TransferRequest, wallet: Wallet): Either[ValidationError, TransferTransaction] = {
    val sender = wallet.privateKeyAccount(request.sender).get
    TransferTransaction
      .create(request.assetId.map(s => Base58.decode(s).get),
        sender: PrivateKeyAccount,
        new Account(request.recipient),
        request.amount,
        getTimestamp,
        request.feeAssetId.map(s => Base58.decode(s).get),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray))
      .filterOrElse(onNewOffchainTransaction, StateCheckFailed)
  }

  override def issueAsset(request: IssueRequest, wallet: Wallet): Either[ValidationError, IssueTransaction] = {
    val sender = wallet.privateKeyAccount(request.sender).get
    IssueTransaction
      .create(sender, request.name.getBytes(Charsets.UTF_8), request.description.getBytes(Charsets.UTF_8), request.quantity, request.decimals, request.reissuable, request.fee, getTimestamp)
      .filterOrElse(onNewOffchainTransaction, StateCheckFailed)
  }

  override def reissueAsset(request: ReissueRequest, wallet: Wallet): Either[ValidationError, ReissueTransaction] = {
    val sender = wallet.privateKeyAccount(request.sender).get
    ReissueTransaction
      .create(sender, Base58.decode(request.assetId).get, request.quantity, request.reissuable, request.fee, getTimestamp)
      .filterOrElse(onNewOffchainTransaction, StateCheckFailed)
  }

  override def burnAsset(request: BurnRequest, wallet: Wallet): Either[ValidationError, BurnTransaction] = {
    val sender = wallet.privateKeyAccount(request.sender).get

    BurnTransaction
      .create(sender, Base58.decode(request.assetId).get, request.quantity, request.fee, getTimestamp)
      .filterOrElse(onNewOffchainTransaction, StateCheckFailed)
  }

  private var txTime: Long = 0

  private def getTimestamp: Long = synchronized {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  override def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): Either[ValidationError, PaymentTransaction] =
    PaymentTransaction
      .create(sender, recipient, amount, fee, getTimestamp)
      .filterOrElse(onNewOffchainTransaction, StateCheckFailed)

  override def genesisData: Seq[Transaction] = hardForkParams.genesisTxs

  /** Check whether tx is valid on current state and not expired yet
    */
  override def isValid(tx: Transaction, blockTime: Long): Boolean = try {
    val notExpired = (blockStorage.history.lastBlock.timestamp - tx.timestamp).millis <= MaxTimeForUnconfirmed
    notExpired && blockStorage.state.isValid(tx, blockTime)
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
    lazy val txsAreNew = block.transactionDataField.asInstanceOf[TransactionsBlockField].value.forall { tx => (lastBlockTs - tx.timestamp).millis <= MaxTxAndBlockDiff }
    lazy val blockIsValid = blockStorage.state.isValid(block.transactionData, blockStorage.history.heightOf(block), block.timestamp)
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

  val minimumTxFee = 100000 // TODO: remove later

  override def signPayment(payment: Payment, wallet: Wallet): Either[ValidationError, PaymentTransaction] = {
    PaymentTransaction.create(wallet.privateKeyAccount(payment.sender).get, new Account(payment.recipient), payment.amount, payment.fee, NTP.correctedTime())
  }

  override def createSignedPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction] = {

    val paymentVal = PaymentTransaction.create(sender, recipient, amount, fee, timestamp)

    paymentVal match {
      case Right(payment) =>
        if (blockStorage.state.isValid(payment, payment.timestamp)) {
          Right(payment)
        } else Left(ValidationError.NoBalance)
      case Left(err) => Left(err)
    }
  }

  /**
    * Publish signed payment transaction which generated outside node
    */
  override def broadcastPayment(payment: SignedPayment): Either[ValidationError, PaymentTransaction] = {
    val paymentTx = for {
      _signature <- Base58.decode(payment.signature).toOption.toRight(ValidationError.InvalidSignature)
      _sender <- PublicKeyAccount.fromBase58String(payment.senderPublicKey)
      _account <- if (Account.isValidAddress(payment.recipient)) Right(new Account(payment.recipient)) else Left(InvalidAddress)
      tx <- PaymentTransaction.create(_sender, _account, payment.amount, payment.fee, payment.timestamp, _signature)
    } yield tx

    paymentTx.filterOrElse(onNewOffchainTransaction, ValidationError.StateValidationError("State validation failed"))
  }
}

object SimpleTransactionModule {
  val MaxTimeDrift: FiniteDuration = 15.seconds
  val MaxTimeForUnconfirmed: FiniteDuration = 90.minutes
  val MaxTxAndBlockDiff: FiniteDuration = 2.hour
  val MaxTransactionsPerBlock: Int = 100
}
