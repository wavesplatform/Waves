package scorex.transaction

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.app.Application
import scorex.block.{Block, BlockField}
import scorex.consensus.TransactionsOrdering
import scorex.crypto.encode.Base58
import scorex.network.message.Message
import scorex.network.{Broadcast, NetworkController, TransactionalMessagesRepo}
import scorex.settings.{ChainParameters, Settings}
import scorex.transaction.assets.{BurnTransaction, _}
import scorex.transaction.state.database.{BlockStorageImpl, UnconfirmedTransactionsDatabaseImpl}
import scorex.transaction.state.wallet._
import scorex.utils._
import scorex.wallet.Wallet
import scorex.waves.transaction.SignedPayment

import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Left, Right, Try}

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


class SimpleTransactionModule(hardForkParams: ChainParameters)(implicit val settings: Settings,
                                                               application: Application)
  extends TransactionModule with ScorexLogging {

  import SimpleTransactionModule._

  val networkController = application.networkController
  private val feeCalculator = new FeeCalculator(settings)

  val InitialBalance = hardForkParams.initialBalance

  val utxStorage: UnconfirmedTransactionsStorage = new UnconfirmedTransactionsDatabaseImpl(settings)

  override val blockStorage = new BlockStorageImpl(settings, hardForkParams)(application.consensusModule, this)

  override def unconfirmedTxs: Seq[Transaction] = utxStorage.all()

  override def putUnconfirmedIfNew(tx: Transaction): Boolean = synchronized {
    if (feeCalculator.enoughFee(tx)) {
      utxStorage.putIfNew(tx, isValid(_, tx.timestamp))
    } else false
  }

  override def packUnconfirmed(): Seq[Transaction] = synchronized {
    clearIncorrectTransactions()

    val txs = utxStorage.all().sorted(TransactionsOrdering).take(MaxTransactionsPerBlock)
    val valid = blockStorage.state.validate(txs, NTP.correctedTime())

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
    val valid = blockStorage.state.validate(notFromFuture, currentTime)
    // remove non valid or expired from storage
    txs.diff(valid).foreach(utxStorage.remove)
  }

  override def onNewOffchainTransaction(transaction: Transaction): Unit =
    if (putUnconfirmedIfNew(transaction)) {
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(transaction), None)
      networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
    }

  def createPayment(payment: Payment, wallet: Wallet): Option[Either[ValidationError, PaymentTransaction]] = {
    wallet.privateKeyAccount(payment.sender).map { sender =>
      createPayment(sender, new Account(payment.recipient), payment.amount, payment.fee)
    }
  }

  def transferAsset(request: TransferRequest, wallet: Wallet): Try[Either[ValidationError, TransferTransaction]] = Try {
    val sender = wallet.privateKeyAccount(request.sender).get

    val transferVal = TransferTransaction.create(request.assetId.map(s => Base58.decode(s).get),
      sender: PrivateKeyAccount,
      new Account(request.recipient),
      request.amount,
      getTimestamp,
      request.feeAssetId.map(s => Base58.decode(s).get),
      request.fee,
      Option(request.attachment).filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray))

    transferVal match {
      case Right(tx) =>
        if (isValid(tx, tx.timestamp)) onNewOffchainTransaction(tx)
        else throw new StateCheckFailed("Invalid transfer transaction generated: " + tx.json)
      case Left(err) =>
        throw new IllegalArgumentException(err.toString)
    }
    transferVal
  }

  def issueAsset(request: IssueRequest, wallet: Wallet): Try[IssueTransaction] = Try {
    val sender = wallet.privateKeyAccount(request.sender).get
    val issueVal = IssueTransaction.create(sender,
      request.name.getBytes(Charsets.UTF_8),
      request.description.getBytes(Charsets.UTF_8),
      request.quantity,
      request.decimals,
      request.reissuable,
      request.fee,
      getTimestamp)

    issueVal match {
      case Right(tx) =>
        if (isValid(tx, tx.timestamp)) onNewOffchainTransaction(tx)
        else throw new StateCheckFailed("Invalid issue transaction generated: " + tx.json)
        tx
      case Left(err) =>
        throw new IllegalArgumentException(err.toString)

    }
  }

  /**
    * Validate transaction according to the State and send it to network
    */
  def broadcastTransaction(tx: SignedTransaction): Either[ValidationError, Unit] = {
    if (isValid(tx, tx.timestamp)) {
      onNewOffchainTransaction(tx)
      Right(())
    } else Left(ValidationError.StateCheckFailed)
  }

  /**
    * Validate transactions according to the State and send it to network
    */
  def broadcastTransactions(txs: Seq[SignedTransaction]): Either[ValidationError, Unit] = {
    if (txs.nonEmpty && isValid(txs, txs.map(_.timestamp).max)) {
      txs.foreach(onNewOffchainTransaction)
      Right(())
    } else {
      Left(ValidationError.StateCheckFailed)
    }
  }

  def reissueAsset(request: ReissueRequest, wallet: Wallet): Try[ReissueTransaction] = Try {
    val sender = wallet.privateKeyAccount(request.sender).get
    val reissueVal = ReissueTransaction.create(sender,
      Base58.decode(request.assetId).get,
      request.quantity,
      request.reissuable,
      request.fee,
      getTimestamp)

    reissueVal match {
      case Right(tx) =>
        if (isValid(tx, tx.timestamp)) onNewOffchainTransaction(tx)
        else throw new StateCheckFailed("Invalid reissue transaction generated: " + tx.json)
        tx
      case Left(err) =>
        throw new IllegalArgumentException(err.toString)
    }
  }

  def burnAsset(request: BurnRequest, wallet: Wallet): Try[BurnTransaction] = Try {
    val sender = wallet.privateKeyAccount(request.sender).get
    val txVal: Either[ValidationError, BurnTransaction] = BurnTransaction.create(sender,
      Base58.decode(request.assetId).get,
      request.quantity,
      request.fee,
      getTimestamp)

    txVal match {
      case Right(tx) =>
        if (isValid(tx, tx.timestamp)) onNewOffchainTransaction(tx)
        else throw new StateCheckFailed("Invalid delete transaction generated: " + tx.json)
        tx
      case Left(err) =>
        throw new IllegalArgumentException(err.toString)

    }
  }

  private var txTime: Long = 0

  private def getTimestamp: Long = synchronized {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): Either[ValidationError, PaymentTransaction] = {
    val pt = PaymentTransaction.create(sender, recipient, amount, fee, getTimestamp)
    pt match {
      case Right(t) => onNewOffchainTransaction(t)
      case _ =>
    }
    pt
  }

  override def genesisData: Seq[Transaction] = {
    val ipoMembers = List(
      "3N3rfWUDPkFsf2GEZBCLw491A79G46djvQk",
      "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K",
      "3N6dsnfD88j5yKgpnEavaaJDzAVSRBRVbMY"
      /*
            "3Mb4mR4taeYS3wci78SntztFwLoaS6iiKY9",
            "3MbWTyn6Tg7zL6XbdN8TLcFMfhWX76fGNCz",
            "3Mn3UAtrpGY3cwiqLYf973q29oDR2Kw7UyV"
      */
    )

    val timestamp = 0L
    val totalBalance = InitialBalance

    val txs = ipoMembers.map { addr =>
      val recipient = new Account(addr)
      GenesisTransaction.create(recipient, totalBalance / ipoMembers.length, timestamp)
    }.map(_.right.get)

    txs
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
    notExpiredForAll && blockStorage.state.allValid(txs, blockTime)
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
    lazy val txsAreNew = block.transactionData.forall { tx => (lastBlockTs - tx.timestamp).millis <= MaxTxAndBlockDiff }
    assert(blockStorage.history.heightOf(block).isEmpty, "Should not exist, this is new block, right?")
    lazy val blockIsValid = blockStorage.state.allValid(block.transactionData, block.timestampField.value)
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

  val minimumTxFee = settings.asInstanceOf[WavesSettings].minimumTxFee

  def signPayment(payment: Payment, wallet: Wallet): Option[Either[ValidationError, PaymentTransaction]] = {
    wallet.privateKeyAccount(payment.sender).map { sender =>
      PaymentTransaction.create(sender, new Account(payment.recipient), payment.amount, payment.fee, NTP.correctedTime())
    }
  }

  def createSignedPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction] = {

    val paymentVal = PaymentTransaction.create(sender, recipient, amount, fee, timestamp)

    paymentVal match {
      case Right(payment) => {
        if (blockStorage.state.isValid(payment, payment.timestamp)) {
          Right(payment)
        } else Left(ValidationError.NoBalance)
      }
      case Left(err) => Left(err)
    }
  }

  /**
    * Publish signed payment transaction which generated outside node
    */
  def broadcastPayment(payment: SignedPayment): Either[ValidationError, PaymentTransaction] = {
    val maybeSignatureBytes = Base58.decode(payment.signature).toOption
    if (payment.fee < minimumTxFee)
      Left(ValidationError.InsufficientFee)
    else if (maybeSignatureBytes.isEmpty)
      Left(ValidationError.InvalidSignature)
    else {
      val time = payment.timestamp
      val sigBytes = maybeSignatureBytes.get
      val senderPubKey = payment.senderPublicKey
      val recipientAccount = payment.recipient
      val txVal = PaymentTransaction.create(senderPubKey, recipientAccount, payment.amount, payment.fee, time, sigBytes)
      txVal match {
        case Right(tx) => {
          if (blockStorage.state.isValid(tx, tx.timestamp)) {
            onNewOffchainTransaction(tx)
            Right(tx)
          } else Left(ValidationError.NoBalance)
        }
        case Left(err) => Left(err)
      }
    }
  }
}

object SimpleTransactionModule {

  val MaxTimeDrift: FiniteDuration = 15.seconds
  val MaxTimeForUnconfirmed: FiniteDuration = 90.minutes
  val MaxTxAndBlockDiff: FiniteDuration = 2.hour
  val MaxTransactionsPerBlock: Int = 100
}
