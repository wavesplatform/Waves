package scorex.transaction

import com.google.common.base.Charsets
import com.wavesplatform.history.BlockStorageImpl
import com.wavesplatform.settings.{GenesisSettings, WavesSettings}
import com.wavesplatform.state2.Validator
import scorex.account._
import scorex.api.http.alias.CreateAliasRequest
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.app.Application
import scorex.consensus.TransactionsOrdering
import scorex.crypto.encode.Base58
import scorex.network.message.Message
import scorex.network.{Broadcast, NetworkController, TransactionalMessagesRepo}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.{BurnTransaction, _}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.utils._
import scorex.wallet.Wallet
import scorex.waves.transaction.SignedPaymentRequest

import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Left, Right}


class SimpleTransactionModule(genesisSettings: GenesisSettings)(implicit val settings: WavesSettings,
                                                                application: Application)
  extends TransactionModule with TransactionOperations with ScorexLogging {

  import SimpleTransactionModule._

  private val networkController = application.networkController
  private val feeCalculator = new FeeCalculator(settings.feesSettings)
  private val fs = settings.blockchainSettings.functionalitySettings

  val utxStorage: UnconfirmedTransactionsStorage = new UnconfirmedTransactionsDatabaseImpl(settings.utxSettings)

  override val blockStorage = new BlockStorageImpl(settings.blockchainSettings)

  override def unconfirmedTxs: Seq[Transaction] = utxStorage.all()

  override def putUnconfirmedIfNew[T <: Transaction](tx: T): Either[ValidationError, T] = synchronized {
    for {
      t1 <- feeCalculator.enoughFee(tx)
      t2 <- utxStorage.putIfNew(t1, (t: T) => validate(t))
    } yield t2
  }

  override def packUnconfirmed(heightOpt: Option[Int]): Seq[Transaction] = synchronized {
    clearIncorrectTransactions()

    val txs = utxStorage.all()
      .sorted(TransactionsOrdering.InUTXPool)
      .take(MaxTransactionsPerBlock)
      .sorted(TransactionsOrdering.InBlock)

    val valid = Validator.validate(fs, blockStorage.stateReader, txs, heightOpt, NTP.correctedTime())._2

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
    val notExpired = txs.filter { tx => (currentTime - tx.timestamp).millis <= MaxTimeUtxPast }
    val notFromFuture = notExpired.filter { tx => (tx.timestamp - currentTime).millis <= MaxTimeUtxFuture }
    val inOrder = notFromFuture.sorted(TransactionsOrdering.InUTXPool)
    val valid = Validator.validate(fs, blockStorage.stateReader, inOrder, None, currentTime)._2
    // remove non valid or expired from storage
    txs.diff(valid).foreach(utxStorage.remove)
  }

  override def onNewOffchainTransaction[T <: Transaction](transaction: T): Either[ValidationError, T] =
    for {
      tx <- putUnconfirmedIfNew(transaction)
    } yield {
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(transaction), None)
      networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
      tx
    }

  override def createPayment(request: PaymentRequest, wallet: Wallet): Either[ValidationError, PaymentTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    rec <- Account.fromString(request.recipient)
    pmt <- createPayment(pk, rec, request.amount, request.fee)
  } yield pmt


  override def transferAsset(request: TransferRequest, wallet: Wallet): Either[ValidationError, TransferTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc <- AccountOrAlias.fromString(request.recipient)
      tx <- TransferTransaction
        .create(request.assetId.map(s => Base58.decode(s).get),
          senderPrivateKey,
          recipientAcc,
          request.amount,
          getTimestamp,
          request.feeAssetId.map(s => Base58.decode(s).get),
          request.fee,
          request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray))
      r <- onNewOffchainTransaction(tx)
    } yield r

  override def issueAsset(request: IssueRequest, wallet: Wallet): Either[ValidationError, IssueTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      tx <- IssueTransaction.create(senderPrivateKey,
        request.name.getBytes(Charsets.UTF_8),
        request.description.getBytes(Charsets.UTF_8),
        request.quantity, request.decimals, request.reissuable, request.fee, getTimestamp)
      r <- onNewOffchainTransaction(tx)
    } yield r

  def lease(request: LeaseRequest, wallet: Wallet): Either[ValidationError, LeaseTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    recipientAcc <- AccountOrAlias.fromString(request.recipient)
    tx <- LeaseTransaction.create(senderPrivateKey, request.amount, request.fee, getTimestamp, recipientAcc)
    r <- onNewOffchainTransaction(tx)
  } yield r

  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet): Either[ValidationError, LeaseCancelTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      lease <- LeaseCancelTransaction.create(pk, Base58.decode(request.txId).get, request.fee, getTimestamp)
      t <- onNewOffchainTransaction(lease)
    } yield t


  override def alias(request: CreateAliasRequest, wallet: Wallet): Either[ValidationError, CreateAliasTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    alias <- Alias.buildWithCurrentNetworkByte(request.alias)
    tx <- CreateAliasTransaction.create(senderPrivateKey, alias, request.fee, getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r

  override def reissueAsset(request: ReissueRequest, wallet: Wallet): Either[ValidationError, ReissueTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- ReissueTransaction.create(pk, Base58.decode(request.assetId).get, request.quantity, request.reissuable, request.fee, getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r


  override def burnAsset(request: BurnRequest, wallet: Wallet): Either[ValidationError, BurnTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- BurnTransaction.create(pk, Base58.decode(request.assetId).get, request.quantity, request.fee, getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r

  override def makeAssetNameUnique(request: MakeAssetNameUniqueRequest, wallet: Wallet): Either[ValidationError, MakeAssetNameUniqueTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- MakeAssetNameUniqueTransaction.create(pk, Base58.decode(request.assetId).get, request.fee, getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r


  private var txTime: Long = 0

  private def getTimestamp: Long = synchronized {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  override def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): Either[ValidationError, PaymentTransaction] =
    PaymentTransaction.create(sender, recipient, amount, fee, getTimestamp)
      .flatMap(onNewOffchainTransaction)


  override def genesisData: Seq[Transaction] = buildTransactions(genesisSettings)


  /** Check whether tx is valid on current state and not expired yet
    */
  override def validate[T <: Transaction](tx: T): Either[ValidationError, T] = try {
    val lastBlockTimestamp = blockStorage.history.lastBlock.timestamp
    val notExpired = (lastBlockTimestamp - tx.timestamp).millis <= MaxTimePreviousBlockOverTransactionDiff
    if (notExpired) {
      Validator.validate(fs, blockStorage.stateReader, tx)
    } else {
      Left(TransactionValidationError(tx, s"Transaction is too old: Last block timestamp is $lastBlockTimestamp"))
    }
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      Left(TransactionValidationError(tx, "DB can't find last block because of unexpected modification"))
    case NonFatal(t) =>
      log.error(s"Unexpected error during validation", t)
      throw t
  }

  override def createPayment(sender: PrivateKeyAccount, recipient: Account,
                             amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction] =
    PaymentTransaction.create(sender, recipient, amount, fee, timestamp)

  override def broadcastPayment(payment: SignedPaymentRequest): Either[ValidationError, PaymentTransaction] =
    for {
      _signature <- Base58.decode(payment.signature).toOption.toRight(ValidationError.InvalidSignature)
      _sender <- PublicKeyAccount.fromBase58String(payment.senderPublicKey)
      _recipient <- Account.fromString(payment.recipient)
      _t <- PaymentTransaction.create(_sender, _recipient, payment.amount, payment.fee, payment.timestamp, _signature)
      t <- onNewOffchainTransaction(_t)
    } yield t
}

object SimpleTransactionModule {
  val MaxTimeUtxFuture: FiniteDuration = 15.seconds
  val MaxTimeUtxPast: FiniteDuration = 90.minutes
  val MaxTimeTransactionOverBlockDiff: FiniteDuration = 90.minutes
  val MaxTimePreviousBlockOverTransactionDiff: FiniteDuration = 90.minutes
  val MaxTimeCurrentBlockOverTransactionDiff: FiniteDuration = 2.hour
  val MaxTransactionsPerBlock: Int = 100


  def buildTransactions(genesisSettings: GenesisSettings): Seq[GenesisTransaction] = {
    genesisSettings.transactions.map { ts =>
      val acc = Account.fromString(ts.recipient).right.get
      GenesisTransaction.create(acc, ts.amount, genesisSettings.transactionsTimestamp).right.get
    }
  }

}
