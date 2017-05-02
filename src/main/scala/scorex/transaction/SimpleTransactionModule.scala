package scorex.transaction

import akka.actor.ActorRef
import com.google.common.base.Charsets
import com.wavesplatform.settings.{FunctionalitySettings, GenesisSettings, WavesSettings}
import com.wavesplatform.state2.Validator
import com.wavesplatform.state2.reader.StateReader
import scorex.account._
import scorex.api.http.alias.CreateAliasRequest
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.network._
import scorex.network.message.Message
import scorex.transaction.assets.{BurnTransaction, _}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.utils._
import scorex.wallet.Wallet
import scorex.waves.transaction.SignedPaymentRequest

import scala.concurrent.duration._
import scala.util.Right


class SimpleTransactionModule(fs: FunctionalitySettings, networkController: ActorRef, time: Time, feeCalculator: FeeCalculator,
                              utxStorage: UnconfirmedTransactionsStorage, history: History, stateReader: StateReader)
  extends TransactionModule with TransactionOperations with ScorexLogging {

  override def onNewOffchainTransaction[T <: Transaction](transaction: T, exceptOf: Option[ConnectedPeer]): Either[ValidationError, T] =
    for {
      validAgainstFee <- feeCalculator.enoughFee(transaction)
      tx <- utxStorage.putIfNew(validAgainstFee, (t: T) => Validator.validateWithHistory(history, fs, stateReader)(t))
    } yield {
      val ntwMsg = Message(TransactionalMessagesRepo.TransactionMessageSpec, Right(transaction), None)
      networkController ! NetworkController.SendToNetwork(ntwMsg, exceptOf.map(BroadcastExceptOf).getOrElse(Broadcast))
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
          time.getTimestamp,
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
        request.quantity, request.decimals, request.reissuable, request.fee, time.getTimestamp)
      r <- onNewOffchainTransaction(tx)
    } yield r

  def lease(request: LeaseRequest, wallet: Wallet): Either[ValidationError, LeaseTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    recipientAcc <- AccountOrAlias.fromString(request.recipient)
    tx <- LeaseTransaction.create(senderPrivateKey, request.amount, request.fee, time.getTimestamp, recipientAcc)
    r <- onNewOffchainTransaction(tx)
  } yield r

  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet): Either[ValidationError, LeaseCancelTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      tx <- LeaseCancelTransaction.create(pk, Base58.decode(request.txId).get, request.fee, time.getTimestamp)
      t <- onNewOffchainTransaction(tx)
    } yield t


  override def alias(request: CreateAliasRequest, wallet: Wallet): Either[ValidationError, CreateAliasTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    alias <- Alias.buildWithCurrentNetworkByte(request.alias)
    tx <- CreateAliasTransaction.create(senderPrivateKey, alias, request.fee, time.getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r

  override def reissueAsset(request: ReissueRequest, wallet: Wallet): Either[ValidationError, ReissueTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- ReissueTransaction.create(pk, Base58.decode(request.assetId).get, request.quantity, request.reissuable, request.fee, time.getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r


  override def burnAsset(request: BurnRequest, wallet: Wallet): Either[ValidationError, BurnTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- BurnTransaction.create(pk, Base58.decode(request.assetId).get, request.quantity, request.fee, time.getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r

  override def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): Either[ValidationError, PaymentTransaction] =
    PaymentTransaction.create(sender, recipient, amount, fee, time.getTimestamp)
      .flatMap(onNewOffchainTransaction(_, None))

  override def broadcastPayment(payment: SignedPaymentRequest): Either[ValidationError, PaymentTransaction] =
    for {
      _signature <- Base58.decode(payment.signature).toOption.toRight(ValidationError.InvalidSignature)
      _sender <- PublicKeyAccount.fromBase58String(payment.senderPublicKey)
      _recipient <- Account.fromString(payment.recipient)
      tx <- PaymentTransaction.create(_sender, _recipient, payment.amount, payment.fee, payment.timestamp, _signature)
      t <- onNewOffchainTransaction(tx)
    } yield t
}

object SimpleTransactionModule {

  val MaxTimeUtxFuture: FiniteDuration = 15.seconds
  val MaxTimeUtxPast: FiniteDuration = 90.minutes
  val MaxTimeTransactionOverBlockDiff: FiniteDuration = 90.minutes
  val MaxTimePreviousBlockOverTransactionDiff: FiniteDuration = 90.minutes
  val MaxTimeCurrentBlockOverTransactionDiff: FiniteDuration = 2.hour
  val MaxTransactionsPerBlock: Int = 100
  val BaseTargetLength: Int = 8
  val GeneratorSignatureLength: Int = 32
  val MinimalEffectiveBalanceForGenerator: Long = 1000000000000L
  val AvgBlockTimeDepth: Int = 3
  val MaxTimeDrift: FiniteDuration = 15.seconds
  val EmptySignature: Array[Byte] = Array.fill(DigestSize)(0: Byte)
  val Version: Byte = 2


  def buildTransactions(genesisSettings: GenesisSettings): Seq[GenesisTransaction] = {
    genesisSettings.transactions.map { ts =>
      val acc = Account.fromString(ts.recipient).right.get
      GenesisTransaction.create(acc, ts.amount, genesisSettings.transactionsTimestamp).right.get
    }
  }

}
