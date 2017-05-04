package scorex.transaction

import com.google.common.base.Charsets
import scorex.account._
import scorex.api.http.alias.CreateAliasRequest
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.crypto.encode.Base58
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.utils.Time
import scorex.wallet.Wallet
import scorex.waves.transaction.SignedPaymentRequest

trait TransactionOperations {
  def transferAsset(request: TransferRequest, wallet: Wallet): Either[ValidationError, TransferTransaction]
  def issueAsset(request: IssueRequest, wallet: Wallet): Either[ValidationError, IssueTransaction]
  def reissueAsset(request: ReissueRequest, wallet: Wallet): Either[ValidationError, ReissueTransaction]
  def burnAsset(request: BurnRequest, wallet: Wallet): Either[ValidationError, BurnTransaction]
  def makeAssetNameUnique(request: MakeAssetNameUniqueRequest, wallet: Wallet): Either[ValidationError, MakeAssetNameUniqueTransaction]
  def lease(request: LeaseRequest, wallet: Wallet): Either[ValidationError, LeaseTransaction]
  def alias(request: CreateAliasRequest, wallet: Wallet): Either[ValidationError, CreateAliasTransaction]
  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet): Either[ValidationError, LeaseCancelTransaction]
  def createPayment(payment: PaymentRequest, wallet: Wallet): Either[ValidationError, PaymentTransaction]
  def broadcastPayment(payment: SignedPaymentRequest): Either[ValidationError, PaymentTransaction]
}

class TransactionOperationsImpl(tm: NewTransactionHandler, time: Time) extends TransactionOperations {
  override def createPayment(request: PaymentRequest, wallet: Wallet): Either[ValidationError, PaymentTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    rec <- Account.fromString(request.recipient)
    tx <- PaymentTransaction.create(pk, rec, request.amount, request.fee, time.getTimestamp)
    r <- tm.onNewOffchainTransaction(tx)
  } yield r


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
      r <- tm.onNewOffchainTransaction(tx)
    } yield r

  override def issueAsset(request: IssueRequest, wallet: Wallet): Either[ValidationError, IssueTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      tx <- IssueTransaction.create(senderPrivateKey,
        request.name.getBytes(Charsets.UTF_8),
        request.description.getBytes(Charsets.UTF_8),
        request.quantity, request.decimals, request.reissuable, request.fee, time.getTimestamp)
      r <- tm.onNewOffchainTransaction(tx)
    } yield r

  def lease(request: LeaseRequest, wallet: Wallet): Either[ValidationError, LeaseTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    recipientAcc <- AccountOrAlias.fromString(request.recipient)
    tx <- LeaseTransaction.create(senderPrivateKey, request.amount, request.fee, time.getTimestamp, recipientAcc)
    r <- tm.onNewOffchainTransaction(tx)
  } yield r

  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet): Either[ValidationError, LeaseCancelTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      tx <- LeaseCancelTransaction.create(pk, Base58.decode(request.txId).get, request.fee, time.getTimestamp)
      t <- tm.onNewOffchainTransaction(tx)
    } yield t


  override def alias(request: CreateAliasRequest, wallet: Wallet): Either[ValidationError, CreateAliasTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    alias <- Alias.buildWithCurrentNetworkByte(request.alias)
    tx <- CreateAliasTransaction.create(senderPrivateKey, alias, request.fee, time.getTimestamp)
    r <- tm.onNewOffchainTransaction(tx)
  } yield r

  override def reissueAsset(request: ReissueRequest, wallet: Wallet): Either[ValidationError, ReissueTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- ReissueTransaction.create(pk, Base58.decode(request.assetId).get, request.quantity, request.reissuable, request.fee, time.getTimestamp)
    r <- tm.onNewOffchainTransaction(tx)
  } yield r


  override def burnAsset(request: BurnRequest, wallet: Wallet): Either[ValidationError, BurnTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- BurnTransaction.create(pk, Base58.decode(request.assetId).get, request.quantity, request.fee, time.getTimestamp)
    r <- tm.onNewOffchainTransaction(tx)
  } yield r

  override def makeAssetNameUnique(request: MakeAssetNameUniqueRequest, wallet: Wallet): Either[ValidationError, MakeAssetNameUniqueTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- MakeAssetNameUniqueTransaction.create(pk, Base58.decode(request.assetId).get, request.fee, time.getTimestamp)
    r <- tm.onNewOffchainTransaction(tx)
  } yield r

  override def broadcastPayment(payment: SignedPaymentRequest): Either[ValidationError, PaymentTransaction] =
    for {
      _signature <- Base58.decode(payment.signature).toOption.toRight(ValidationError.InvalidSignature)
      _sender <- PublicKeyAccount.fromBase58String(payment.senderPublicKey)
      _recipient <- Account.fromString(payment.recipient)
      tx <- PaymentTransaction.create(_sender, _recipient, payment.amount, payment.fee, payment.timestamp, _signature)
      t <- tm.onNewOffchainTransaction(tx)
    } yield t
}