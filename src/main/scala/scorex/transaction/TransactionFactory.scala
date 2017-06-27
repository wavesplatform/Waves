package scorex.transaction

import com.google.common.base.Charsets
import com.wavesplatform.state2.ByteStr
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

object TransactionFactory {

  def createPayment(request: PaymentRequest, wallet: Wallet, time: Time): Either[ValidationError, PaymentTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    rec <- Account.fromString(request.recipient)
    tx <- PaymentTransaction.create(pk, rec, request.amount, request.fee, time.getTimestamp())
  } yield tx


  def transferAsset(request: TransferRequest, wallet: Wallet, time: Time): Either[ValidationError, TransferTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc <- AccountOrAlias.fromString(request.recipient)
      tx <- TransferTransaction
        .create(request.assetId.map(s => ByteStr.decodeBase58(s).get),
          senderPrivateKey,
          recipientAcc,
          request.amount,
          time.getTimestamp(),
          request.feeAssetId.map(s => ByteStr.decodeBase58(s).get),
          request.fee,
          request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray))
    } yield tx

  def issueAsset(request: IssueRequest, wallet: Wallet, time: Time): Either[ValidationError, IssueTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      tx <- IssueTransaction.create(senderPrivateKey,
        request.name.getBytes(Charsets.UTF_8),
        request.description.getBytes(Charsets.UTF_8),
        request.quantity, request.decimals, request.reissuable, request.fee, time.getTimestamp())
    } yield tx

  def lease(request: LeaseRequest, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    recipientAcc <- AccountOrAlias.fromString(request.recipient)
    tx <- LeaseTransaction.create(senderPrivateKey, request.amount, request.fee, time.getTimestamp(), recipientAcc)
  } yield tx

  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      tx <- LeaseCancelTransaction.create(pk, ByteStr.decodeBase58(request.txId).get, request.fee, time.getTimestamp())
    } yield tx


  def alias(request: CreateAliasRequest, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    alias <- Alias.buildWithCurrentNetworkByte(request.alias)
    tx <- CreateAliasTransaction.create(senderPrivateKey, alias, request.fee, time.getTimestamp())
  } yield tx

  def reissueAsset(request: ReissueRequest, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- ReissueTransaction.create(pk, ByteStr.decodeBase58(request.assetId).get, request.quantity, request.reissuable, request.fee, time.getTimestamp())
  } yield tx


  def burnAsset(request: BurnRequest, wallet: Wallet, time: Time): Either[ValidationError, BurnTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- BurnTransaction.create(pk, ByteStr.decodeBase58(request.assetId).get, request.quantity, request.fee, time.getTimestamp())
  } yield tx

  def makeAssetNameUnique(request: MakeAssetNameUniqueRequest, wallet: Wallet, time: Time): Either[ValidationError, MakeAssetNameUniqueTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- MakeAssetNameUniqueTransaction.create(pk, ByteStr.decodeBase58(request.assetId).get, request.fee, AddressScheme.current.chainId, time.getTimestamp())
  } yield tx

  def broadcastPayment(payment: SignedPaymentRequest): Either[ValidationError, PaymentTransaction] =
    for {
      _signature <- ByteStr.decodeBase58(payment.signature).toOption.toRight(ValidationError.InvalidRequestSignature)
      _sender <- PublicKeyAccount.fromBase58String(payment.senderPublicKey)
      _recipient <- Account.fromString(payment.recipient)
      tx <- PaymentTransaction.create(_sender, _recipient, payment.amount, payment.fee, payment.timestamp, _signature)
    } yield tx
}