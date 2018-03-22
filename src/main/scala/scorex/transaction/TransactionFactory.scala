package scorex.transaction

import com.google.common.base.Charsets
import com.wavesplatform.state2.ByteStr
import scorex.account._
import scorex.api.http.DataRequest
import scorex.api.http.alias.CreateAliasRequest
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.crypto.encode.Base58
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.{Script, SetScriptTransaction}
import scorex.utils.Time
import scorex.wallet.Wallet

object TransactionFactory {

  def transferAsset(request: TransferRequest, wallet: Wallet, time: Time): Either[ValidationError, TransferTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc     <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransaction
        .create(
          request.assetId.map(s => ByteStr.decodeBase58(s).get),
          senderPrivateKey,
          recipientAcc,
          request.amount,
          request.timestamp.getOrElse(time.getTimestamp()),
          request.feeAssetId.map(s => ByteStr.decodeBase58(s).get),
          request.fee,
          request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray)
        )
    } yield tx

  def versionedTransfer(request: VersionedTransferRequest, wallet: Wallet, time: Time): Either[ValidationError, VersionedTransferTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc     <- AddressOrAlias.fromString(request.recipient)
      tx <- VersionedTransferTransaction
        .selfSigned(
          2,
          request.assetId.map(s => ByteStr.decodeBase58(s).get),
          senderPrivateKey,
          recipientAcc,
          request.amount,
          request.timestamp.getOrElse(time.getTimestamp()),
          request.fee,
          request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray)
        )
    } yield tx

  def massTransferAsset(request: MassTransferRequest, wallet: Wallet, time: Time): Either[ValidationError, MassTransferTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      transfers        <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.selfSigned(
        request.version,
        request.assetId.map(s => ByteStr.decodeBase58(s).get),
        senderPrivateKey,
        transfers,
        request.timestamp.getOrElse(time.getTimestamp()),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray)
      )
    } yield tx

  def setScript(request: SetScriptRequest, wallet: Wallet, time: Time): Either[ValidationError, SetScriptTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      script <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase58String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.selfSigned(
        sender = senderPrivateKey,
        script = script,
        fee = request.fee,
        timestamp = request.timestamp.getOrElse(time.getTimestamp()),
      )
    } yield tx

  def smartIssue(request: SmartIssueRequest, wallet: Wallet, time: Time): Either[ValidationError, SmartIssueTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      script <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase58String(s).map(Some(_))
      }
      tx <- SmartIssueTransaction.selfSigned(
        sender = senderPrivateKey,
        script = script,
        fee = request.fee,
        timestamp = request.timestamp.getOrElse(time.getTimestamp()),
        version = 1,
        chainId = AddressScheme.current.chainId,
        name = request.name.getBytes(Charsets.UTF_8),
        description = request.description.getBytes(Charsets.UTF_8),
        quantity = request.quantity,
        decimals = request.decimals,
        reissuable = request.reissuable
      )
    } yield tx

  def issueAsset(request: IssueRequest, wallet: Wallet, time: Time): Either[ValidationError, IssueTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- IssueTransaction.create(
        senderPrivateKey,
        request.name.getBytes(Charsets.UTF_8),
        request.description.getBytes(Charsets.UTF_8),
        request.quantity,
        request.decimals,
        request.reissuable,
        request.fee,
        timestamp
      )
    } yield tx

  def lease(request: LeaseRequest, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc     <- AddressOrAlias.fromString(request.recipient)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- LeaseTransaction.create(senderPrivateKey, request.amount, request.fee, timestamp, recipientAcc)
    } yield tx

  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- LeaseCancelTransaction.create(pk, ByteStr.decodeBase58(request.txId).get, request.fee, timestamp)
    } yield tx

  def alias(request: CreateAliasRequest, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      alias            <- Alias.buildWithCurrentNetworkByte(request.alias)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- CreateAliasTransaction.create(senderPrivateKey, alias, request.fee, timestamp)
    } yield tx

  def reissueAsset(request: ReissueRequest, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- ReissueTransaction.create(pk, ByteStr.decodeBase58(request.assetId).get, request.quantity, request.reissuable, request.fee, timestamp)
    } yield tx

  def burnAsset(request: BurnRequest, wallet: Wallet, time: Time): Either[ValidationError, BurnTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- BurnTransaction.create(pk, ByteStr.decodeBase58(request.assetId).get, request.quantity, request.fee, timestamp)
    } yield tx

  def data(request: DataRequest, wallet: Wallet, time: Time): Either[ValidationError, DataTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- DataTransaction.selfSigned(request.version, pk, request.data, request.fee, timestamp)
    } yield tx
}
