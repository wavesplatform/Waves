package scorex.transaction

import com.google.common.base.Charsets
import com.wavesplatform.state.ByteStr
import scorex.account._
import scorex.api.http.DataRequest
import scorex.api.http.alias.{CreateAliasV1Request, CreateAliasV2Request}
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelV1Request, LeaseCancelV2Request, LeaseV1Request, LeaseV2Request}
import scorex.crypto.encode.Base58
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransactionV1, LeaseCancelTransactionV2, LeaseTransactionV1, LeaseTransactionV2}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.Script
import scorex.transaction.transfer._
import scorex.utils.Time
import scorex.wallet.Wallet

object TransactionFactory {

  def transferAssetV1(request: TransferV1Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransactionV1] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc     <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV1
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

  def transferAssetV2(request: TransferV2Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransactionV2] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc     <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV2
        .selfSigned(
          request.version,
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
        version = request.version,
        sender = senderPrivateKey,
        script = script,
        fee = request.fee,
        timestamp = request.timestamp.getOrElse(time.getTimestamp()),
      )
    } yield tx

  def issueAssetV2(request: IssueV2Request, wallet: Wallet, time: Time): Either[ValidationError, IssueTransactionV2] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      s <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase58String(s).map(Some(_))
      }
      tx <- IssueTransactionV2.selfSigned(
        sender = senderPrivateKey,
        script = s,
        fee = request.fee,
        timestamp = request.timestamp.getOrElse(time.getTimestamp()),
        version = request.version,
        chainId = AddressScheme.current.chainId,
        name = request.name.getBytes(Charsets.UTF_8),
        description = request.description.getBytes(Charsets.UTF_8),
        quantity = request.quantity,
        decimals = request.decimals,
        reissuable = request.reissuable
      )
    } yield tx

  def issueAssetV1(request: IssueV1Request, wallet: Wallet, time: Time): Either[ValidationError, IssueTransactionV1] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- IssueTransactionV1.create(
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

  def leaseV1(request: LeaseV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV1] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc     <- AddressOrAlias.fromString(request.recipient)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- LeaseTransactionV1.create(senderPrivateKey, request.amount, request.fee, timestamp, recipientAcc)
    } yield tx

  def leaseV2(request: LeaseV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV2] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc     <- AddressOrAlias.fromString(request.recipient)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- LeaseTransactionV2.selfSigned(request.version, senderPrivateKey, request.amount, request.fee, timestamp, recipientAcc)
    } yield tx

  def leaseCancelV1(request: LeaseCancelV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV1] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- LeaseCancelTransactionV1.create(pk, ByteStr.decodeBase58(request.leaseId).get, request.fee, timestamp)
    } yield tx

  def leaseCancelV2(request: LeaseCancelV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV2] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- LeaseCancelTransactionV2.selfSigned(request.version,
                                                AddressScheme.current.chainId,
                                                pk,
                                                ByteStr.decodeBase58(request.leaseId).get,
                                                request.fee,
                                                timestamp)
    } yield tx

  def aliasV1(request: CreateAliasV1Request, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransactionV1] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      alias            <- Alias.buildWithCurrentNetworkByte(request.alias)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- CreateAliasTransactionV1.create(senderPrivateKey, alias, request.fee, timestamp)
    } yield tx

  def aliasV2(request: CreateAliasV2Request, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransactionV2] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      alias            <- Alias.buildWithCurrentNetworkByte(request.alias)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- CreateAliasTransactionV2.selfSigned(senderPrivateKey, request.version, alias, request.fee, timestamp)
    } yield tx

  def reissueAssetV1(request: ReissueV1Request, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransactionV1] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- ReissueTransactionV1.create(pk, ByteStr.decodeBase58(request.assetId).get, request.quantity, request.reissuable, request.fee, timestamp)
    } yield tx

  def reissueAssetV2(request: ReissueV2Request, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransactionV2] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      chainId   = AddressScheme.current.chainId
      tx <- ReissueTransactionV2.selfSigned(request.version,
                                            chainId,
                                            pk,
                                            ByteStr.decodeBase58(request.assetId).get,
                                            request.quantity,
                                            request.reissuable,
                                            request.fee,
                                            timestamp)
    } yield tx

  def burnAssetV1(request: BurnV1Request, wallet: Wallet, time: Time): Either[ValidationError, BurnTransactionV1] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- BurnTransactionV1.create(pk, ByteStr.decodeBase58(request.assetId).get, request.quantity, request.fee, timestamp)
    } yield tx

  def burnAssetV2(request: BurnV2Request, wallet: Wallet, time: Time): Either[ValidationError, BurnTransactionV2] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- BurnTransactionV2.selfSigned(request.version,
                                         AddressScheme.current.chainId,
                                         pk,
                                         ByteStr.decodeBase58(request.assetId).get,
                                         request.quantity,
                                         request.fee,
                                         timestamp)
    } yield tx

  def data(request: DataRequest, wallet: Wallet, time: Time): Either[ValidationError, DataTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- DataTransaction.selfSigned(request.version, pk, request.data, request.fee, timestamp)
    } yield tx

  def sponsor(request: SponsorFeeRequest, wallet: Wallet, time: Time): Either[ValidationError, SponsorFeeTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      assetId   = ByteStr.decodeBase58(request.assetId).get
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- SponsorFeeTransaction.create(request.version, pk, assetId, request.minSponsoredAssetFee, request.fee, timestamp)
    } yield tx
}
