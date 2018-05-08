package scorex.transaction

import com.google.common.base.Charsets
import com.wavesplatform.state.ByteStr
import scorex.account._
import scorex.api.http.DataRequest
import scorex.api.http.alias.{CreateAliasV1Request, CreateAliasV2Request}
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelV1Request, LeaseCancelV2Request, LeaseV1Request, LeaseV2Request}
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransactionV1, LeaseCancelTransactionV2, LeaseTransactionV1, LeaseTransactionV2}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.Script
import scorex.transaction.transfer._
import scorex.utils.Time
import scorex.wallet.Wallet

object TransactionFactory {

  def transferAssetV1(request: TransferV1Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransactionV1] =
    wallet.findWallet(request.sender).flatMap(transferAssetV1(request, _, time))

  def transferAssetV1(request: TransferV1Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, TransferTransactionV1] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
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
    wallet.findWallet(request.sender).flatMap(transferAssetV2(request, _, time))

  def transferAssetV2(request: TransferV2Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, TransferTransactionV2] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
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
    wallet.findWallet(request.sender).flatMap(massTransferAsset(request, _, time))

  def massTransferAsset(request: MassTransferRequest,
                        senderPrivateKey: PrivateKeyAccount,
                        time: Time): Either[ValidationError, MassTransferTransaction] =
    for {
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
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
    wallet.findWallet(request.sender).flatMap(setScript(request, _, time))

  def setScript(request: SetScriptRequest, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, SetScriptTransaction] =
    for {
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
    wallet.findWallet(request.sender).flatMap(issueAssetV2(request, _, time))

  def issueAssetV2(request: IssueV2Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, IssueTransactionV2] =
    for {
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
    wallet.findWallet(request.sender).flatMap(issueAssetV1(request, _, time))

  def issueAssetV1(request: IssueV1Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, IssueTransactionV1] =
    IssueTransactionV1.create(
      senderPrivateKey,
      request.name.getBytes(Charsets.UTF_8),
      request.description.getBytes(Charsets.UTF_8),
      request.quantity,
      request.decimals,
      request.reissuable,
      request.fee,
      request.timestamp.getOrElse(time.getTimestamp())
    )

  def leaseV1(request: LeaseV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV1] =
    wallet.findWallet(request.sender).flatMap(leaseV1(request, _, time))

  def leaseV1(request: LeaseV1Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, LeaseTransactionV1] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- LeaseTransactionV1.create(senderPrivateKey, request.amount, request.fee, timestamp, recipientAcc)
    } yield tx

  def leaseV2(request: LeaseV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV2] =
    wallet.findWallet(request.sender).flatMap(leaseV2(request, _, time))

  def leaseV2(request: LeaseV2Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, LeaseTransactionV2] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- LeaseTransactionV2.selfSigned(request.version, senderPrivateKey, request.amount, request.fee, timestamp, recipientAcc)
    } yield tx

  def leaseCancelV1(request: LeaseCancelV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV1] =
    wallet.findWallet(request.sender).flatMap(leaseCancelV1(request, _, time))

  def leaseCancelV1(request: LeaseCancelV1Request,
                    senderPrivateKey: PrivateKeyAccount,
                    time: Time): Either[ValidationError, LeaseCancelTransactionV1] =
    LeaseCancelTransactionV1.create(
      senderPrivateKey,
      ByteStr.decodeBase58(request.txId).get,
      request.fee,
      request.timestamp.getOrElse(time.getTimestamp())
    )

  def leaseCancelV2(request: LeaseCancelV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV2] =
    wallet.findWallet(request.sender).flatMap(leaseCancelV2(request, _, time))

  def leaseCancelV2(request: LeaseCancelV2Request,
                    senderPrivateKey: PrivateKeyAccount,
                    time: Time): Either[ValidationError, LeaseCancelTransactionV2] =
    LeaseCancelTransactionV2.selfSigned(
      request.version,
      AddressScheme.current.chainId,
      senderPrivateKey,
      ByteStr.decodeBase58(request.txId).get,
      request.fee,
      request.timestamp.getOrElse(time.getTimestamp())
    )

  def aliasV1(request: CreateAliasV1Request, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransactionV1] =
    wallet.findWallet(request.sender).flatMap(aliasV1(request, _, time))

  def aliasV1(request: CreateAliasV1Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, CreateAliasTransactionV1] =
    for {
      alias <- Alias.buildWithCurrentNetworkByte(request.alias)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- CreateAliasTransactionV1.create(senderPrivateKey, alias, request.fee, timestamp)
    } yield tx

  def aliasV2(request: CreateAliasV2Request, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransactionV2] =
    wallet.findWallet(request.sender).flatMap(aliasV2(request, _, time))

  def aliasV2(request: CreateAliasV2Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, CreateAliasTransactionV2] =
    for {
      alias <- Alias.buildWithCurrentNetworkByte(request.alias)
      timestamp = request.timestamp.getOrElse(time.getTimestamp())
      tx <- CreateAliasTransactionV2.selfSigned(senderPrivateKey, request.version, alias, request.fee, timestamp)
    } yield tx

  def reissueAssetV1(request: ReissueV1Request, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransactionV1] =
    wallet.findWallet(request.sender).flatMap(reissueAssetV1(request, _, time))

  def reissueAssetV1(request: ReissueV1Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, ReissueTransactionV1] =
    ReissueTransactionV1.create(
      senderPrivateKey,
      ByteStr.decodeBase58(request.assetId).get,
      request.quantity,
      request.reissuable,
      request.fee,
      request.timestamp.getOrElse(time.getTimestamp())
    )

  def reissueAssetV2(request: ReissueV2Request, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransactionV2] =
    wallet.findWallet(request.sender).flatMap(reissueAssetV2(request, _, time))

  def reissueAssetV2(request: ReissueV2Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, ReissueTransactionV2] =
    ReissueTransactionV2.selfSigned(
      request.version,
      AddressScheme.current.chainId,
      senderPrivateKey,
      ByteStr.decodeBase58(request.assetId).get,
      request.quantity,
      request.reissuable,
      request.fee,
      request.timestamp.getOrElse(time.getTimestamp())
    )

  def burnAssetV1(request: BurnV1Request, wallet: Wallet, time: Time): Either[ValidationError, BurnTransactionV1] =
    wallet.findWallet(request.sender).flatMap(burnAssetV1(request, _, time))

  def burnAssetV1(request: BurnV1Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, BurnTransactionV1] =
    BurnTransactionV1.create(
      senderPrivateKey,
      ByteStr.decodeBase58(request.assetId).get,
      request.quantity,
      request.fee,
      request.timestamp.getOrElse(time.getTimestamp())
    )

  def burnAssetV2(request: BurnV2Request, wallet: Wallet, time: Time): Either[ValidationError, BurnTransactionV2] =
    wallet.findWallet(request.sender).flatMap(burnAssetV2(request, _, time))

  def burnAssetV2(request: BurnV2Request, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, BurnTransactionV2] =
    BurnTransactionV2.selfSigned(
      request.version,
      AddressScheme.current.chainId,
      senderPrivateKey,
      ByteStr.decodeBase58(request.assetId).get,
      request.quantity,
      request.fee,
      request.timestamp.getOrElse(time.getTimestamp())
    )

  def data(request: DataRequest, wallet: Wallet, time: Time): Either[ValidationError, DataTransaction] =
    wallet.findWallet(request.sender).flatMap(data(request, _, time))

  def data(request: DataRequest, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, DataTransaction] =
    DataTransaction.selfSigned(
      request.version,
      senderPrivateKey,
      request.data,
      request.fee,
      request.timestamp.getOrElse(time.getTimestamp())
    )

  def sponsor(request: SponsorFeeRequest, wallet: Wallet, time: Time): Either[ValidationError, SponsorFeeTransaction] =
    wallet.findWallet(request.sender).flatMap(sponsor(request, _, time))

  def sponsor(request: SponsorFeeRequest, senderPrivateKey: PrivateKeyAccount, time: Time): Either[ValidationError, SponsorFeeTransaction] =
    for {
      assetId <- ByteStr.decodeBase58(request.assetId).toEither.left.map(_ => GenericError(s"Wrong Base58 string: ${request.assetId}"))
      tx <- SponsorFeeTransaction.create(
        request.version,
        senderPrivateKey,
        assetId,
        request.minSponsoredAssetFee,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp())
      )
    } yield tx
}
