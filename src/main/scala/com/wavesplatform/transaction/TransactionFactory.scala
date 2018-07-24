package com.wavesplatform.transaction

import com.google.common.base.Charsets
import com.wavesplatform.account._
import com.wavesplatform.api.http.DataRequest
import com.wavesplatform.api.http.alias.{CreateAliasV1Request, CreateAliasV2Request}
import com.wavesplatform.api.http.assets._
import com.wavesplatform.api.http.leasing.{LeaseCancelV1Request, LeaseCancelV2Request, LeaseV1Request, LeaseV2Request}
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseCancelTransactionV2, LeaseTransactionV1, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{Base58, Time}
import com.wavesplatform.wallet.Wallet

object TransactionFactory {

  def getSenderAndSigner(wallet: Wallet, signerAddress: String): Either[ValidationError, (PublicKeyAccount, PrivateKeyAccount)] =
    getSenderAndSigner(wallet, signerAddress, signerAddress)

  def getSenderAndSigner(wallet: Wallet,
                         senderAddress: String,
                         signerAddress: String): Either[ValidationError, (PublicKeyAccount, PrivateKeyAccount)] =
    for {
      sender <- wallet.findPrivateKey(senderAddress)
      signer <- if (senderAddress == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
    } yield (sender, signer)

  def transferAssetV1(request: TransferV1Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransactionV1] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => transferAssetV1(request, sender, signer, time)
    }

  def transferAssetV1(request: TransferV1Request,
                      sender: PublicKeyAccount,
                      signer: PrivateKeyAccount,
                      time: Time): Either[ValidationError, TransferTransactionV1] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV1.signed(
        request.assetId.map(s => ByteStr.decodeBase58(s).get),
        sender,
        recipientAcc,
        request.amount,
        request.timestamp.getOrElse(time.getTimestamp()),
        request.feeAssetId.map(s => ByteStr.decodeBase58(s).get),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def transferAssetV2(request: TransferV2Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransactionV2] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => transferAssetV2(request, sender, signer, time)
    }

  def transferAssetV2(request: TransferV2Request,
                      sender: PublicKeyAccount,
                      signer: PrivateKeyAccount,
                      time: Time): Either[ValidationError, TransferTransactionV2] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV2.signed(
        request.version,
        request.assetId.map(s => ByteStr.decodeBase58(s).get),
        sender,
        recipientAcc,
        request.amount,
        request.timestamp.getOrElse(time.getTimestamp()),
        request.feeAssetId.map(s => ByteStr.decodeBase58(s).get),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def massTransferAsset(request: MassTransferRequest, wallet: Wallet, time: Time): Either[ValidationError, MassTransferTransaction] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => massTransferAsset(request, sender, signer, time)
    }

  def massTransferAsset(request: MassTransferRequest,
                        sender: PublicKeyAccount,
                        signer: PrivateKeyAccount,
                        time: Time): Either[ValidationError, MassTransferTransaction] =
    for {
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.signed(
        request.version,
        request.assetId.map(s => ByteStr.decodeBase58(s).get),
        sender,
        transfers,
        request.timestamp.getOrElse(time.getTimestamp()),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def setScript(request: SetScriptRequest, wallet: Wallet, time: Time): Either[ValidationError, SetScriptTransaction] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => setScript(request, sender, signer, time)
    }

  def setScript(request: SetScriptRequest,
                sender: PublicKeyAccount,
                signer: PrivateKeyAccount,
                time: Time): Either[ValidationError, SetScriptTransaction] =
    for {
      script <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.signed(
        request.version,
        sender,
        script,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def issueAssetV2(request: IssueV2Request, wallet: Wallet, time: Time): Either[ValidationError, IssueTransactionV2] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => issueAssetV2(request, sender, signer, time)
    }

  def issueAssetV2(request: IssueV2Request,
                   sender: PublicKeyAccount,
                   signer: PrivateKeyAccount,
                   time: Time): Either[ValidationError, IssueTransactionV2] =
    for {
      s <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      tx <- IssueTransactionV2.signed(
        version = request.version,
        chainId = AddressScheme.current.chainId,
        sender = sender,
        name = request.name.getBytes(Charsets.UTF_8),
        description = request.description.getBytes(Charsets.UTF_8),
        quantity = request.quantity,
        decimals = request.decimals,
        reissuable = request.reissuable,
        script = s,
        fee = request.fee,
        timestamp = request.timestamp.getOrElse(time.getTimestamp()),
        signer = signer
      )
    } yield tx

  def issueAssetV1(request: IssueV1Request, wallet: Wallet, time: Time): Either[ValidationError, IssueTransactionV1] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => issueAssetV1(request, sender, signer, time)
    }

  def issueAssetV1(request: IssueV1Request,
                   sender: PublicKeyAccount,
                   signer: PrivateKeyAccount,
                   time: Time): Either[ValidationError, IssueTransactionV1] = IssueTransactionV1.signed(
    sender,
    request.name.getBytes(Charsets.UTF_8),
    request.description.getBytes(Charsets.UTF_8),
    request.quantity,
    request.decimals,
    request.reissuable,
    request.fee,
    request.timestamp.getOrElse(time.getTimestamp()),
    signer
  )

  def leaseV1(request: LeaseV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV1] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => leaseV1(request, sender, signer, time)
    }

  def leaseV1(request: LeaseV1Request, sender: PublicKeyAccount, signer: PrivateKeyAccount, time: Time): Either[ValidationError, LeaseTransactionV1] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransactionV1.signed(
        sender,
        request.amount,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        recipientAcc,
        signer
      )
    } yield tx

  def leaseV2(request: LeaseV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV2] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => leaseV2(request, sender, signer, time)
    }

  def leaseV2(request: LeaseV2Request, sender: PublicKeyAccount, signer: PrivateKeyAccount, time: Time): Either[ValidationError, LeaseTransactionV2] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransactionV2.signed(
        request.version,
        sender,
        request.amount,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        recipientAcc,
        signer
      )
    } yield tx

  def leaseCancelV1(request: LeaseCancelV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV1] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => leaseCancelV1(request, sender, signer, time)
    }

  def leaseCancelV1(request: LeaseCancelV1Request,
                    sender: PublicKeyAccount,
                    signer: PrivateKeyAccount,
                    time: Time): Either[ValidationError, LeaseCancelTransactionV1] = LeaseCancelTransactionV1.signed(
    sender,
    ByteStr.decodeBase58(request.txId).get,
    request.fee,
    request.timestamp.getOrElse(time.getTimestamp()),
    signer
  )

  def leaseCancelV2(request: LeaseCancelV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV2] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => leaseCancelV2(request, sender, signer, time)
    }

  def leaseCancelV2(request: LeaseCancelV2Request,
                    sender: PublicKeyAccount,
                    signer: PrivateKeyAccount,
                    time: Time): Either[ValidationError, LeaseCancelTransactionV2] = LeaseCancelTransactionV2.signed(
    request.version,
    AddressScheme.current.chainId,
    sender,
    ByteStr.decodeBase58(request.txId).get,
    request.fee,
    request.timestamp.getOrElse(time.getTimestamp()),
    signer
  )

  def aliasV1(request: CreateAliasV1Request, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransactionV1] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => aliasV1(request, sender, signer, time)
    }

  def aliasV1(request: CreateAliasV1Request,
              sender: PublicKeyAccount,
              signer: PrivateKeyAccount,
              time: Time): Either[ValidationError, CreateAliasTransactionV1] =
    for {
      alias <- Alias.buildWithCurrentNetworkByte(request.alias)
      tx <- CreateAliasTransactionV1.signed(
        sender,
        alias,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def aliasV2(request: CreateAliasV2Request, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransactionV2] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => aliasV2(request, sender, signer, time)
    }

  def aliasV2(request: CreateAliasV2Request,
              sender: PublicKeyAccount,
              signer: PrivateKeyAccount,
              time: Time): Either[ValidationError, CreateAliasTransactionV2] =
    for {
      alias <- Alias.buildWithCurrentNetworkByte(request.alias)
      tx <- CreateAliasTransactionV2.signed(
        sender,
        request.version,
        alias,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def reissueAssetV1(request: ReissueV1Request, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransactionV1] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => reissueAssetV1(request, sender, signer, time)
    }

  def reissueAssetV1(request: ReissueV1Request,
                     sender: PublicKeyAccount,
                     signer: PrivateKeyAccount,
                     time: Time): Either[ValidationError, ReissueTransactionV1] = ReissueTransactionV1.signed(
    sender,
    ByteStr.decodeBase58(request.assetId).get,
    request.quantity,
    request.reissuable,
    request.fee,
    request.timestamp.getOrElse(time.getTimestamp()),
    signer
  )

  def reissueAssetV2(request: ReissueV2Request, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransactionV2] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => reissueAssetV2(request, sender, signer, time)
    }

  def reissueAssetV2(request: ReissueV2Request,
                     sender: PublicKeyAccount,
                     signer: PrivateKeyAccount,
                     time: Time): Either[ValidationError, ReissueTransactionV2] = ReissueTransactionV2.signed(
    request.version,
    AddressScheme.current.chainId,
    sender,
    ByteStr.decodeBase58(request.assetId).get,
    request.quantity,
    request.reissuable,
    request.fee,
    request.timestamp.getOrElse(time.getTimestamp()),
    signer
  )

  def burnAssetV1(request: BurnV1Request, wallet: Wallet, time: Time): Either[ValidationError, BurnTransactionV1] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => burnAssetV1(request, sender, signer, time)
    }

  def burnAssetV1(request: BurnV1Request,
                  sender: PublicKeyAccount,
                  signer: PrivateKeyAccount,
                  time: Time): Either[ValidationError, BurnTransactionV1] = BurnTransactionV1.signed(
    sender,
    ByteStr.decodeBase58(request.assetId).get,
    request.quantity,
    request.fee,
    request.timestamp.getOrElse(time.getTimestamp()),
    signer
  )

  def burnAssetV2(request: BurnV2Request, wallet: Wallet, time: Time): Either[ValidationError, BurnTransactionV2] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => burnAssetV2(request, sender, signer, time)
    }

  def burnAssetV2(request: BurnV2Request,
                  sender: PublicKeyAccount,
                  signer: PrivateKeyAccount,
                  time: Time): Either[ValidationError, BurnTransactionV2] = BurnTransactionV2.signed(
    request.version,
    AddressScheme.current.chainId,
    sender,
    ByteStr.decodeBase58(request.assetId).get,
    request.quantity,
    request.fee,
    request.timestamp.getOrElse(time.getTimestamp()),
    signer
  )

  def data(request: DataRequest, wallet: Wallet, time: Time): Either[ValidationError, DataTransaction] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => data(request, sender, signer, time)
    }

  def data(request: DataRequest, sender: PublicKeyAccount, signer: PrivateKeyAccount, time: Time): Either[ValidationError, DataTransaction] =
    DataTransaction.signed(
      request.version,
      sender,
      request.data,
      request.fee,
      request.timestamp.getOrElse(time.getTimestamp()),
      signer
    )

  def sponsor(request: SponsorFeeRequest, wallet: Wallet, time: Time): Either[ValidationError, SponsorFeeTransaction] =
    getSenderAndSigner(wallet, request.sender).flatMap {
      case (sender, signer) => sponsor(request, sender, signer, time)
    }

  def sponsor(request: SponsorFeeRequest,
              sender: PublicKeyAccount,
              signer: PrivateKeyAccount,
              time: Time): Either[ValidationError, SponsorFeeTransaction] =
    for {
      assetId <- ByteStr.decodeBase58(request.assetId).toEither.left.map(_ => GenericError(s"Wrong Base58 string: ${request.assetId}"))
      tx <- SponsorFeeTransaction.signed(
        request.version,
        sender,
        assetId,
        request.minSponsoredAssetFee,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx
}
