package com.wavesplatform.transaction

import com.google.common.base.Charsets
import com.wavesplatform.account._
import com.wavesplatform.api.http.DataRequest._
import com.wavesplatform.api.http.alias.{CreateAliasV1Request, CreateAliasV2Request, SignedCreateAliasV1Request, SignedCreateAliasV2Request}
import com.wavesplatform.api.http.assets.SponsorFeeRequest._
import com.wavesplatform.api.http.assets._
import com.wavesplatform.api.http.leasing.{LeaseCancelV1Request, LeaseCancelV2Request, LeaseV1Request, LeaseV2Request, _}
import com.wavesplatform.api.http.{DataRequest, InvokeScriptRequest, SignedDataRequest, SignedInvokeScriptRequest, versionReads}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseCancelTransactionV2, LeaseTransactionV1, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.{JsObject, JsValue}

object TransactionFactory {

  private val EmptySignature = ByteStr(Array.fill(SignatureLength)(0: Byte))

  def transferAssetV1(request: TransferV1Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransactionV1] =
    transferAssetV1(request, wallet, request.sender, time)

  def transferAssetV1(request: TransferV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransferTransactionV1] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV1.signed(
        Asset.fromCompatId(request.assetId.map(s => ByteStr.decodeBase58(s).get)),
        sender,
        recipientAcc,
        request.amount,
        request.timestamp.getOrElse(time.getTimestamp()),
        Asset.fromCompatId(request.feeAssetId.map(s => ByteStr.decodeBase58(s).get)),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.tryDecodeWithLimit(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def transferAssetV1(request: TransferV1Request, sender: PublicKey): Either[ValidationError, TransferTransactionV1] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV1.create(
        Asset.fromCompatId(request.assetId.map(s => ByteStr.decodeBase58(s).get)),
        sender,
        recipientAcc,
        request.amount,
        0,
        Asset.fromCompatId(request.feeAssetId.map(s => ByteStr.decodeBase58(s).get)),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.tryDecodeWithLimit(_).get).getOrElse(Array.emptyByteArray),
        EmptySignature
      )
    } yield tx

  def transferAssetV2(request: TransferV2Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransactionV2] =
    transferAssetV2(request, wallet, request.sender, time)

  def transferAssetV2(request: TransferV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransferTransactionV2] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV2.signed(
        Asset.fromCompatId(request.assetId.map(s => ByteStr.decodeBase58(s).get)),
        sender,
        recipientAcc,
        request.amount,
        request.timestamp.getOrElse(time.getTimestamp()),
        Asset.fromCompatId(request.feeAssetId.map(s => ByteStr.decodeBase58(s).get)),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.tryDecodeWithLimit(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def transferAssetV2(request: TransferV2Request, sender: PublicKey): Either[ValidationError, TransferTransactionV2] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV2.create(
        Asset.fromCompatId(request.assetId.map(s => ByteStr.decodeBase58(s).get)),
        sender,
        recipientAcc,
        request.amount,
        0,
        Asset.fromCompatId(request.feeAssetId.map(s => ByteStr.decodeBase58(s).get)),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.tryDecodeWithLimit(_).get).getOrElse(Array.emptyByteArray),
        Proofs.empty
      )
    } yield tx

  def massTransferAsset(request: MassTransferRequest, wallet: Wallet, time: Time): Either[ValidationError, MassTransferTransaction] =
    massTransferAsset(request, wallet, request.sender, time)

  def massTransferAsset(request: MassTransferRequest,
                        wallet: Wallet,
                        signerAddress: String,
                        time: Time): Either[ValidationError, MassTransferTransaction] =
    for {
      sender    <- wallet.findPrivateKey(request.sender)
      signer    <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.signed(
        Asset.fromCompatId(request.assetId.map(s => ByteStr.decodeBase58(s).get)),
        sender,
        transfers,
        request.timestamp.getOrElse(time.getTimestamp()),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.tryDecodeWithLimit(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def massTransferAsset(request: MassTransferRequest, sender: PublicKey): Either[ValidationError, MassTransferTransaction] =
    for {
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.create(
        Asset.fromCompatId(request.assetId.map(s => ByteStr.decodeBase58(s).get)),
        sender,
        transfers,
        0,
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.tryDecodeWithLimit(_).get).getOrElse(Array.emptyByteArray),
        Proofs.empty
      )
    } yield tx

  def setScript(request: SetScriptRequest, wallet: Wallet, time: Time): Either[ValidationError, SetScriptTransaction] =
    setScript(request, wallet, request.sender, time)

  def setScript(request: SetScriptRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, SetScriptTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      script <- request.script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.signed(
        sender,
        script,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def setScript(request: SetScriptRequest, sender: PublicKey): Either[ValidationError, SetScriptTransaction] =
    for {
      script <- request.script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.create(
        sender,
        script,
        request.fee,
        0,
        Proofs.empty
      )
    } yield tx

  def setAssetScript(request: SetAssetScriptRequest,
                     wallet: Wallet,
                     signerAddress: String,
                     time: Time): Either[ValidationError, SetAssetScriptTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      script <- request.script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetAssetScriptTransaction.signed(
        AddressScheme.current.chainId,
        sender,
        IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
        script,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def setAssetScript(request: SetAssetScriptRequest, sender: PublicKey): Either[ValidationError, SetAssetScriptTransaction] =
    for {
      script <- request.script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetAssetScriptTransaction.create(
        AddressScheme.current.chainId,
        sender,
        IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
        script,
        request.fee,
        request.timestamp.getOrElse(0),
        Proofs.empty
      )
    } yield tx

  def issueAssetV2(request: IssueV2Request, wallet: Wallet, time: Time): Either[ValidationError, IssueTransactionV2] =
    issueAssetV2(request, wallet, request.sender, time)

  def issueAssetV2(request: IssueV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, IssueTransactionV2] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      s <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      tx <- IssueTransactionV2.signed(
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

  def issueAssetV2(request: IssueV2Request, sender: PublicKey): Either[ValidationError, IssueTransactionV2] =
    for {
      s <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      tx <- IssueTransactionV2.create(
        chainId = AddressScheme.current.chainId,
        sender = sender,
        name = request.name.getBytes(Charsets.UTF_8),
        description = request.description.getBytes(Charsets.UTF_8),
        quantity = request.quantity,
        decimals = request.decimals,
        reissuable = request.reissuable,
        script = s,
        fee = request.fee,
        timestamp = request.timestamp.getOrElse(0),
        proofs = Proofs.empty
      )
    } yield tx

  def issueAssetV1(request: IssueV1Request, wallet: Wallet, time: Time): Either[ValidationError, IssueTransactionV1] =
    issueAssetV1(request, wallet, request.sender, time)

  def issueAssetV1(request: IssueV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, IssueTransactionV1] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- IssueTransactionV1.signed(
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
    } yield tx

  def issueAssetV1(request: IssueV1Request, sender: PublicKey): Either[ValidationError, IssueTransactionV1] = IssueTransactionV1.create(
    sender,
    request.name.getBytes(Charsets.UTF_8),
    request.description.getBytes(Charsets.UTF_8),
    request.quantity,
    request.decimals,
    request.reissuable,
    request.fee,
    request.timestamp.getOrElse(0),
    EmptySignature
  )

  def leaseV1(request: LeaseV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV1] =
    leaseV1(request, wallet, request.sender, time)

  def leaseV1(request: LeaseV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseTransactionV1] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
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

  def leaseV1(request: LeaseV1Request, sender: PublicKey): Either[ValidationError, LeaseTransactionV1] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransactionV1.create(
        sender,
        request.amount,
        request.fee,
        request.timestamp.getOrElse(0),
        recipientAcc,
        EmptySignature
      )
    } yield tx

  def leaseV2(request: LeaseV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV2] =
    leaseV2(request, wallet, request.sender, time)

  def leaseV2(request: LeaseV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseTransactionV2] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransactionV2.signed(
        sender,
        request.amount,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        recipientAcc,
        signer
      )
    } yield tx

  def leaseV2(request: LeaseV2Request, sender: PublicKey): Either[ValidationError, LeaseTransactionV2] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransactionV2.create(
        sender,
        request.amount,
        request.fee,
        request.timestamp.getOrElse(0),
        recipientAcc,
        Proofs.empty
      )
    } yield tx

  def leaseCancelV1(request: LeaseCancelV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV1] =
    leaseCancelV1(request, wallet, request.sender, time)

  def leaseCancelV1(request: LeaseCancelV1Request,
                    wallet: Wallet,
                    signerAddress: String,
                    time: Time): Either[ValidationError, LeaseCancelTransactionV1] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- LeaseCancelTransactionV1.signed(
        sender,
        ByteStr.decodeBase58(request.txId).get,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def leaseCancelV1(request: LeaseCancelV1Request, sender: PublicKey): Either[ValidationError, LeaseCancelTransactionV1] =
    LeaseCancelTransactionV1.create(
      sender,
      ByteStr.decodeBase58(request.txId).get,
      request.fee,
      request.timestamp.getOrElse(0),
      EmptySignature
    )

  def leaseCancelV2(request: LeaseCancelV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV2] =
    leaseCancelV2(request, wallet, request.sender, time)

  def leaseCancelV2(request: LeaseCancelV2Request,
                    wallet: Wallet,
                    signerAddress: String,
                    time: Time): Either[ValidationError, LeaseCancelTransactionV2] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- LeaseCancelTransactionV2.signed(
        AddressScheme.current.chainId,
        sender,
        ByteStr.decodeBase58(request.txId).get,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def leaseCancelV2(request: LeaseCancelV2Request, sender: PublicKey): Either[ValidationError, LeaseCancelTransactionV2] =
    LeaseCancelTransactionV2.create(
      AddressScheme.current.chainId,
      sender,
      ByteStr.decodeBase58(request.txId).get,
      request.fee,
      request.timestamp.getOrElse(0),
      Proofs.empty
    )

  def aliasV1(request: CreateAliasV1Request, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransactionV1] =
    aliasV1(request, wallet, request.sender, time)

  def aliasV1(request: CreateAliasV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, CreateAliasTransactionV1] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      alias  <- Alias.create(request.alias)
      tx <- CreateAliasTransactionV1.signed(
        sender,
        alias,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def aliasV1(request: CreateAliasV1Request, sender: PublicKey): Either[ValidationError, CreateAliasTransactionV1] =
    for {
      alias <- Alias.create(request.alias)
      tx <- CreateAliasTransactionV1.create(
        sender,
        alias,
        request.fee,
        request.timestamp.getOrElse(0),
        EmptySignature
      )
    } yield tx

  def aliasV2(request: CreateAliasV2Request, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransactionV2] =
    aliasV2(request, wallet, request.sender, time)

  def aliasV2(request: CreateAliasV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, CreateAliasTransactionV2] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      alias  <- Alias.create(request.alias)
      tx <- CreateAliasTransactionV2.signed(
        sender,
        alias,
        request.fee,
        timestamp = request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def aliasV2(request: CreateAliasV2Request, sender: PublicKey): Either[ValidationError, CreateAliasTransactionV2] =
    for {
      alias <- Alias.create(request.alias)
      tx <- CreateAliasTransactionV2.create(
        sender,
        alias,
        request.fee,
        request.timestamp.getOrElse(0),
        Proofs.empty
      )
    } yield tx

  def reissueAssetV1(request: ReissueV1Request, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransactionV1] =
    reissueAssetV1(request, wallet, request.sender, time)

  def reissueAssetV1(request: ReissueV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, ReissueTransactionV1] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- ReissueTransactionV1.signed(
        sender,
        IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
        request.quantity,
        request.reissuable,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def reissueAssetV1(request: ReissueV1Request, sender: PublicKey): Either[ValidationError, ReissueTransactionV1] =
    ReissueTransactionV1.create(
      sender,
      IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
      request.quantity,
      request.reissuable,
      request.fee,
      request.timestamp.getOrElse(0),
      EmptySignature
    )

  def reissueAssetV2(request: ReissueV2Request, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransactionV2] =
    reissueAssetV2(request, wallet, request.sender, time)

  def reissueAssetV2(request: ReissueV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, ReissueTransactionV2] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- ReissueTransactionV2.signed(
        AddressScheme.current.chainId,
        sender,
        IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
        request.quantity,
        request.reissuable,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def reissueAssetV2(request: ReissueV2Request, sender: PublicKey): Either[ValidationError, ReissueTransactionV2] =
    ReissueTransactionV2.create(
      AddressScheme.current.chainId,
      sender,
      IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
      request.quantity,
      request.reissuable,
      request.fee,
      request.timestamp.getOrElse(0),
      Proofs.empty
    )

  def burnAssetV1(request: BurnV1Request, wallet: Wallet, time: Time): Either[ValidationError, BurnTransactionV1] =
    burnAssetV1(request, wallet, request.sender, time)

  def burnAssetV1(request: BurnV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, BurnTransactionV1] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- BurnTransactionV1.signed(
        sender,
        IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
        request.quantity,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def burnAssetV1(request: BurnV1Request, sender: PublicKey): Either[ValidationError, BurnTransactionV1] = BurnTransactionV1.create(
    sender,
    IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
    request.quantity,
    request.fee,
    request.timestamp.getOrElse(0),
    EmptySignature
  )

  def burnAssetV2(request: BurnV2Request, wallet: Wallet, time: Time): Either[ValidationError, BurnTransactionV2] =
    burnAssetV2(request, wallet, request.sender, time)

  def burnAssetV2(request: BurnV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, BurnTransactionV2] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- BurnTransactionV2.signed(
        AddressScheme.current.chainId,
        sender,
        IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
        request.quantity,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def burnAssetV2(request: BurnV2Request, sender: PublicKey): Either[ValidationError, BurnTransactionV2] = BurnTransactionV2.create(
    AddressScheme.current.chainId,
    sender,
    IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
    request.quantity,
    request.fee,
    request.timestamp.getOrElse(0),
    Proofs.empty
  )

  def data(request: DataRequest, wallet: Wallet, time: Time): Either[ValidationError, DataTransaction] =
    data(request, wallet, request.sender, time)

  def data(request: DataRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, DataTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- DataTransaction.signed(
        sender,
        request.data,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def data(request: DataRequest, sender: PublicKey): Either[ValidationError, DataTransaction] =
    DataTransaction.create(
      sender,
      request.data,
      request.fee,
      request.timestamp.getOrElse(0),
      Proofs.empty
    )

  def invokeScript(request: InvokeScriptRequest, wallet: Wallet, time: Time): Either[ValidationError, InvokeScriptTransaction] =
    invokeScript(request, wallet, request.sender, time)

  def invokeScript(request: InvokeScriptRequest,
                   wallet: Wallet,
                   signerAddress: String,
                   time: Time): Either[ValidationError, InvokeScriptTransaction] =
    for {
      sender   <- wallet.findPrivateKey(request.sender)
      signer   <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      contract <- AddressOrAlias.fromString(request.dApp)

      tx <- InvokeScriptTransaction.signed(
        sender,
        contract,
        request.call.map(fCallPart => InvokeScriptRequest.buildFunctionCall(fCallPart)),
        request.payment,
        request.fee,
        Asset.fromCompatId(request.feeAssetId.map(s => ByteStr.decodeBase58(s).get)),
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def invokeScript(request: InvokeScriptRequest, sender: PublicKey): Either[ValidationError, InvokeScriptTransaction] =
    for {
      addressOrAlias <- AddressOrAlias.fromString(request.dApp)
      fcOpt = request.call.map(fCallPart => InvokeScriptRequest.buildFunctionCall(fCallPart))
      tx <- InvokeScriptTransaction.create(
        sender,
        addressOrAlias,
        fcOpt,
        request.payment,
        request.fee,
        Asset.fromCompatId(request.feeAssetId.map(s => ByteStr.decodeBase58(s).get)),
        request.timestamp.getOrElse(0),
        Proofs.empty
      )

    } yield tx

  def sponsor(request: SponsorFeeRequest, wallet: Wallet, time: Time): Either[ValidationError, SponsorFeeTransaction] =
    sponsor(request, wallet, request.sender, time)

  def sponsor(request: SponsorFeeRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, SponsorFeeTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      assetId <- ByteStr
        .decodeBase58(request.assetId)
        .toEither
        .right
        .map(IssuedAsset)
        .left
        .map(_ => GenericError(s"Wrong Base58 string: ${request.assetId}"))
      tx <- SponsorFeeTransaction.signed(
        sender,
        assetId,
        request.minSponsoredAssetFee,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def sponsor(request: SponsorFeeRequest, sender: PublicKey): Either[ValidationError, SponsorFeeTransaction] =
    for {
      assetId <- ByteStr
        .decodeBase58(request.assetId)
        .toEither
        .right
        .map(IssuedAsset)
        .left
        .map(_ => GenericError(s"Wrong Base58 string: ${request.assetId}"))
      tx <- SponsorFeeTransaction.create(
        sender,
        assetId,
        request.minSponsoredAssetFee,
        request.fee,
        request.timestamp.getOrElse(0),
        Proofs.empty
      )
    } yield tx

  def exchangeV1(request: SignedExchangeRequest, sender: PublicKey): Either[ValidationError, ExchangeTransactionV1] = {
    def orderV1(ord: Order) = {
      import ord._
      OrderV1(senderPublicKey, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, proofs)
    }

    for {
      signature <- ByteStr.decodeBase58(request.signature).toEither.left.map(_ => GenericError(s"Wrong Base58 string: ${request.signature}"))
      tx <- ExchangeTransactionV1.create(
        orderV1(request.order1),
        orderV1(request.order2),
        request.amount,
        request.price,
        request.buyMatcherFee,
        request.sellMatcherFee,
        request.fee,
        request.timestamp,
        signature
      )
    } yield tx
  }

  def exchangeV2(request: SignedExchangeRequestV2, sender: PublicKey): Either[ValidationError, ExchangeTransactionV2] = {
    import cats.instances.either._
    import cats.instances.list._
    import cats.syntax.traverse._

    for {
      proofs <- request.proofs
        .map(str => ByteStr.decodeBase58(str).toEither.left.map(e => GenericError(s"Invalid proof: $str ($e)")))
        .sequence

      tx <- ExchangeTransactionV2.create(
        request.order1,
        request.order2,
        request.amount,
        request.price,
        request.buyMatcherFee,
        request.sellMatcherFee,
        request.fee,
        request.timestamp,
        proofs
      )
    } yield tx
  }

  def fromSignedRequest(jsv: JsValue): Either[ValidationError, Transaction] = {
    import InvokeScriptRequest._
    val chainId = (jsv \ "chainId").asOpt[Byte]
    val typeId  = (jsv \ "type").as[Byte]
    val version = (jsv \ "version").asOpt[Byte](versionReads).getOrElse(1.toByte)

    val pf: PartialFunction[TransactionParser, Either[ValidationError, Transaction]] = {
      case IssueTransactionV1        => jsv.as[SignedIssueV1Request].toTx
      case IssueTransactionV2        => jsv.as[SignedIssueV2Request].toTx
      case TransferTransactionV1     => jsv.as[SignedTransferV1Request].toTx
      case TransferTransactionV2     => jsv.as[SignedTransferV2Request].toTx
      case MassTransferTransaction   => jsv.as[SignedMassTransferRequest].toTx
      case ReissueTransactionV1      => jsv.as[SignedReissueV1Request].toTx
      case ReissueTransactionV2      => jsv.as[SignedReissueV2Request].toTx
      case BurnTransactionV1         => jsv.as[SignedBurnV1Request].toTx
      case BurnTransactionV2         => jsv.as[SignedBurnV2Request].toTx
      case LeaseTransactionV1        => jsv.as[SignedLeaseV1Request].toTx
      case LeaseTransactionV2        => jsv.as[SignedLeaseV2Request].toTx
      case LeaseCancelTransactionV1  => jsv.as[SignedLeaseCancelV1Request].toTx
      case LeaseCancelTransactionV2  => jsv.as[SignedLeaseCancelV2Request].toTx
      case CreateAliasTransactionV1  => jsv.as[SignedCreateAliasV1Request].toTx
      case CreateAliasTransactionV2  => jsv.as[SignedCreateAliasV2Request].toTx
      case DataTransaction           => jsv.as[SignedDataRequest].toTx
      case InvokeScriptTransaction   => jsv.as[SignedInvokeScriptRequest].toTx
      case SetScriptTransaction      => jsv.as[SignedSetScriptRequest].toTx
      case SetAssetScriptTransaction => jsv.as[SignedSetAssetScriptRequest].toTx
      case SponsorFeeTransaction     => jsv.as[SignedSponsorFeeRequest].toTx
      case ExchangeTransactionV1     => jsv.as[SignedExchangeRequest].toTx
      case ExchangeTransactionV2     => jsv.as[SignedExchangeRequestV2].toTx
    }

    TransactionParsers.by(typeId, version) match {
      case _ if chainId.exists(_ != AddressScheme.current.chainId) =>
        Left(GenericError(s"Invalid chain id: ${chainId.get}, expected: ${AddressScheme.current.chainId}"))
      case Some(txType) if pf.isDefinedAt(txType) => pf(txType)
      case _                                      => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
    }
  }

  def parseRequestAndSign(wallet: Wallet, signerAddress: String, time: Time, jsv: JsObject): Either[ValidationError, Transaction] = {
    import play.api.libs.json._

    val typeId = (jsv \ "type").as[Byte]

    (jsv \ "version").validateOpt[Byte](versionReads) match {
      case JsError(errors) =>
        Left(GenericError(s"Wrong JSON: ${errors.mkString(", ")}"))
      case JsSuccess(value, _) =>
        val version = value getOrElse (1: Byte)
        val txJson  = jsv ++ Json.obj("version" -> version)

        TransactionParsers.by(typeId, version) match {
          case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
          case Some(x) =>
            x match {
              case IssueTransactionV1       => TransactionFactory.issueAssetV1(txJson.as[IssueV1Request], wallet, signerAddress, time)
              case IssueTransactionV2       => TransactionFactory.issueAssetV2(txJson.as[IssueV2Request], wallet, signerAddress, time)
              case TransferTransactionV1    => TransactionFactory.transferAssetV1(txJson.as[TransferV1Request], wallet, signerAddress, time)
              case TransferTransactionV2    => TransactionFactory.transferAssetV2(txJson.as[TransferV2Request], wallet, signerAddress, time)
              case ReissueTransactionV1     => TransactionFactory.reissueAssetV1(txJson.as[ReissueV1Request], wallet, signerAddress, time)
              case ReissueTransactionV2     => TransactionFactory.reissueAssetV2(txJson.as[ReissueV2Request], wallet, signerAddress, time)
              case BurnTransactionV1        => TransactionFactory.burnAssetV1(txJson.as[BurnV1Request], wallet, signerAddress, time)
              case BurnTransactionV2        => TransactionFactory.burnAssetV2(txJson.as[BurnV2Request], wallet, signerAddress, time)
              case MassTransferTransaction  => TransactionFactory.massTransferAsset(txJson.as[MassTransferRequest], wallet, signerAddress, time)
              case LeaseTransactionV1       => TransactionFactory.leaseV1(txJson.as[LeaseV1Request], wallet, signerAddress, time)
              case LeaseTransactionV2       => TransactionFactory.leaseV2(txJson.as[LeaseV2Request], wallet, signerAddress, time)
              case LeaseCancelTransactionV1 => TransactionFactory.leaseCancelV1(txJson.as[LeaseCancelV1Request], wallet, signerAddress, time)
              case LeaseCancelTransactionV2 => TransactionFactory.leaseCancelV2(txJson.as[LeaseCancelV2Request], wallet, signerAddress, time)
              case CreateAliasTransactionV1 => TransactionFactory.aliasV1(txJson.as[CreateAliasV1Request], wallet, signerAddress, time)
              case CreateAliasTransactionV2 => TransactionFactory.aliasV2(txJson.as[CreateAliasV2Request], wallet, signerAddress, time)
              case DataTransaction          => TransactionFactory.data(txJson.as[DataRequest], wallet, signerAddress, time)
              case InvokeScriptTransaction =>
                TransactionFactory.invokeScript(txJson.as[InvokeScriptRequest], wallet, signerAddress, time)
              case SetScriptTransaction      => TransactionFactory.setScript(txJson.as[SetScriptRequest], wallet, signerAddress, time)
              case SetAssetScriptTransaction => TransactionFactory.setAssetScript(txJson.as[SetAssetScriptRequest], wallet, signerAddress, time)
              case SponsorFeeTransaction     => TransactionFactory.sponsor(txJson.as[SponsorFeeRequest], wallet, signerAddress, time)
              case _                         => Left(TxValidationError.UnsupportedTransactionType)
            }
        }
    }
  }
}
