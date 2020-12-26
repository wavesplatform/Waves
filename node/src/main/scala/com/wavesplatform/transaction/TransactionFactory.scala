package com.wavesplatform.transaction

import com.wavesplatform.account._
import com.wavesplatform.api.http.requests.DataRequest._
import com.wavesplatform.api.http.requests.SponsorFeeRequest._
import com.wavesplatform.api.http.requests._
import com.wavesplatform.api.http.versionReads
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{GenericError, UnsupportedTypeAndVersion, WrongChain}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.{JsObject, JsValue}

object  TransactionFactory {
  def transferAsset(request: TransferRequest, wallet: Wallet, time: Time): Either[ValidationError, TransferTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- transferAsset(request, wallet, request.sender.get, time)
    } yield tx

  def transferAsset(request: TransferRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransferTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def massTransferAsset(request: MassTransferRequest, wallet: Wallet, time: Time): Either[ValidationError, MassTransferTransaction] =
    massTransferAsset(request, wallet, request.sender, time)

  def massTransferAsset(
      request: MassTransferRequest,
      wallet: Wallet,
      signerAddress: String,
      time: Time
  ): Either[ValidationError, MassTransferTransaction] =
    for {
      sender    <- wallet.findPrivateKey(request.sender)
      signer    <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.signed(
        request.version.getOrElse(1.toByte),
        sender.publicKey,
        Asset.fromCompatId(request.assetId.map(s => ByteStr.decodeBase58(s).get)),
        transfers,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        request.attachment.getOrElse(ByteStr.empty),
        signer.privateKey
      )
    } yield tx

  def massTransferAsset(request: MassTransferRequest, sender: PublicKey): Either[ValidationError, MassTransferTransaction] =
    for {
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.create(
        request.version.getOrElse(1.toByte),
        sender,
        Asset.fromCompatId(request.assetId.map(s => ByteStr.decodeBase58(s).get)),
        transfers,
        request.fee,
        0,
        request.attachment.getOrElse(ByteStr.empty),
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
      tx <- SetScriptTransaction.signed(request.version.getOrElse(1.toByte), sender.publicKey, script, request.fee, request.timestamp.getOrElse(time.getTimestamp()), signer.privateKey)
    } yield tx

  def setScript(request: SetScriptRequest, sender: PublicKey): Either[ValidationError, SetScriptTransaction] =
    for {
      script <- request.script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.create(request.version.getOrElse(1.toByte), sender, script, request.fee, 0, Proofs.empty)
    } yield tx

  def setAssetScript(
      request: SetAssetScriptRequest,
      wallet: Wallet,
      signerAddress: String,
      time: Time
  ): Either[ValidationError, SetAssetScriptTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      script <- request.script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetAssetScriptTransaction.signed(
        request.version.getOrElse(1.toByte),
        sender.publicKey,
        IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
        script,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer.privateKey
      )
    } yield tx

  def setAssetScript(request: SetAssetScriptRequest, sender: PublicKey): Either[ValidationError, SetAssetScriptTransaction] =
    for {
      script <- request.script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetAssetScriptTransaction.create(
        request.version.getOrElse(1.toByte),
        sender,
        IssuedAsset(ByteStr.decodeBase58(request.assetId).get),
        script,
        request.fee,
        request.timestamp.getOrElse(0),
        Proofs.empty
      )
    } yield tx

  def lease(request: LeaseRequest, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- lease(request, wallet, request.sender.get, time)
    } yield tx

  def lease(request: LeaseRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- leaseCancel(request, wallet, request.sender.get, time)
    } yield tx

  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseCancelTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def createAlias(request: CreateAliasRequest, wallet: Wallet, time: Time): Either[ValidationError, CreateAliasTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- createAlias(request, wallet, request.sender.get, time)
    } yield tx

  def createAlias(request: CreateAliasRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, CreateAliasTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def exchange(request: ExchangeRequest, wallet: Wallet, time: Time): Either[ValidationError, ExchangeTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- exchange(request, wallet, request.sender.get, time)
    } yield tx

  def exchange(request: ExchangeRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, ExchangeTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def issue(request: IssueRequest, wallet: Wallet, time: Time): Either[ValidationError, IssueTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- issue(request, wallet, request.sender.get, time)
    } yield tx

  def issue(request: IssueRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, IssueTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def reissue(request: ReissueRequest, wallet: Wallet, time: Time): Either[ValidationError, ReissueTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- reissue(request, wallet, request.sender.get, time)
    } yield tx

  def reissue(request: ReissueRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, ReissueTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def burn(request: BurnRequest, wallet: Wallet, time: Time): Either[ValidationError, BurnTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- burn(request, wallet, request.sender.get, time)
    } yield tx

  def burn(request: BurnRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, BurnTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def data(request: DataRequest, wallet: Wallet, time: Time): Either[ValidationError, DataTransaction] =
    data(request, wallet, request.sender, time)

  def data(request: DataRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, DataTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx     <- DataTransaction.signed(request.version, sender.publicKey, request.data, request.fee, request.timestamp.getOrElse(time.getTimestamp()), signer.privateKey)
    } yield tx

  def data(request: DataRequest, sender: PublicKey): Either[ValidationError, DataTransaction] =
    DataTransaction.create(request.version, sender, request.data, request.fee, request.timestamp.getOrElse(0), Proofs.empty)

  def invokeScript(request: InvokeScriptRequest, wallet: Wallet, time: Time): Either[ValidationError, InvokeScriptTransaction] =
    invokeScript(request, wallet, request.sender, time)

  def invokeScript(
      request: InvokeScriptRequest,
      wallet: Wallet,
      signerAddress: String,
      time: Time
  ): Either[ValidationError, InvokeScriptTransaction] =
    for {
      sender   <- wallet.findPrivateKey(request.sender)
      signer   <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      contract <- AddressOrAlias.fromString(request.dApp)

      tx <- InvokeScriptTransaction.signed(
        request.version.getOrElse(1.toByte),
        sender.publicKey,
        contract,
        request.call.map(fCallPart => InvokeScriptRequest.buildFunctionCall(fCallPart)),
        request.payment,
        request.fee,
        Asset.fromCompatId(request.feeAssetId.map(s => ByteStr.decodeBase58(s).get)),
        request.extraFeePerStep.getOrElse(InvokeScriptTransaction.DefaultExtraFeePerStep),
        request.timestamp.getOrElse(time.getTimestamp()),
        signer.privateKey
      )
    } yield tx

  def invokeScript(request: InvokeScriptRequest, sender: PublicKey): Either[ValidationError, InvokeScriptTransaction] =
    for {
      addressOrAlias <- AddressOrAlias.fromString(request.dApp)
      fcOpt = request.call.map(fCallPart => InvokeScriptRequest.buildFunctionCall(fCallPart))
      tx <- InvokeScriptTransaction.create(
        request.version.getOrElse(1.toByte),
        sender,
        addressOrAlias,
        fcOpt,
        request.payment,
        request.fee,
        Asset.fromCompatId(request.feeAssetId.map(s => ByteStr.decodeBase58(s).get)),
        request.extraFeePerStep.getOrElse(InvokeScriptTransaction.DefaultExtraFeePerStep),
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
        .map(IssuedAsset)
        .left
        .map(_ => GenericError(s"Wrong Base58 string: ${request.assetId}"))
      tx <- SponsorFeeTransaction.signed(
        request.version.getOrElse(1.toByte),
        sender.publicKey,
        assetId,
        request.minSponsoredAssetFee,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer.privateKey
      )
    } yield tx

  def sponsor(request: SponsorFeeRequest, sender: PublicKey): Either[ValidationError, SponsorFeeTransaction] =
    for {
      assetId <- ByteStr
        .decodeBase58(request.assetId)
        .toEither
        .map(IssuedAsset)
        .left
        .map(_ => GenericError(s"Wrong Base58 string: ${request.assetId}"))
      tx <- SponsorFeeTransaction.create(
        request.version.getOrElse(1.toByte),
        sender,
        assetId,
        request.minSponsoredAssetFee,
        request.fee,
        request.timestamp.getOrElse(0),
        Proofs.empty
      )
    } yield tx

  def fromSignedRequest(jsv: JsValue): Either[ValidationError, Transaction] = {
    import InvokeScriptRequest._
    val chainId = (jsv \ "chainId").asOpt[Byte]
    val typeId  = (jsv \ "type").as[Byte]
    val version = (jsv \ "version").asOpt[Byte](versionReads).getOrElse(1.toByte)

    val pf: PartialFunction[TransactionParser, Either[ValidationError, Transaction]] = {
      case TransferTransaction        => jsv.as[TransferRequest].toTx
      case CreateAliasTransaction     => jsv.as[CreateAliasRequest].toTx
      case LeaseTransaction           => jsv.as[LeaseRequest].toTx
      case LeaseCancelTransaction     => jsv.as[LeaseCancelRequest].toTx
      case IssueTransaction           => jsv.as[IssueRequest].toTx
      case ReissueTransaction         => jsv.as[ReissueRequest].toTx
      case BurnTransaction            => jsv.as[BurnRequest].toTx
      case MassTransferTransaction    => jsv.as[SignedMassTransferRequest].toTx
      case DataTransaction            => jsv.as[SignedDataRequest].toTx
      case InvokeScriptTransaction    => jsv.as[SignedInvokeScriptRequest].toTx
      case SetScriptTransaction       => jsv.as[SignedSetScriptRequest].toTx
      case SetAssetScriptTransaction  => jsv.as[SignedSetAssetScriptRequest].toTx
      case SponsorFeeTransaction      => jsv.as[SignedSponsorFeeRequest].toTx
      case ExchangeTransaction        => jsv.as[ExchangeRequest].toTx
      case UpdateAssetInfoTransaction => jsv.as[SignedUpdateAssetInfoRequest].toTx
    }

    TransactionParsers.by(typeId, version) match {
      case _ if chainId.exists(_ != AddressScheme.current.chainId) =>
        Left(WrongChain(AddressScheme.current.chainId, chainId.get))
      case Some(txType) if pf.isDefinedAt(txType) => pf(txType)
      case _                                      => Left(UnsupportedTypeAndVersion(typeId, version))
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
          case None => Left(UnsupportedTypeAndVersion(typeId, version))
          case Some(x) =>
            x match {
              case TransferTransaction       => TransactionFactory.transferAsset(txJson.as[TransferRequest], wallet, signerAddress, time)
              case CreateAliasTransaction    => TransactionFactory.createAlias(txJson.as[CreateAliasRequest], wallet, signerAddress, time)
              case LeaseTransaction          => TransactionFactory.lease(txJson.as[LeaseRequest], wallet, signerAddress, time)
              case LeaseCancelTransaction    => TransactionFactory.leaseCancel(txJson.as[LeaseCancelRequest], wallet, signerAddress, time)
              case IssueTransaction          => TransactionFactory.issue(txJson.as[IssueRequest], wallet, signerAddress, time)
              case ReissueTransaction        => TransactionFactory.reissue(txJson.as[ReissueRequest], wallet, signerAddress, time)
              case BurnTransaction           => TransactionFactory.burn(txJson.as[BurnRequest], wallet, signerAddress, time)
              case MassTransferTransaction   => TransactionFactory.massTransferAsset(txJson.as[MassTransferRequest], wallet, signerAddress, time)
              case DataTransaction           => TransactionFactory.data(txJson.as[DataRequest], wallet, signerAddress, time)
              case InvokeScriptTransaction   => TransactionFactory.invokeScript(txJson.as[InvokeScriptRequest], wallet, signerAddress, time)
              case SetScriptTransaction      => TransactionFactory.setScript(txJson.as[SetScriptRequest], wallet, signerAddress, time)
              case SetAssetScriptTransaction => TransactionFactory.setAssetScript(txJson.as[SetAssetScriptRequest], wallet, signerAddress, time)
              case SponsorFeeTransaction     => TransactionFactory.sponsor(txJson.as[SponsorFeeRequest], wallet, signerAddress, time)
              case _                         => Left(TxValidationError.UnsupportedTransactionType)
            }
        }
    }
  }
}
