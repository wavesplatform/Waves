package com.wavesplatform.transaction

import com.wavesplatform.account.*
import com.wavesplatform.api.http.requests.*
import com.wavesplatform.api.http.requests.DataRequest.*
import com.wavesplatform.api.http.requests.InvokeExpressionRequest.*
import com.wavesplatform.api.http.requests.SponsorFeeRequest.*
import com.wavesplatform.api.http.versionReads
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{GenericError, UnsupportedTransactionType, UnsupportedTypeAndVersion, WrongChain}
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.{JsObject, JsValue}

object TransactionFactory {
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
      tx <- SetScriptTransaction.signed(
        request.version.getOrElse(1.toByte),
        sender.publicKey,
        script,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer.privateKey
      )
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
      tx <- DataTransaction.signed(
        request.version,
        sender.publicKey,
        request.data,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer.privateKey
      )
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

      tx <- InvokeScriptTransaction
        .create(
          request.version.getOrElse(1.toByte),
          sender.publicKey,
          contract,
          request.call.map(fCallPart => InvokeScriptRequest.buildFunctionCall(fCallPart)),
          request.payment,
          request.fee,
          Asset.fromCompatId(request.feeAssetId.map(s => ByteStr.decodeBase58(s).get)),
          request.timestamp.getOrElse(time.getTimestamp()),
          Proofs.empty,
          request.chainId.getOrElse(AddressScheme.current.chainId)
        )
        .map(_.signWith(signer.privateKey))
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
        request.timestamp.getOrElse(0),
        Proofs.empty,
        request.chainId.getOrElse(AddressScheme.current.chainId)
      )

    } yield tx

  def invokeExpression(request: InvokeExpressionRequest, sender: PublicKey): Either[ValidationError, InvokeExpressionTransaction] =
    for {
      expression <- Script.fromBase64String(request.expression).flatMap {
        case e: ExprScript => Right(e)
        case _             => Left(GenericError("Unexpected expression type for InvokeExpression"))
      }
      tx <- InvokeExpressionTransaction.create(
        request.version.getOrElse(1.toByte),
        sender,
        expression,
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
        .map(IssuedAsset(_))
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
        .map(IssuedAsset(_))
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

  def updateAssetInfo(
      request: UpdateAssetInfoRequest,
      wallet: Wallet,
      signerAddress: String,
      time: Time
  ): Either[ValidationError, UpdateAssetInfoTransaction] =
    for {
      sender <- request.sender match {
        case Some(sender) => wallet.findPrivateKey(sender)
        case None         => Left(GenericError("invalid.sender"))
      }
      signer <- if (request.sender.contains(signerAddress)) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
    } yield {
      tx.signWith(signer.privateKey)
    }

  def fromSignedRequest(jsv: JsValue): Either[ValidationError, Transaction] = {
    import InvokeScriptRequest.*
    val chainId = (jsv \ "chainId").asOpt[Byte]
    val typeId  = (jsv \ "type").as[Byte]
    val version = (jsv \ "version").asOpt[Byte](versionReads).getOrElse(1.toByte)

    val pf: PartialFunction[TransactionType.TransactionType, Either[ValidationError, Transaction]] = {
      case TransactionType.Transfer         => jsv.as[TransferRequest].toTx
      case TransactionType.CreateAlias      => jsv.as[CreateAliasRequest].toTx
      case TransactionType.Lease            => jsv.as[LeaseRequest].toTx
      case TransactionType.LeaseCancel      => jsv.as[LeaseCancelRequest].toTx
      case TransactionType.Issue            => jsv.as[IssueRequest].toTx
      case TransactionType.Reissue          => jsv.as[ReissueRequest].toTx
      case TransactionType.Burn             => jsv.as[BurnRequest].toTx
      case TransactionType.MassTransfer     => jsv.as[SignedMassTransferRequest].toTx
      case TransactionType.Data             => jsv.as[SignedDataRequest].toTx
      case TransactionType.InvokeScript     => jsv.as[SignedInvokeScriptRequest].toTx
      case TransactionType.SetScript        => jsv.as[SignedSetScriptRequest].toTx
      case TransactionType.SetAssetScript   => jsv.as[SignedSetAssetScriptRequest].toTx
      case TransactionType.SponsorFee       => jsv.as[SignedSponsorFeeRequest].toTx
      case TransactionType.Exchange         => jsv.as[ExchangeRequest].toTx
      case TransactionType.UpdateAssetInfo  => jsv.as[SignedUpdateAssetInfoRequest].toTx
      case TransactionType.InvokeExpression => jsv.as[SignedInvokeExpressionRequest].toTx
    }

    if (chainId.exists(_ != AddressScheme.current.chainId)) {
      Left(WrongChain(AddressScheme.current.chainId, chainId.get))
    } else
      try pf(TransactionType(typeId))
      catch {
        case _: NoSuchElementException => Left(UnsupportedTypeAndVersion(typeId, version))
        case _: MatchError             => Left(UnsupportedTransactionType)
      }
  }

  def parseRequestAndSign(wallet: Wallet, signerAddress: String, time: Time, jsv: JsObject): Either[ValidationError, Transaction] = {
    import play.api.libs.json.*

    val typeId = (jsv \ "type").as[Byte]

    (jsv \ "version").validateOpt[Byte](versionReads) match {
      case JsError(errors) =>
        Left(GenericError(s"Wrong JSON: ${errors.mkString(", ")}"))
      case JsSuccess(value, _) =>
        val version = value getOrElse (1: Byte)
        val txJson  = jsv ++ Json.obj("version" -> version)

        try
          (TransactionType(typeId): @unchecked) match {
            case TransactionType.Transfer        => TransactionFactory.transferAsset(txJson.as[TransferRequest], wallet, signerAddress, time)
            case TransactionType.CreateAlias     => TransactionFactory.createAlias(txJson.as[CreateAliasRequest], wallet, signerAddress, time)
            case TransactionType.Lease           => TransactionFactory.lease(txJson.as[LeaseRequest], wallet, signerAddress, time)
            case TransactionType.LeaseCancel     => TransactionFactory.leaseCancel(txJson.as[LeaseCancelRequest], wallet, signerAddress, time)
            case TransactionType.Issue           => TransactionFactory.issue(txJson.as[IssueRequest], wallet, signerAddress, time)
            case TransactionType.Reissue         => TransactionFactory.reissue(txJson.as[ReissueRequest], wallet, signerAddress, time)
            case TransactionType.Burn            => TransactionFactory.burn(txJson.as[BurnRequest], wallet, signerAddress, time)
            case TransactionType.MassTransfer    => TransactionFactory.massTransferAsset(txJson.as[MassTransferRequest], wallet, signerAddress, time)
            case TransactionType.Data            => TransactionFactory.data(txJson.as[DataRequest], wallet, signerAddress, time)
            case TransactionType.InvokeScript    => TransactionFactory.invokeScript(txJson.as[InvokeScriptRequest], wallet, signerAddress, time)
            case TransactionType.SetScript       => TransactionFactory.setScript(txJson.as[SetScriptRequest], wallet, signerAddress, time)
            case TransactionType.SetAssetScript  => TransactionFactory.setAssetScript(txJson.as[SetAssetScriptRequest], wallet, signerAddress, time)
            case TransactionType.SponsorFee      => TransactionFactory.sponsor(txJson.as[SponsorFeeRequest], wallet, signerAddress, time)
            case TransactionType.UpdateAssetInfo => TransactionFactory.updateAssetInfo(txJson.as[UpdateAssetInfoRequest], wallet, signerAddress, time)
          } catch {
          case _: NoSuchElementException => Left(UnsupportedTypeAndVersion(typeId, version))
          case _: MatchError             => Left(UnsupportedTransactionType)
        }
    }
  }
}
