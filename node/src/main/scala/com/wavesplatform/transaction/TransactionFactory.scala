package com.wavesplatform.transaction

import com.wavesplatform.account.*
import com.wavesplatform.api.http.requests.*
import com.wavesplatform.api.http.requests.CommitToGenerationRequest.given
import com.wavesplatform.api.http.requests.DataRequest.*
import com.wavesplatform.api.http.requests.InvokeExpressionRequest.*
import com.wavesplatform.api.http.requests.SponsorFeeRequest.*
import com.wavesplatform.api.http.versionReads
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.state.GenerationPeriod
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.{JsObject, JsValue}

class TransactionFactory(wallet: Wallet, time: Time, currentPeriod: Option[GenerationPeriod]) {
  def transferAsset(request: TransferRequest): Either[ValidationError, TransferTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- transferAsset(request, request.sender.get)
    } yield tx

  def transferAsset(request: TransferRequest, signerAddress: String): Either[ValidationError, TransferTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def massTransferAsset(request: MassTransferRequest): Either[ValidationError, MassTransferTransaction] =
    massTransferAsset(request, request.sender)

  def massTransferAsset(request: MassTransferRequest, signerAddress: String): Either[ValidationError, MassTransferTransaction] =
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

  def setScript(request: SetScriptRequest): Either[ValidationError, SetScriptTransaction] = setScript(request, request.sender)

  def setScript(request: SetScriptRequest, signerAddress: String): Either[ValidationError, SetScriptTransaction] =
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

  def setAssetScript(request: SetAssetScriptRequest, signerAddress: String): Either[ValidationError, SetAssetScriptTransaction] =
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

  def lease(request: LeaseRequest): Either[ValidationError, LeaseTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- lease(request, request.sender.get)
    } yield tx

  def lease(request: LeaseRequest, signerAddress: String): Either[ValidationError, LeaseTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def leaseCancel(request: LeaseCancelRequest): Either[ValidationError, LeaseCancelTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- leaseCancel(request, request.sender.get)
    } yield tx

  def leaseCancel(request: LeaseCancelRequest, signerAddress: String): Either[ValidationError, LeaseCancelTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def createAlias(request: CreateAliasRequest): Either[ValidationError, CreateAliasTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- createAlias(request, request.sender.get)
    } yield tx

  def createAlias(request: CreateAliasRequest, signerAddress: String): Either[ValidationError, CreateAliasTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def exchange(request: ExchangeRequest): Either[ValidationError, ExchangeTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- exchange(request, request.sender.get)
    } yield tx

  def exchange(request: ExchangeRequest, signerAddress: String): Either[ValidationError, ExchangeTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def issue(request: IssueRequest): Either[ValidationError, IssueTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- issue(request, request.sender.get)
    } yield tx

  def issue(request: IssueRequest, signerAddress: String): Either[ValidationError, IssueTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def reissue(request: ReissueRequest): Either[ValidationError, ReissueTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- reissue(request, request.sender.get)
    } yield tx

  def reissue(request: ReissueRequest, signerAddress: String): Either[ValidationError, ReissueTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def burn(request: BurnRequest): Either[ValidationError, BurnTransaction] =
    for {
      _  <- Either.cond(request.sender.nonEmpty, (), GenericError("invalid.sender"))
      tx <- burn(request, request.sender.get)
    } yield tx

  def burn(request: BurnRequest, signerAddress: String): Either[ValidationError, BurnTransaction] =
    for {
      _      <- Either.cond(request.sender.isDefined, (), GenericError("invalid.sender"))
      sender <- wallet.findPrivateKey(request.sender.get)
      tx     <- request.copy(timestamp = request.timestamp.orElse(Some(time.getTimestamp()))).toTxFrom(sender.publicKey)
      signer <- if (request.sender.get == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      signedTx = tx.signWith(signer.privateKey)
    } yield signedTx

  def data(request: DataRequest): Either[ValidationError, DataTransaction] = data(request, request.sender)

  def data(request: DataRequest, signerAddress: String): Either[ValidationError, DataTransaction] =
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

  def invokeScript(request: InvokeScriptRequest): Either[ValidationError, InvokeScriptTransaction] =
    invokeScript(request, request.sender)

  def invokeScript(request: InvokeScriptRequest, signerAddress: String): Either[ValidationError, InvokeScriptTransaction] =
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

  def sponsor(request: SponsorFeeRequest): Either[ValidationError, SponsorFeeTransaction] = sponsor(request, request.sender)

  def sponsor(request: SponsorFeeRequest, signerAddress: String): Either[ValidationError, SponsorFeeTransaction] =
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

  def updateAssetInfo(request: UpdateAssetInfoRequest, signerAddress: String): Either[ValidationError, UpdateAssetInfoTransaction] =
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

  def commitToGeneration(request: CommitToGenerationRequest, signerAddress: String): Either[ValidationError, CommitToGenerationTransaction] = {
    for {
      currentPeriod <- currentPeriod.toRight(ActivationError("DeterministicFinality is not yet activated"))
      defaultPeriod = currentPeriod.next
      sender <- request.sender match {
        case Some(sender) => wallet.findPrivateKey(sender)
        case None         => Left(GenericError("invalid.sender"))
      }
      signer <- wallet.findPrivateKey(signerAddress)
      tx     <- request.toTxFrom(sender.publicKey, BlsKeyPair(signer.privateKey), defaultPeriod.start, time.getTimestamp())
    } yield tx.signWith(signer.privateKey)
  }

  def parseRequestAndSign(signerAddress: String, jsv: JsObject): Either[ValidationError, Transaction] = {
    import play.api.libs.json.*

    val typeId = (jsv \ "type").as[Byte]
    (jsv \ "version").validateOpt[Byte](using versionReads) match {
      case JsError(errors) =>
        Left(GenericError(s"Wrong JSON: ${errors.mkString(", ")}"))
      case JsSuccess(value, _) =>
        val version = value getOrElse (1: Byte)
        val txJson  = jsv ++ Json.obj("version" -> version)

        try {
          (TransactionType(typeId): @unchecked) match {
            case TransactionType.Transfer           => transferAsset(txJson.as[TransferRequest], signerAddress)
            case TransactionType.CreateAlias        => createAlias(txJson.as[CreateAliasRequest], signerAddress)
            case TransactionType.Lease              => lease(txJson.as[LeaseRequest], signerAddress)
            case TransactionType.LeaseCancel        => leaseCancel(txJson.as[LeaseCancelRequest], signerAddress)
            case TransactionType.Issue              => issue(txJson.as[IssueRequest], signerAddress)
            case TransactionType.Reissue            => reissue(txJson.as[ReissueRequest], signerAddress)
            case TransactionType.Burn               => burn(txJson.as[BurnRequest], signerAddress)
            case TransactionType.MassTransfer       => massTransferAsset(txJson.as[MassTransferRequest], signerAddress)
            case TransactionType.Data               => data(txJson.as[DataRequest], signerAddress)
            case TransactionType.InvokeScript       => invokeScript(txJson.as[InvokeScriptRequest], signerAddress)
            case TransactionType.SetScript          => setScript(txJson.as[SetScriptRequest], signerAddress)
            case TransactionType.SetAssetScript     => setAssetScript(txJson.as[SetAssetScriptRequest], signerAddress)
            case TransactionType.SponsorFee         => sponsor(txJson.as[SponsorFeeRequest], signerAddress)
            case TransactionType.UpdateAssetInfo    => updateAssetInfo(txJson.as[UpdateAssetInfoRequest], signerAddress)
            case TransactionType.CommitToGeneration => commitToGeneration(txJson.as[CommitToGenerationRequest], signerAddress)
          }
        } catch {
          case _: NoSuchElementException => Left(UnsupportedTypeAndVersion(typeId, version))
          case _: MatchError             => Left(UnsupportedTransactionType)
        }
    }
  }
}

object TransactionFactory {
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

  def setScript(request: SetScriptRequest, sender: PublicKey): Either[ValidationError, SetScriptTransaction] =
    for {
      script <- request.script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.create(request.version.getOrElse(1.toByte), sender, script, request.fee, 0, Proofs.empty)
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

  def data(request: DataRequest, sender: PublicKey): Either[ValidationError, DataTransaction] =
    DataTransaction.create(request.version, sender, request.data, request.fee, request.timestamp.getOrElse(0), Proofs.empty)

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

  def fromSignedRequest(jsv: JsValue): Either[ValidationError, Transaction] = {
    import InvokeScriptRequest.*
    val chainId = (jsv \ "chainId").asOpt[Byte]
    val typeId  = (jsv \ "type").as[Byte]
    val version = (jsv \ "version").asOpt[Byte](using versionReads).getOrElse(1.toByte)

    val pf: PartialFunction[TransactionType.TransactionType, Either[ValidationError, Transaction]] = {
      case TransactionType.Transfer           => jsv.as[TransferRequest].toTx
      case TransactionType.CreateAlias        => jsv.as[CreateAliasRequest].toTx
      case TransactionType.Lease              => jsv.as[LeaseRequest].toTx
      case TransactionType.LeaseCancel        => jsv.as[LeaseCancelRequest].toTx
      case TransactionType.Issue              => jsv.as[IssueRequest].toTx
      case TransactionType.Reissue            => jsv.as[ReissueRequest].toTx
      case TransactionType.Burn               => jsv.as[BurnRequest].toTx
      case TransactionType.MassTransfer       => jsv.as[SignedMassTransferRequest].toTx
      case TransactionType.Data               => jsv.as[SignedDataRequest].toTx
      case TransactionType.InvokeScript       => jsv.as[SignedInvokeScriptRequest].toTx
      case TransactionType.SetScript          => jsv.as[SignedSetScriptRequest].toTx
      case TransactionType.SetAssetScript     => jsv.as[SignedSetAssetScriptRequest].toTx
      case TransactionType.SponsorFee         => jsv.as[SignedSponsorFeeRequest].toTx
      case TransactionType.Exchange           => jsv.as[ExchangeRequest].toTx
      case TransactionType.UpdateAssetInfo    => jsv.as[SignedUpdateAssetInfoRequest].toTx
      case TransactionType.InvokeExpression   => jsv.as[SignedInvokeExpressionRequest].toTx
      case TransactionType.CommitToGeneration => jsv.as[SignedCommitToGenerationRequest].toTx
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
}
