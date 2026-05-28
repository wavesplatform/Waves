package com.wavesplatform.transaction

import com.wavesplatform.account.*
import com.wavesplatform.api.http.requests.*
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.*

object TransactionFactory {
  def parseRequestAndSign(request: JsObject, signer: KeyPair, generationPeriodStart: => Option[Int]): Either[ValidationError, Transaction] = {
    val overrides = Json.newBuilder
    if (!request.keys.contains("senderPublicKey")) {
      overrides += "senderPublicKey" -> signer.publicKey
    }

    val extendedRequest = if ((request \ "type").as[Int] == TransactionType.CommitToGeneration.id) {
      for {
        periodStart <- ((request \ "generationPeriodStart").asOpt[Int] orElse generationPeriodStart)
          .toRight(GenericError("missing generation period start"))
      } yield {
        val endorserKP = BlsKeyPair(signer.privateKey)
        overrides ++= Seq(
          "commitmentSignature"   -> CommitToGenerationTransaction.mkPopSignature(endorserKP, Height(periodStart)).base58,
          "generationPeriodStart" -> periodStart,
          "endorserPublicKey"     -> endorserKP.publicKey.base58
        )
        overrides.result()
      }
    } else Right(overrides.result())

    for {
      req <- extendedRequest
      tx  <- parseRequest(req ++ request)
    } yield tx.signWith(signer.privateKey)
  }

  def parseRequestAndSign(
      request: JsObject,
      wallet: Wallet,
      signer: Option[String | KeyPair],
      generationPeriodStart: => Option[Int]
  ): Either[ValidationError, Transaction] =
    signer
      .fold((request \ "sender").asOpt[String].toRight(GenericError("invalid.sender")).flatMap(wallet.findPrivateKey)) {
        case signerAddress: String => wallet.findPrivateKey(signerAddress)
        case signerKP: KeyPair     => Right(signerKP)
      }
      .flatMap(signer => parseRequestAndSign(request, signer, generationPeriodStart))

  def parseRequest(request: JsObject): Either[ValidationError, Transaction & ProvenTransaction] = {
    val overrides = Json.newBuilder
    if (!request.keys.contains("timestamp")) {
      overrides += "timestamp" -> System.currentTimeMillis()
    }
    if (!request.keys.contains("version")) {
      overrides += "version" -> 1
    }

    val jsv = overrides.result() ++ request

    val typeId  = (jsv \ "type").as[Byte]
    val version = (jsv \ "version").as[Byte]

    try {
      import TransactionType.*
      import cats.syntax.either.*
      val req = TransactionType.fromId(typeId) match {
        case Transfer           => jsv.as[TransferRequest].asRight
        case CreateAlias        => jsv.as[CreateAliasRequest].asRight
        case Lease              => jsv.as[LeaseRequest].asRight
        case LeaseCancel        => jsv.as[LeaseCancelRequest].asRight
        case Issue              => jsv.as[IssueRequest].asRight
        case Reissue            => jsv.as[ReissueRequest].asRight
        case Burn               => jsv.as[BurnRequest].asRight
        case MassTransfer       => jsv.as[MassTransferRequest].asRight
        case Data               => jsv.as[DataRequest].asRight
        case InvokeScript       => jsv.as[InvokeScriptRequest].asRight
        case SetScript          => jsv.as[SetScriptRequest].asRight
        case SetAssetScript     => jsv.as[SetAssetScriptRequest].asRight
        case SponsorFee         => jsv.as[SponsorFeeRequest].asRight
        case UpdateAssetInfo    => jsv.as[UpdateAssetInfoRequest].asRight
        case CommitToGeneration => jsv.as[CommitToGenerationRequest].asRight
        case Exchange           => jsv.as[ExchangeRequest].asRight
        case InvokeExpression   => jsv.as[InvokeExpressionRequest].asRight
        case Genesis | Payment | Ethereum | InvokeExpression =>
          UnsupportedTransactionType.asLeft[TxBroadcastRequest[Transaction & ProvenTransaction]]
      }

      for {
        r  <- req
        tx <- r.toTx
      } yield tx
    } catch {
      case _: MatchError | _: NoSuchElementException => Left(UnsupportedTypeAndVersion(typeId, version))
    }
  }
}
