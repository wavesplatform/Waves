package com.wavesplatform.transaction

import com.wavesplatform.account.*
import com.wavesplatform.api.http.requests.*
import com.wavesplatform.api.http.requests.CommitToGenerationRequest.given
import com.wavesplatform.api.http.requests.InvokeExpressionRequest.*
import com.wavesplatform.api.http.requests.SponsorFeeRequest.*
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.*

object TransactionFactory {
  def parseRequestAndSign(
      request: JsObject,
      wallet: Wallet,
      signer: Option[String | KeyPair],
      generationPeriodStart: => Option[Int]
  ): Either[ValidationError, Transaction] = {
    val signerE = signer
      .fold((request \ "sender").asOpt[String].toRight(GenericError("invalid.sender")).flatMap(s => wallet.findPrivateKey(s).map(_.privateKey))) {
        case signerAddress: String => wallet.findPrivateKey(signerAddress).map(_.privateKey)
        case signerKP: KeyPair     => Right(signerKP.privateKey)
      }

    val extendedRequest = if ((request \ "type").as[Int] == TransactionType.CommitToGeneration.id) {
      for {
        signer <- signerE
        periodStart <- ((request \ "generationPeriodStart").asOpt[Int] orElse generationPeriodStart)
          .toRight(GenericError("missing generation period start"))
      } yield {
        val endorserKP = BlsKeyPair(signer)
        Json.obj(
          "commitmentSignature"   -> CommitToGenerationTransaction.mkPopSignature(endorserKP, Height(periodStart)).base58,
          "generationPeriodStart" -> periodStart,
          "endorserPublicKey"     -> endorserKP.publicKey.base58
        ) ++ request
      }
    } else Right(request)

    for {
      req    <- extendedRequest
      tx     <- parseRequest(req, wallet)
      signer <- signerE
    } yield tx.signWith(signer)
  }

  def parseRequest(request: JsObject, wallet: Wallet): Either[ValidationError, Transaction & ProvenTransaction] =
    (if (!request.keys.contains("senderPublicKey")) {
       (request \ "sender").asOpt[String].fold(Left(GenericError("invalid.sender"))) { senderAddress =>
         wallet.findPrivateKey(senderAddress).map(pk => request ++ Json.obj("senderPublicKey" -> pk.publicKey.toString))
       }
     } else Right(request)).flatMap(parseRequest)

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
        case MassTransfer       => jsv.as[SignedMassTransferRequest].asRight
        case Data               => jsv.as[SignedDataRequest].asRight
        case InvokeScript       => jsv.as[SignedInvokeScriptRequest].asRight
        case SetScript          => jsv.as[SignedSetScriptRequest].asRight
        case SetAssetScript     => jsv.as[SignedSetAssetScriptRequest].asRight
        case SponsorFee         => jsv.as[SignedSponsorFeeRequest].asRight
        case UpdateAssetInfo    => jsv.as[UpdateAssetInfoRequest].asRight
        case CommitToGeneration => jsv.as[CommitToGenerationRequest].asRight
        case Exchange           => jsv.as[ExchangeRequest].asRight
        case InvokeExpression   => jsv.as[SignedInvokeExpressionRequest].asRight
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
