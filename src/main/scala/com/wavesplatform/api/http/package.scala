package com.wavesplatform.api

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http.DataRequest._
import com.wavesplatform.api.http.alias.{CreateAliasV1Request, CreateAliasV2Request}
import com.wavesplatform.api.http.assets.SponsorFeeRequest._
import com.wavesplatform.api.http.assets._
import com.wavesplatform.api.http.leasing._
import com.wavesplatform.http.ApiMarshallers
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransactionV1, ExchangeTransactionV2}
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import play.api.libs.json._

import scala.util.{Success, Try}

package object http extends ApiMarshallers {
  val versionReads: Reads[Byte] = {
    val defaultByteReads = implicitly[Reads[Byte]]
    val intToByteReads   = implicitly[Reads[Int]].map(_.toByte)
    val stringToByteReads = implicitly[Reads[String]]
      .map(s => Try(s.toByte))
      .collect(JsonValidationError("Can't parse version")) {
        case Success(v) => v
      }

    defaultByteReads orElse
      intToByteReads orElse
      stringToByteReads
  }

  def createTransaction(senderPk: String, jsv: JsObject)(txToResponse: Transaction => ToResponseMarshallable): ToResponseMarshallable = {
    val typeId = (jsv \ "type").as[Byte]

    (jsv \ "version").validateOpt[Byte](versionReads) match {
      case JsError(errors) => WrongJson(None, errors)
      case JsSuccess(value, _) =>
        val version = value.getOrElse(1: Byte)
        val txJson  = jsv ++ Json.obj("version" -> version)

        PublicKeyAccount
          .fromBase58String(senderPk)
          .flatMap { senderPk =>
            TransactionParsers.by(typeId, version) match {
              case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
              case Some(x) =>
                x match {
                  case IssueTransactionV1        => TransactionFactory.issueAssetV1(txJson.as[IssueV1Request], senderPk)
                  case IssueTransactionV2        => TransactionFactory.issueAssetV2(txJson.as[IssueV2Request], senderPk)
                  case TransferTransactionV1     => TransactionFactory.transferAssetV1(txJson.as[TransferV1Request], senderPk)
                  case TransferTransactionV2     => TransactionFactory.transferAssetV2(txJson.as[TransferV2Request], senderPk)
                  case ReissueTransactionV1      => TransactionFactory.reissueAssetV1(txJson.as[ReissueV1Request], senderPk)
                  case ReissueTransactionV2      => TransactionFactory.reissueAssetV2(txJson.as[ReissueV2Request], senderPk)
                  case BurnTransactionV1         => TransactionFactory.burnAssetV1(txJson.as[BurnV1Request], senderPk)
                  case BurnTransactionV2         => TransactionFactory.burnAssetV2(txJson.as[BurnV2Request], senderPk)
                  case MassTransferTransaction   => TransactionFactory.massTransferAsset(txJson.as[MassTransferRequest], senderPk)
                  case LeaseTransactionV1        => TransactionFactory.leaseV1(txJson.as[LeaseV1Request], senderPk)
                  case LeaseTransactionV2        => TransactionFactory.leaseV2(txJson.as[LeaseV2Request], senderPk)
                  case LeaseCancelTransactionV1  => TransactionFactory.leaseCancelV1(txJson.as[LeaseCancelV1Request], senderPk)
                  case LeaseCancelTransactionV2  => TransactionFactory.leaseCancelV2(txJson.as[LeaseCancelV2Request], senderPk)
                  case CreateAliasTransactionV1  => TransactionFactory.aliasV1(txJson.as[CreateAliasV1Request], senderPk)
                  case CreateAliasTransactionV2  => TransactionFactory.aliasV2(txJson.as[CreateAliasV2Request], senderPk)
                  case DataTransaction           => TransactionFactory.data(txJson.as[DataRequest], senderPk)
                  case InvokeScriptTransaction   => TransactionFactory.invokeScript(txJson.as[InvokeScriptRequest], senderPk)
                  case SetScriptTransaction      => TransactionFactory.setScript(txJson.as[SetScriptRequest], senderPk)
                  case SetAssetScriptTransaction => TransactionFactory.setAssetScript(txJson.as[SetAssetScriptRequest], senderPk)
                  case SponsorFeeTransaction     => TransactionFactory.sponsor(txJson.as[SponsorFeeRequest], senderPk)
                  case ExchangeTransactionV1     => TransactionFactory.exchangeV1(txJson.as[SignedExchangeRequest], senderPk)
                  case ExchangeTransactionV2     => TransactionFactory.exchangeV2(txJson.as[SignedExchangeRequestV2], senderPk)
                }
            }
          }
          .fold(ApiError.fromValidationError, txToResponse)
    }
  }

  def parseOrCreateTransaction(jsv: JsObject)(txToResponse: Transaction => ToResponseMarshallable): ToResponseMarshallable = {
    val result = TransactionFactory.fromSignedRequest(jsv)
    if (result.isRight) {
      result.fold(ApiError.fromValidationError, txToResponse)
    } else {
      createTransaction((jsv \ "senderPk").as[String], jsv)(txToResponse)
    }
  }
}
