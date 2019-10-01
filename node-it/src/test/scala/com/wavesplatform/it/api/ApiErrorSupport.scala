package com.wavesplatform.it.api

import com.wavesplatform.api.http.ApiError
import com.wavesplatform.api.http.ApiError._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

trait ApiErrorSupport {

  private implicit val apiErrorReads: Reads[ApiError] = new Reads[ApiError] {
    override def reads(json: JsValue): JsResult[ApiError] =
      (json \ "error").validate[Int] match {
        // API Auth
        case JsSuccess(Unknown.id, _) => JsSuccess(Unknown)
        case JsSuccess(WrongJson.Id, _) =>
          json.validate[WrongJson](
            (
              // format: off
              (JsPath \ "message").read[String](verifying[String](_ == WrongJson.Message)) and
              (JsPath \ "cause").readNullable[String].map(cause => cause.map(new Throwable(_))) and
              (JsPath \ "validationErrors").read[String]
              // format: on
            ) { (_, cause, _) =>
              WrongJson(cause)
            }
          )
        case JsSuccess(ApiKeyNotValid.id, _)        => JsSuccess(ApiKeyNotValid)
        case JsSuccess(DiscontinuedApi.id, _)       => JsSuccess(DiscontinuedApi)
        case JsSuccess(TooBigArrayAllocation.id, _) => JsSuccess(TooBigArrayAllocation)

        // Validation
        case JsSuccess(InvalidSignature.id, _) => JsSuccess(InvalidSignature)
        case JsSuccess(InvalidAddress.id, _)   => JsSuccess(InvalidAddress)
        case JsSuccess(InvalidSeed.id, _)      => JsSuccess(InvalidSeed)
        case JsSuccess(InvalidAmount.id, _)    => JsSuccess(InvalidAmount)
        case JsSuccess(InvalidFee.id, _)       => JsSuccess(InvalidFee)
        case JsSuccess(InvalidSender.id, _)    => JsSuccess(InvalidSender)
        case JsSuccess(InvalidRecipient.id, _) => JsSuccess(InvalidRecipient)
        case JsSuccess(InvalidPublicKey.id, _) => JsSuccess(InvalidPublicKey)
        case JsSuccess(InvalidNotNumber.id, _) => JsSuccess(InvalidNotNumber)
        case JsSuccess(InvalidMessage.id, _)   => JsSuccess(InvalidMessage)
        case JsSuccess(InvalidName.id, _)      => JsSuccess(InvalidName)
        case JsSuccess(StateCheckFailed.Id, _) =>
          json.validate[StateCheckFailed](
            (
              // format: off
              (JsPath \ "message").read[String].map(_.replaceFirst(StateCheckFailed.MessagePrefix, "").trim) and
              (JsPath \ "tx").read[String]
              // format: on
            ) { (message, tx) =>
              ???
            }
          )
        case JsSuccess(OverflowError.id, _)           => JsSuccess(OverflowError)
        case JsSuccess(ToSelfError.id, _)             => JsSuccess(ToSelfError)
        case JsSuccess(MissingSenderPrivateKey.id, _) => JsSuccess(MissingSenderPrivateKey)
        case JsSuccess(CustomValidationError.Id, _) =>
          json.validate[CustomValidationError](
            (JsPath \ "message").read[String].map(CustomValidationError.apply)
          )
        case JsSuccess(BlockDoesNotExist.id, _) => JsSuccess(BlockDoesNotExist)
        case JsSuccess(AliasDoesNotExist.Id, _) =>
          ???
        case JsSuccess(Mistiming.Id, _) =>
          ???
        case JsSuccess(DataKeyDoesNotExist.id, _) => JsSuccess(DataKeyDoesNotExist)
        case JsSuccess(ScriptCompilerError.Id, _) =>
          ???
        case JsSuccess(ScriptExecutionError.Id, _) =>
          ???
        case JsSuccess(TransactionNotAllowedByAccountScript.Id, _) =>
          ???
        case JsSuccess(TransactionNotAllowedByAssetScript.Id, _) =>
          ???
        case JsSuccess(SignatureError.Id, _) =>
          ???
        case JsSuccess(WalletNotExist.id, _)            => JsSuccess(WalletNotExist)
        case JsSuccess(WalletAddressDoesNotExist.id, _) => JsSuccess(WalletAddressDoesNotExist)
        case JsSuccess(WalletLocked.id, _)              => JsSuccess(WalletLocked)
        case JsSuccess(WalletAlreadyExists.id, _)       => JsSuccess(WalletAlreadyExists)
        case JsSuccess(WalletSeedExportFailed.id, _)    => JsSuccess(WalletSeedExportFailed)

        // Transactions
        case JsSuccess(TransactionDoesNotExist.id, _)    => JsSuccess(TransactionDoesNotExist)
        case JsSuccess(UnsupportedTransactionType.id, _) => JsSuccess(UnsupportedTransactionType)
        case JsSuccess(NoBalance.id, _)                  => JsSuccess(NoBalance)
        case JsSuccess(NegativeAmount.Id, _) =>
          ???
        case JsSuccess(InsufficientFee.Id, _) =>
          ???
        case JsSuccess(WrongTransactionJson.Id, _) =>
          ???
        case JsSuccess(NegativeMinFee.Id, _) =>
          ???
        case JsSuccess(NonPositiveAmount.Id, _) =>
          ???

        case JsError(errors) =>
          ???
        case _ =>
          ???
      }
  }
}
