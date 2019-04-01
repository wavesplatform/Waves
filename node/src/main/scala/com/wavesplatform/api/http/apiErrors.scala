package com.wavesplatform.api.http

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import play.api.libs.json.JsError

//WALLET

case object WalletNotExist extends ApiError {
  override val id: Int          = 201
  override val message: String  = "wallet does not exist"
  override val code: StatusCode = StatusCodes.NotFound
}

case object WalletAddressNotExists extends ApiError {
  override val id: Int          = 202
  override val message: String  = "private key for the public key does not exist in wallet"
  override val code: StatusCode = StatusCodes.NotFound
}

case object WalletLocked extends ApiError {
  override val id: Int          = 203
  override val message: String  = "wallet is locked"
  override val code: StatusCode = StatusCodes.UnprocessableEntity
}

case object WalletAlreadyExists extends ApiError {
  override val id: Int          = 204
  override val message: String  = "wallet already exists"
  override val code: StatusCode = StatusCodes.Conflict
}

case object WalletSeedExportFailed extends ApiError {
  override val id: Int          = 205
  override val message: String  = "seed exporting failed"
  override val code: StatusCode = StatusCodes.InternalServerError
}

//TRANSACTIONS
case object TransactionNotExists extends ApiError {
  override val id: Int          = 311
  override val message: String  = "transactions does not exist"
  override val code: StatusCode = StatusCodes.NotFound
}

case object NoBalance extends ApiError {
  override val id: Int          = 2
  override val message: String  = "not enough balance"
  override val code: StatusCode = StatusCodes.BadRequest
}

case class NegativeAmount(msg: String) extends ApiError {
  override val id: Int          = 111
  override val message: String  = s"negative amount: $msg"
  override val code: StatusCode = StatusCodes.BadRequest
}
@Deprecated //"Use InsufficientFee instead"
case object NegativeFee extends ApiError {
  override val id: Int          = 112
  override val message: String  = "negative fee"
  override val code: StatusCode = StatusCodes.BadRequest
}

case class InsufficientFee(override val message: String = "insufficient fee") extends ApiError {
  override val id: Int          = 112
  override val code: StatusCode = StatusCodes.BadRequest
}

case class WrongTransactionJson(err: JsError) extends ApiError {
  override val id: Int = 113
  override val message: String =
    err.errors.map(e => s"Validation failed for field '${e._1}', errors:${e._2}. ").mkString("\n")
  override val code: StatusCode = StatusCodes.UnprocessableEntity
}

case class NegativeMinFee(msg: String) extends ApiError {
  override val id: Int          = 114
  override val message: String  = s"negative fee per: $msg"
  override val code: StatusCode = StatusCodes.BadRequest
}

case class NonPositiveAmount(msg: String) extends ApiError {
  override val id: Int          = 115
  override val message: String  = s"non-positive amount: $msg"
  override val code: StatusCode = StatusCodes.BadRequest
}
