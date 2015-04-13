package api.http

import play.api.libs.json.Json

object ApiError extends Enumeration {
  type APiError = Value

  val ERROR_UNKNOWN = Value(0, "unknown error")
  val ERROR_JSON = Value(1, "failed to parse json message")
  val ERROR_NO_BALANCE = Value(2, "not enough balance")
  val ERROR_NOT_YET_RELEASED = Value(3, "that feature is not yet released")

  //VALIDATION
  val ERROR_INVALID_SIGNATURE = Value(101, "invalid signature")
  val ERROR_INVALID_ADDRESS = Value(102, "invalid address")
  val ERROR_INVALID_SEED = Value(103, "invalid seed")
  val ERROR_INVALID_AMOUNT = Value(104, "invalid amount")
  val ERROR_INVALID_FEE = Value(105, "invalid fee")
  val ERROR_INVALID_SENDER = Value(106, "invalid sender")
  val ERROR_INVALID_RECIPIENT = Value(107, "invalid recipient")
  val ERROR_INVALID_PUBLIC_KEY = Value(108, "invalid public key")
  val ERROR_INVALID_NOT_NUMBER = Value(109, "argument is not a number")

  //WALLET
  val ERROR_WALLET_NO_EXISTS = Value(201, "wallet does not exist")
  val ERROR_WALLET_ADDRESS_NO_EXISTS = Value(202, "address does not exist in wallet")
  val ERROR_WALLET_LOCKED = Value(203, "wallet is locked")
  val ERROR_WALLET_ALREADY_EXISTS = Value(204, "wallet already exists")
  val ERROR_WALLET_SEED_EXPORT_FAILED = Value(205, "seed exporting failed")

  //BLOCKS
  val ERROR_BLOCK_NO_EXISTS = Value(301, "block does not exist")

  //TRANSACTIONS
  val ERROR_TRANSACTION_NO_EXISTS = Value(311, "transactions does not exist")

  def toJson(error: ApiError.Value) = Json.obj("error" -> error.id, "message" -> error.toString)

  def toJson(t: Throwable) = Json.obj("error" -> ERROR_UNKNOWN.id, "message" -> t.getMessage)
}
