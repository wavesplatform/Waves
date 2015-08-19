package scorex.app.api.http

import play.api.libs.json.Json

object ApiError extends Enumeration {
  type APiError = Value

  val Unknown = Value(0, "unknown error")
  val WrongJson = Value(1, "failed to parse json message")
  val NoBalance = Value(2, "not enough balance")
  val NotYetReleased = Value(3, "that feature is not yet released")

  //VALIDATION
  val InvalidSignature = Value(101, "invalid signature")
  val InvalidAddress = Value(102, "invalid address")
  val InvalidSeed = Value(103, "invalid seed")
  val InvalidAmount = Value(104, "invalid amount")
  val InvalidFee = Value(105, "invalid fee")
  val InvalidSender = Value(106, "invalid sender")
  val InvalidRecipient = Value(107, "invalid recipient")
  val InvalidPublicKey = Value(108, "invalid public key")
  val InvalidNotNumber = Value(109, "argument is not a number")
  val InvalidMessage = Value(110, "invalid message")
  val NegativeAmount = Value(111, "negative amount")
  val NegativeFee = Value(112, "negative fee")

  //WALLET
  val WalletNotExist = Value(201, "wallet does not exist")
  val WalletAddressNotExists = Value(202, "address does not exist in wallet")
  val WalletLocked = Value(203, "wallet is locked")
  val WalletAlreadyExists = Value(204, "wallet already exists")
  val WalletSeedExportFailed = Value(205, "seed exporting failed")

  //BLOCKS
  val BlockNotExists = Value(301, "block does not exist")

  //TRANSACTIONS
  val TransactionNotExists = Value(311, "transactions does not exist")

  def json(error: ApiError.Value) = Json.obj("error" -> error.id, "message" -> error.toString)

  def json(t: Throwable) = Json.obj("error" -> Unknown.id, "message" -> t.getMessage)
}
