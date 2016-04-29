package scorex.api.http

import play.api.libs.json.Json

trait ApiError {
  val id: Int
  val message: String

  lazy val json = Json.obj("error" -> id, "message" -> message)
}

case object Unknown extends ApiError {
  override val id = 0
  override val message = "Error is unknown"
}


case object WrongJson extends ApiError {
  override val id = 1
  override val message = "failed to parse json message"
}

//API Auth
case object ApiKeyNotValid extends ApiError {
  override val id: Int = 2
  override val message: String = "Provided API key is not correct"
}

//VALIDATION
case object InvalidSignature extends ApiError {
  override val id = 101
  override val message = "invalid signature"
}

case object InvalidAddress extends ApiError {
  override val id = 102
  override val message = "invalid address"
}

case object InvalidSeed extends ApiError {
  override val id = 103
  override val message = "invalid seed"
}

case object InvalidAmount extends ApiError {
  override val id = 104
  override val message = "invalid amount"
}

case object InvalidFee extends ApiError {
  override val id = 105
  override val message = "invalid fee"
}

case object InvalidSender extends ApiError {
  override val id = 106
  override val message = "invalid sender"
}

case object InvalidRecipient extends ApiError {
  override val id = 107
  override val message = "invalid recipient"
}

case object InvalidPublicKey extends ApiError {
  override val id = 108
  override val message = "invalid public key"
}

case object InvalidNotNumber extends ApiError {
  override val id = 109
  override val message = "argument is not a number"
}

case object InvalidMessage extends ApiError {
  override val id = 110
  override val message = "invalid message"
}

//BLOCKS
case object BlockNotExists extends ApiError {
  override val id: Int = 301
  override val message: String = "block does not exist"
}
