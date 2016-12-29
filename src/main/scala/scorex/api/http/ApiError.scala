package scorex.api.http

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import play.api.libs.json.Json

case class ApiErrorResponse(error: Int, message: String)

trait ApiError {
  val id: Int
  val message: String
  val code: StatusCode

  lazy val json = Json.obj("error" -> id, "message" -> message)
  lazy val response = JsonResponse(json, code)
}

case object Unknown extends ApiError {
  override val id = 0
  override val code = StatusCodes.InternalServerError
  override val message = "Error is unknown"
}

case object WrongJson extends ApiError {
  override val id = 1
  override val code = StatusCodes.BadRequest
  override val message = "failed to parse json message"
}

//API Auth
case object ApiKeyNotValid extends ApiError {
  override val id = 2
  override val code = StatusCodes.Forbidden
  override val message: String = "Provided API key is not correct"
}

case object TooBigArrayAllocation extends ApiError {
  override val id: Int = 10
  override val message: String = "Too big sequences requested"
  override val code: StatusCode = StatusCodes.BadRequest
}


//VALIDATION
case object InvalidSignature extends ApiError {
  override val id = 101
  override val code = StatusCodes.BadRequest
  override val message = "invalid signature"
}

case object InvalidAddress extends ApiError {
  override val id = 102
  override val code = StatusCodes.BadRequest
  override val message = "invalid address"
}

case object InvalidSeed extends ApiError {
  override val id = 103
  override val code = StatusCodes.BadRequest
  override val message = "invalid seed"
}

case object InvalidAmount extends ApiError {
  override val id = 104
  override val code = StatusCodes.BadRequest
  override val message = "invalid amount"
}

case object InvalidFee extends ApiError {
  override val id = 105
  override val code = StatusCodes.BadRequest
  override val message = "invalid fee"
}

case object InvalidSender extends ApiError {
  override val id = 106
  override val code = StatusCodes.BadRequest
  override val message = "invalid sender"
}

case object InvalidRecipient extends ApiError {
  override val id = 107
  override val code = StatusCodes.BadRequest
  override val message = "invalid recipient"
}

case object InvalidPublicKey extends ApiError {
  override val id = 108
  override val code = StatusCodes.BadRequest
  override val message = "invalid public key"
}

case object InvalidNotNumber extends ApiError {
  override val id = 109
  override val code = StatusCodes.BadRequest
  override val message = "argument is not a number"
}

case object InvalidMessage extends ApiError {
  override val id = 110
  override val code = StatusCodes.BadRequest
  override val message = "invalid message"
}

case object InvalidName extends ApiError {
  override val id: Int = 111
  override val message: String = "invalid name"
  override val code: StatusCode = StatusCodes.BadRequest
}

case object StateCheckFailed extends ApiError {
  override val id: Int = 112
  override val message: String = "State check failed"
  override val code: StatusCode = StatusCodes.BadRequest
}

//BLOCKS
case object BlockNotExists extends ApiError {
  override val id: Int = 301
  override val code = StatusCodes.NotFound
  override val message: String = "block does not exist"
}

