package com.wavesplatform.it

import play.api.libs.json.{Format, Json}


object RequestErrorAssert {

  case class ErrorMessage(error: Int, message: String)

  implicit val errorMessageFormat: Format[ErrorMessage] = Json.format

}
