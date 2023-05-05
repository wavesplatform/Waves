package com.wavesplatform.ride.runner.storage

import cats.syntax.contravariantSemigroupal.*
import com.wavesplatform.account.Address
import play.api.libs.json.*

final case class RideScriptRunRequest(address: Address, requestBody: JsObject) {
  val detailedLogPrefix: String = s"[hash=${requestBody.hashCode()}, $address, $requestBody]"
  val shortLogPrefix: String    = s"[${requestBody.hashCode()}]"
  override def toString: String = shortLogPrefix
}

object RideScriptRunRequest {
  implicit val rideScriptRunRequestReads: Reads[RideScriptRunRequest] = Reads {
    case JsArray(rawAddress +: rawRequestBody +: xs) if xs.isEmpty =>
      val address = rawAddress match {
        case JsString(rawAddress) => Address.fromString(rawAddress).left.map(e => s"Expected '$rawAddress' to be an address: $e")
        case x                    => Left(s"Expected a string, got: $x")
      }

      val requestBody = rawRequestBody match {
        case r: JsObject => Right(r)
        case x           => Left(s"Expected a JsObject, got: $x")
      }

      (address, requestBody).mapN(RideScriptRunRequest.apply) match {
        case Left(e)  => JsError(s"Can't parse RequestKey: $e")
        case Right(r) => JsSuccess(r)
      }

    case x => JsError(s"Expected an array with two elements, got: $x")
  }
}
