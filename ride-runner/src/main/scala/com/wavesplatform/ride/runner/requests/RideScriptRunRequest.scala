package com.wavesplatform.ride.runner.requests

import cats.syntax.contravariantSemigroupal.*
import com.wavesplatform.account.Address
import play.api.libs.json.*

final case class RideScriptRunRequest(address: Address, requestBody: JsObject, trace: Boolean = false, intAsString: Boolean = false) {
  val detailedLogPrefix: String = s"[hash=${requestBody.hashCode()}, $address, t=$trace, i=$intAsString, $requestBody]"
  val shortLogPrefix: String    = s"[${requestBody.hashCode()}]"
  override def toString: String = shortLogPrefix
}

object RideScriptRunRequest {
  // Even a compiler says it is never used. But it is a lie.
  private implicit val jsonConfiguration: JsonConfiguration.Aux[Json.WithDefaultValues] = JsonConfiguration[Json.WithDefaultValues]()

  private val fallbackReads = Json.reads[RideScriptRunRequest]

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

      (address, requestBody).mapN(RideScriptRunRequest.apply(_, _, trace = false, intAsString = false)) match {
        case Left(e)  => JsError(s"Can't parse RequestKey: $e")
        case Right(r) => JsSuccess(r)
      }

    case x => fallbackReads.reads(x)
  }
}
