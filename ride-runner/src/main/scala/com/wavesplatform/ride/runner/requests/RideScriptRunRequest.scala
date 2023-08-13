package com.wavesplatform.ride.runner.requests

import com.wavesplatform.account.Address
import play.api.libs.json.*

final case class RideScriptRunRequest(address: Address, requestBody: JsObject, trace: Boolean = false, intAsString: Boolean = false) {
  val detailedLogPrefix: String = s"[hash=${requestBody.hashCode()}, $address, t=$trace, i=$intAsString, $requestBody]"
  val shortLogPrefix: String    = s"[${requestBody.hashCode()}]"
  override def toString: String = shortLogPrefix
}
