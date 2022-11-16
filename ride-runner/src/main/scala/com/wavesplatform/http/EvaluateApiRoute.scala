package com.wavesplatform.http

import akka.http.scaladsl.server.{PathMatcher1, Route}
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.*
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{AccountScriptInfo, Blockchain}
import play.api.libs.json.*

class EvaluateApiRoute(settings: RestAPISettings) extends ApiRoute {
  override val route: Route = pathPrefix("utils") { evaluate }

  def evaluate: Route =
    (path("script" / "evaluate" / ScriptedAddress) & jsonPostD[JsObject] & parameter("trace".as[Boolean] ? false)) {
      case ((address, accountScriptInfo), request, trace) =>
        val apiResult = UtilsApiRoute.evaluate(settings, getBlockchainFor, _ => accountScriptInfo, address, request, trace)
        complete(apiResult ++ request ++ Json.obj("address" -> address.toString))
    }

  private[this] val ScriptedAddress: PathMatcher1[(Address, AccountScriptInfo)] = AddrSegment.map { address =>
    getAccountScript(address) match {
      case Some(accountScriptInfo) => (address, accountScriptInfo)
      case None                    => throw ApiException(CustomValidationError(s"Address $address is not dApp"))
    }
  }

  private def getAccountScript(address: Address): Option[AccountScriptInfo] = ???
  private def getBlockchainFor(expr: Terms.EXPR): Blockchain                = ???
}
