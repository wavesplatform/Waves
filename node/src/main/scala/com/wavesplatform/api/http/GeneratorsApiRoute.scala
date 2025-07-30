package com.wavesplatform.api.http

import com.wavesplatform.api.common.CommonGeneratorsApi
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Height
import com.wavesplatform.utils.Time
import monix.eval.Task
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.server.Route
import play.api.libs.json.{JsNumber, JsString, Json}

case class GeneratorsApiRoute(settings: RestAPISettings, api: CommonGeneratorsApi, time: Time, routeTimeout: RouteTimeout) extends ApiRoute {
  override lazy val route: Route = pathPrefix("generators" / "at") {
    (path(IntNumber) & get & optionalHeaderValueByType(Accept)) { (height, accept) =>
      routeTimeout.executeToFuture {
        Task {
          val formatNumbersAsStrings = accept.fold(false) {
            case a if a.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings) => true
            case _                                                             => false
          }

          api.generators(Height(height)).map { x =>
            val balance = if (formatNumbersAsStrings) JsString(x.balance.toString) else JsNumber(x.balance)
            Json.obj(
              "address"       -> x.address.toString,
              "balance"       -> balance,
              "transactionId" -> x.commitTxnId.toString
            )
          }
        }
      }
    }
  }
}
