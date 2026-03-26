package com.wavesplatform.api.http

import com.wavesplatform.api.common.CommonGeneratorsApi
import com.wavesplatform.api.http.GeneratorsApiRoute.mkGeneratorEntryJson
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.utils.Time
import monix.eval.Task
import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.server.Route
import play.api.libs.json.{JsNumber, JsObject, JsString, Json}

case class GeneratorsApiRoute(settings: RestAPISettings, blockchain: Blockchain, api: CommonGeneratorsApi, time: Time, routeTimeout: RouteTimeout)
    extends ApiRoute {
  override lazy val route: Route = (path("generators" / "at" / IntNumber) & get & optionalHeaderValueByType(Accept)) { (height, accept) =>
    (blockchain.currentGenerationPeriod, blockchain.generationPeriodOf(Height(height))) match {
      case (Some(currGenerationPeriod), Some(reqGenerationPeriod)) if reqGenerationPeriod <= currGenerationPeriod.next =>
        routeTimeout.executeToFuture {
          Task {
            val formatNumbersAsStrings = accept.fold(false) {
              case a if a.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings) => true
              case _                                                             => false
            }

            api.generators(Height(height)).map(mkGeneratorEntryJson(_, formatNumbersAsStrings))
          }
        }

      case _ => complete(StatusCodes.NotFound, Json.arr())
    }
  }
}

object GeneratorsApiRoute {
  def mkGeneratorEntryJson(x: CommonGeneratorsApi.GeneratorEntry, formatNumbersAsStrings: Boolean): JsObject = {
    val builder = Json.newBuilder
    builder ++= Seq(
      "address"       -> x.address.toString,
      "transactionId" -> x.commitTxnId.toString
    )

    x.balance.foreach { b =>
      val v = if (formatNumbersAsStrings) JsString(b.toString) else JsNumber(b)
      builder += "balance" -> v
    }

    x.conflictHeight.foreach { h =>
      builder += "conflictHeight" -> h.toInt
    }

    builder.result()
  }
}
