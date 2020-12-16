package com.wavesplatform.api.http.alias

import akka.NotUsed
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import cats.syntax.either._
import com.wavesplatform.account.Alias
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.requests.CreateAliasRequest
import com.wavesplatform.api.http.{BroadcastRoute, _}
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.{JsString, JsValue, Json}

case class AliasApiRoute(
    settings: RestAPISettings,
    commonApi: CommonTransactionsApi,
    wallet: Wallet,
    transactionPublisher: TransactionPublisher,
    time: Time,
    blockchain: Blockchain
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

  override val route: Route = pathPrefix("alias") {
    addressOfAlias ~ aliasOfAddress ~ deprecatedRoute
  }

  private def deprecatedRoute: Route =
    path("broadcast" / "create") {
      broadcast[CreateAliasRequest](_.toTx)
    } ~ (path("create") & withAuth) {
      broadcast[CreateAliasRequest](TransactionFactory.createAlias(_, wallet, time))
    }

  def addressOfAlias: Route = (get & path("by-alias" / Segment)) { aliasName =>
    complete {
      Alias
        .create(aliasName)
        .flatMap { a =>
          blockchain.resolveAlias(a).bimap(_ => TxValidationError.AliasDoesNotExist(a), addr => Json.obj("address" -> addr.stringRepr))
        }
    }
  }

  private implicit val ess: JsonEntityStreamingSupport = EntityStreamingSupport.json()

  def aliasOfAddress: Route = (get & path("by-address" / AddrSegment)) { address =>
    extractScheduler { implicit s =>
      val value: Source[JsValue, NotUsed] =
        Source.fromPublisher(commonApi.aliasesOfAddress(address).map { case (_, tx) => JsString(tx.alias.stringRepr) }.toReactivePublisher)
      complete(value)
    }
  }
}
