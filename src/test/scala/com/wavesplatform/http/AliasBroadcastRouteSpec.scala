package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.RequestGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import org.scalacheck.{Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json._
import play.api.libs.json._
import scorex.api.http._
import scorex.api.http.alias.AliasBroadcastApiRoute
import scorex.network.ConnectedPeer
import scorex.transaction.{NewTransactionHandler, Transaction, ValidationError}


class AliasBroadcastRouteSpec extends RouteSpec("/alias/broadcast/") with RequestGen with PathMockFactory with PropertyChecks {
  private val settings = RestAPISettings.fromConfig(ConfigFactory.load())

  "returns StateCheckFiled" - {

    val stmMock = {

      def alwaysError(t: Transaction, maybePeer: Option[ConnectedPeer]): Either[ValidationError, Transaction] =
        Left[ValidationError, Transaction](scorex.transaction.ValidationError.TransactionValidationError(t, "foo"))

      val m = mock[NewTransactionHandler]
      (m.onNewOffchainTransactionExcept(_: Transaction, _: Option[ConnectedPeer]))
        .expects(*, *)
        .onCall(alwaysError _)
        .anyNumberOfTimes()
      m
    }

    val route = AliasBroadcastApiRoute(settings, stmMock).route

    def posting(url: String, v: JsValue) = Post(routePath(url), v) ~> route

    "when state validation fails" in {
      forAll(createAliasGen) { (t: Transaction) =>
          posting("create", t.json) should produce(StateCheckFailed(t, "foo"))
        }
    }
  }

  "returns appropriate error code when validation fails for" - {
    val route = AliasBroadcastApiRoute(settings, mock[NewTransactionHandler]).route

    "create alias transaction" in forAll(createAliasReq) { req =>
      import scorex.api.http.alias.SignedCreateAliasRequest.broadcastAliasRequestReadsFormat

      def posting(v: JsValue) = Post(routePath("create"), v) ~> route

      forAll(invalidBase58) { s => posting(toJson(req.copy(senderPublicKey = s))) should produce(InvalidAddress) }
      forAll(nonPositiveLong) { q => posting(toJson(req.copy(fee = q))) should produce(InsufficientFee) }
      forAll(invalidAliasStringByLength) { q =>
        val obj = toJson(req).as[JsObject] ++ Json.obj("alias" -> JsString(q))
        posting(obj) should produce(CustomValidationError(s"Alias '$q' length should be between 4 and 30"))
      }
    }
  }
}
