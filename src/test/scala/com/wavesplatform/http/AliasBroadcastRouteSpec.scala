package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.RequestGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.network.OffChainTransaction
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.{ChannelHandlerContext, ChannelOutboundHandlerAdapter, ChannelPromise}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json._
import play.api.libs.json._
import scorex.api.http._
import scorex.api.http.alias.AliasBroadcastApiRoute
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.GenericError


class AliasBroadcastRouteSpec extends RouteSpec("/alias/broadcast/") with RequestGen with PathMockFactory with PropertyChecks {
  private val settings = RestAPISettings.fromConfig(ConfigFactory.load())

  "returns StateCheckFiled" - {

    class MockHandler extends ChannelOutboundHandlerAdapter {
      override def write(ctx: ChannelHandlerContext, msg: scala.Any, promise: ChannelPromise): Unit = {
        msg match {
          case OffChainTransaction(t, p) =>
            p.success(Left(TransactionValidationError(t, GenericError("foo"))))
        }
      }
    }

    val channel = new EmbeddedChannel(new MockHandler)
    val route = AliasBroadcastApiRoute(settings, channel).route

    def posting(url: String, v: JsValue): RouteTestResult = Post(routePath(url), v) ~> route

    "when state validation fails" in {
      forAll(createAliasGen) { (t: Transaction) =>
          posting("create", t.json) should produce(StateCheckFailed(t, "foo"))
        }
    }
  }

  "returns appropriate error code when validation fails for" - {
    val route = AliasBroadcastApiRoute(settings, new EmbeddedChannel).route

    "create alias transaction" in forAll(createAliasReq) { req =>
      import scorex.api.http.alias.SignedCreateAliasRequest.broadcastAliasRequestReadsFormat

      def posting(v: JsValue): RouteTestResult = Post(routePath("create"), v) ~> route

      forAll(invalidBase58) { s => posting(toJson(req.copy(senderPublicKey = s))) should produce(InvalidAddress) }
      forAll(nonPositiveLong) { q => posting(toJson(req.copy(fee = q))) should produce(InsufficientFee) }
      forAll(invalidAliasStringByLength) { q =>
        val obj = toJson(req).as[JsObject] ++ Json.obj("alias" -> JsString(q))
        posting(obj) should produce(CustomValidationError(s"Alias '$q' length should be between 4 and 30"))
      }
    }
  }
}
