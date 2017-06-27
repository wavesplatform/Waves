package com.wavesplatform.http

import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.network.OffChainTransaction
import com.wavesplatform.{TestWallet, TransactionGen}
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.{ChannelHandlerContext, ChannelOutboundHandlerAdapter, ChannelPromise}
import org.scalacheck.Shrink
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, Json}
import scorex.api.http.{ApiKeyNotValid, PaymentApiRoute}
import scorex.transaction.assets.TransferTransaction
import scorex.utils.{NTP, Time}

class PaymentRouteSpec extends RouteSpec("/payment")
  with MockFactory with PropertyChecks with RestAPISettingsHelper with TestWallet with TransactionGen {


  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  "accepts payments" in {
    forAll(accountOrAliasGen.label("recipient"), positiveLongGen.label("amount"), smallFeeGen.label("fee")) {
      case (recipient, amount, fee) =>

        val timestamp = System.currentTimeMillis()
        val time = mock[Time]
        (time.getTimestamp _).expects().returns(timestamp).anyNumberOfTimes()

        val sender = testWallet.privateKeyAccounts().head
        val tx = TransferTransaction.create(None, sender, recipient, amount, timestamp, None, fee, Array())

        class MockHandler extends ChannelOutboundHandlerAdapter {
          override def write(ctx: ChannelHandlerContext, msg: scala.Any, promise: ChannelPromise): Unit = {
            msg match {
              case OffChainTransaction(_, p) =>
                p.success(tx)
            }
          }
        }

        val channel = new EmbeddedChannel(new MockHandler)
        val route = PaymentApiRoute(restAPISettings, testWallet, channel, time).route

        val req = Json.obj("sender" -> sender.address, "recipient" -> recipient.stringRepr, "amount" -> amount, "fee" -> fee)

        Post(routePath(""), req) ~> route should produce(ApiKeyNotValid)
        Post(routePath(""), req) ~> api_key(apiKey) ~> route ~> check {
          val resp = responseAs[JsObject]

          (resp \ "assetId").asOpt[String] shouldEqual None
          (resp \ "feeAsset").asOpt[String] shouldEqual None
          (resp \ "type").as[Int] shouldEqual 4
          (resp \ "fee").as[Int] shouldEqual fee
          (resp \ "amount").as[Long] shouldEqual amount
          (resp \ "timestamp").as[Long] shouldEqual tx.right.get.timestamp
          (resp \ "signature").as[String] shouldEqual tx.right.get.signature.base58
          (resp \ "sender").as[String] shouldEqual sender.address
          (resp \ "recipient").as[String] shouldEqual recipient.stringRepr
        }
    }
  }
}
