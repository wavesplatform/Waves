package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.RequestGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Gen._
import org.scalacheck.{Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import scorex.api.http._
import scorex.api.http.assets.AssetsBroadcastApiRoute
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.Transaction


class AssetsBroadcastRouteSpec extends RouteSpec("/assets/broadcast/") with RequestGen with PathMockFactory with PropertyChecks {
  private val settings = RestAPISettings.fromConfig(ConfigFactory.load())
  private val utx = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]

  (utx.putIfNew _).when(*).onCall((t: Transaction) => Left(TransactionValidationError(GenericError("foo"), t))).anyNumberOfTimes()

  "returns StateCheckFiled" - {

    val route = AssetsBroadcastApiRoute(settings, utx, allChannels).route

    val vt = Table[String, G[_ <: Transaction], (JsValue) => JsValue](
      ("url", "generator", "transform"),
      ("issue", issueGen, identity),
      ("reissue", reissueGen, identity),
      ("burn", burnGen, {
        case o: JsObject => o ++ Json.obj("quantity" -> o.value("amount"))
        case other => other
      }),
      ("transfer", transferGen, {
        case o: JsObject if o.value.contains("feeAsset") =>
          o ++ Json.obj("feeAssetId" -> o.value("feeAsset"), "quantity" -> o.value("amount"))
        case other => other
      })
    )

    def posting(url: String, v: JsValue): RouteTestResult = Post(routePath(url), v) ~> route

    "when state validation fails" in {
      forAll(vt) { (url, gen, transform) =>
        forAll(gen) { (t: Transaction) =>
          posting(url, transform(t.json())) should produce(StateCheckFailed(t, "foo"))
        }
      }
    }
  }

  "returns appropriate error code when validation fails for" - {
    val route = AssetsBroadcastApiRoute(settings, utx, allChannels).route

    "issue transaction" in forAll(broadcastIssueReq) { ir =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("issue"), v) ~> route

      forAll(nonPositiveLong) { q => posting(ir.copy(fee = q)) should produce(InsufficientFee) }
      forAll(nonPositiveLong) { q => posting(ir.copy(quantity = q)) should produce(NegativeAmount(s"$q of assets")) }
      forAll(invalidDecimals) { d => posting(ir.copy(decimals = d)) should produce(TooBigArrayAllocation) }
      forAll(longDescription) { d => posting(ir.copy(description = d)) should produce(TooBigArrayAllocation) }
      forAll(invalidName) { name => posting(ir.copy(name = name)) should produce(InvalidName) }
      forAll(invalidBase58) { name => posting(ir.copy(name = name)) should produce(InvalidName) }
      forAll(nonPositiveLong) { fee => posting(ir.copy(fee = fee)) should produce(InsufficientFee) }
    }

    "reissue transaction" in forAll(broadcastReissueReq) { rr =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("reissue"), v) ~> route

      // todo: invalid sender
      forAll(nonPositiveLong) { q => posting(rr.copy(quantity = q)) should produce(NegativeAmount(s"$q of assets")) }
      forAll(nonPositiveLong) { fee => posting(rr.copy(fee = fee)) should produce(InsufficientFee) }
    }

    "burn transaction" in forAll(broadcastBurnReq) { br =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("burn"), v) ~> route

      forAll(invalidBase58) { pk => posting(br.copy(senderPublicKey = pk)) should produce(InvalidAddress) }
      forAll(nonPositiveLong) { q => posting(br.copy(quantity = q)) should produce(NegativeAmount(s"$q of assets")) }
      forAll(nonPositiveLong) { fee => posting(br.copy(fee = fee)) should produce(InsufficientFee) }
    }

    "transfer transaction" in forAll(broadcastTransferReq) { tr =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("transfer"), v) ~> route

      forAll(nonPositiveLong) { q => posting(tr.copy(amount = q)) should produce(NegativeAmount(s"$q of waves")) }
      forAll(invalidBase58) { pk => posting(tr.copy(senderPublicKey = pk)) should produce(InvalidAddress) }
      forAll(invalidBase58) { a => posting(tr.copy(recipient = a)) should produce(InvalidAddress) }
      forAll(invalidBase58) { a => posting(tr.copy(assetId = Some(a))) should produce(CustomValidationError("invalid.assetId")) }
      forAll(invalidBase58) { a => posting(tr.copy(feeAssetId = Some(a))) should produce(CustomValidationError("invalid.feeAssetId")) }
      forAll(longAttachment) { a => posting(tr.copy(attachment = Some(a))) should produce(CustomValidationError("invalid.attachment")) }
      forAll(posNum[Long]) { quantity => posting(tr.copy(amount = quantity, fee = Long.MaxValue)) should produce(OverflowError) }
      forAll(nonPositiveLong) { fee => posting(tr.copy(fee = fee)) should produce(InsufficientFee) }
    }
  }
}
