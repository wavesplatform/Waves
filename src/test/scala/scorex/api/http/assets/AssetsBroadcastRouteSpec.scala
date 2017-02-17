package scorex.api.http.assets

import com.typesafe.config.ConfigFactory
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import org.scalacheck.Gen._
import org.scalacheck.{Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import scorex.api.http._
import scorex.api.http.assets.BroadcastRequests._
import scorex.transaction.{Transaction, TransactionModule}


class AssetsBroadcastRouteSpec extends RouteSpec("/assets/broadcast/") with RequestGen with PathMockFactory with PropertyChecks {
  private val settings = RestAPISettings.fromConfig(ConfigFactory.load())

  "returns StateCheckFiled when state validation fails" - {
    val stmMock = {
      val m = mock[TransactionModule]
      (m.onNewOffchainTransaction _).expects(*).onCall { _: Transaction => false } anyNumberOfTimes()
      m
    }

    val route = AssetsBroadcastApiRoute(settings, stmMock).route

    val vt = Table[String, G[_ <: Transaction], (JsValue) => JsValue](
      ("url", "generator", "transform"),
      ("issue", issueGenerator, identity),
      ("reissue", reissueGenerator, identity),
      ("burn", burnGenerator, {
        case o: JsObject => o ++ Json.obj("quantity" -> o.value("amount"))
        case other => other
      }),
      ("transfer", transferGenerator, {
        case o: JsObject if o.value.contains("feeAsset") =>
          o ++ Json.obj("feeAssetId" -> o.value("feeAsset"), "quantity" -> o.value("amount"))
        case other => other
      })
    )

    def posting(url: String, v: JsValue) = Post(routePath(url), v) ~> route

    forAll(vt) { (url, gen, transform) =>
      forAll(gen) { t =>
        posting(url, transform(t.json)) should produce(StateCheckFailed)
      }
    }
  }

  "returns appropriate error code when validation fails for" - {
    val route = AssetsBroadcastApiRoute(settings, mock[TransactionModule]).route

    "issue transaction" in forAll(broadcastIssueReq) { ir =>
      def posting[A: Writes](v: A) = Post(routePath("issue"), v) ~> route

      forAll(nonPositiveLong) { q => posting(ir.copy(fee = q)) should produce (InsufficientFee) }
      forAll(nonPositiveLong) { q => posting(ir.copy(quantity = q)) should produce (NegativeAmount) }
      forAll(invalidDecimals) { d => posting(ir.copy(decimals = d)) should produce (TooBigArrayAllocation) }
      forAll(longDescription) { d => posting(ir.copy(description = d)) should produce (TooBigArrayAllocation) }
      forAll(invalidName) { name => posting(ir.copy(name = name)) should produce (InvalidName) }
      forAll(invalidBase58) { name => posting(ir.copy(name = name)) should produce (InvalidName) }
      forAll(nonPositiveLong) { fee => posting(ir.copy(fee = fee)) should produce (InsufficientFee) }
    }

    "reissue transaction" in forAll(broadcastReissueReq) { rr =>
      def posting[A: Writes](v: A) = Post(routePath("reissue"), v) ~> route

      // todo: invalid sender
      forAll(nonPositiveLong) { q => posting(rr.copy(quantity = q)) should produce (NegativeAmount) }
      forAll(nonPositiveLong) { fee => posting(rr.copy(fee = fee)) should produce (InsufficientFee) }
    }

    "burn transaction" in forAll(broadcastBurnReq) { br =>
      def posting[A: Writes](v: A) = Post(routePath("burn"), v) ~> route

      forAll(invalidBase58) { pk => posting(br.copy(senderPublicKey = pk)) should produce (InvalidAddress) }
      forAll(nonPositiveLong) { q => posting(br.copy(quantity = q)) should produce (NegativeAmount) }
      forAll(nonPositiveLong) { fee => posting(br.copy(fee = fee)) should produce (InsufficientFee) }
    }

    "transfer transaction" in forAll(broadcastTransferReq) { tr =>
      def posting[A: Writes](v: A) = Post(routePath("transfer"), v) ~> route

      forAll(nonPositiveLong) { q => posting(tr.copy(amount = q)) should produce (NegativeAmount) }
      forAll(invalidBase58) { pk => posting(tr.copy(senderPublicKey = pk)) should produce (InvalidAddress) }
      forAll(invalidBase58) { pk => posting(tr.copy(recipient = pk)) should produce (InvalidAddress) }
      forAll(invalidBase58) { a => posting(tr.copy(assetId = Some(a))) should produce (CustomValidationError("invalid.assetId")) }
      forAll(invalidBase58) { a => posting(tr.copy(feeAssetId = Some(a))) should produce (CustomValidationError("invalid.feeAssetId")) }
      forAll(longAttachment) { a => posting(tr.copy(attachment = Some(a))) should produce (TooBigArrayAllocation) }
      forAll(posNum[Long]) { quantity => posting(tr.copy(amount = quantity, fee = Long.MaxValue)) should produce (OverflowError) }
      forAll(nonPositiveLong) { fee => posting(tr.copy(fee = fee)) should produce (InsufficientFee) }
    }
  }
}
