package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.RequestGen
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.leasing.LeaseBroadcastApiRoute
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseTransactionV1}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Gen.posNum
import org.scalacheck.{Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json._
import play.api.libs.json._

class LeaseBroadcastRouteSpec extends RouteSpec("/leasing/broadcast/") with RequestGen with PathMockFactory with PropertyChecks {
  private val settings    = RestAPISettings.fromConfig(ConfigFactory.load())
  private val utx         = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]

  (utx.putIfNew _).when(*).onCall((t: Transaction) => Left(TransactionValidationError(GenericError("foo"), t))).anyNumberOfTimes()

  "returns StateCheckFailed" - {
    val route = LeaseBroadcastApiRoute(settings, utx, allChannels).route

    val vt = Table[String, G[_ <: Transaction], (JsValue) => JsValue](
      ("url", "generator", "transform"),
      ("lease", leaseGen.retryUntil(_.isInstanceOf[LeaseTransactionV1]), identity),
      ("cancel", leaseCancelGen.retryUntil(_.isInstanceOf[LeaseCancelTransactionV1]), {
        case o: JsObject => o ++ Json.obj("txId" -> o.value("leaseId"))
        case other       => other
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
    val route = LeaseBroadcastApiRoute(settings, utx, allChannels).route

    "lease transaction" in forAll(leaseReq) { lease =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("lease"), v) ~> route

      forAll(nonPositiveLong) { q =>
        posting(lease.copy(amount = q)) should produce(NonPositiveAmount(s"$q of waves"))
      }
      forAll(invalidBase58) { pk =>
        posting(lease.copy(senderPublicKey = pk)) should produce(InvalidAddress)
      }
      forAll(invalidBase58) { a =>
        posting(lease.copy(recipient = a)) should produce(InvalidAddress)
      }
      forAll(nonPositiveLong) { fee =>
        posting(lease.copy(fee = fee)) should produce(InsufficientFee())
      }
      forAll(posNum[Long]) { quantity =>
        posting(lease.copy(amount = quantity, fee = Long.MaxValue)) should produce(OverflowError)
      }
    }

    "lease cancel transaction" in forAll(leaseCancelReq) { cancel =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("cancel"), v) ~> route

      forAll(invalidBase58) { pk =>
        posting(cancel.copy(txId = pk)) should produce(CustomValidationError("invalid.leaseTx"))
      }
      forAll(invalidBase58) { pk =>
        posting(cancel.copy(senderPublicKey = pk)) should produce(InvalidAddress)
      }
      forAll(nonPositiveLong) { fee =>
        posting(cancel.copy(fee = fee)) should produce(InsufficientFee())
      }
    }
  }
}
