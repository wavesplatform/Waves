package com.wavesplatform.http

import com.wavesplatform.RequestGen
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.ApiError.*
import com.wavesplatform.api.http.RouteTimeout
import com.wavesplatform.api.http.leasing.LeaseApiRoute
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import com.wavesplatform.utils.{Schedulers, Time}
import com.wavesplatform.wallet.Wallet
import org.scalacheck.Gen as G
import org.scalacheck.Gen.posNum
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.*
import play.api.libs.json.Json.*

import scala.concurrent.duration.*

class LeaseBroadcastRouteSpec extends RouteSpec("/leasing/broadcast/") with RequestGen with PathMockFactory with RestAPISettingsHelper {
  private[this] val publisher = DummyTransactionPublisher.rejecting(t => TransactionValidationError(GenericError("foo"), t))
  private[this] val route = LeaseApiRoute(
    restAPISettings,
    stub[Wallet],
    stub[Blockchain],
    publisher,
    stub[Time],
    stub[CommonAccountsApi],
    new RouteTimeout(60.seconds)(Schedulers.fixedPool(1, "heavy-request-scheduler"))
  ).route
  "returns StateCheckFailed" - {

    val vt = Table[String, G[_ <: Transaction], JsValue => JsValue](
      ("url", "generator", "transform"),
      ("lease", leaseGen.retryUntil(_.version == 1), identity),
      (
        "cancel",
        leaseCancelGen.retryUntil(_.isInstanceOf[LeaseCancelTransaction]),
        {
          case o: JsObject => o ++ Json.obj("txId" -> o.value("leaseId"))
          case other       => other
        }
      )
    )

    def posting(url: String, v: JsValue): RouteTestResult = Post(routePath(url), v) ~> route

    "when state validation fails" in {
      forAll(vt) { (url, gen, transform) =>
        forAll(gen) { t: Transaction =>
          posting(url, transform(t.json())) should produce(StateCheckFailed(t, "foo"))
        }
      }
    }
  }

  "returns appropriate error code when validation fails for" - {

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
        posting(lease.copy(fee = fee)) should produce(InsufficientFee)
      }
      forAll(posNum[Long]) { quantity =>
        posting(lease.copy(amount = quantity, fee = Long.MaxValue)) should produce(OverflowError)
      }
    }

    "lease cancel transaction" in forAll(leaseCancelReq) { cancel =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("cancel"), v) ~> route

      forAll(invalidBase58) { pk =>
        posting(cancel.copy(leaseId = pk)) should produce(CustomValidationError("invalid.leaseTx"))
      }
      forAll(invalidBase58) { pk =>
        posting(cancel.copy(senderPublicKey = pk)) should produce(InvalidAddress)
      }
      forAll(nonPositiveLong) { fee =>
        posting(cancel.copy(fee = fee)) should produce(InsufficientFee)
      }
    }
  }
}
