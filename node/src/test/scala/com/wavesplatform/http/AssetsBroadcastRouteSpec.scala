package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.RequestGen
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.assets._
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, Proofs, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import org.scalacheck.{Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.{JsObject, JsValue, Json, Writes}

class AssetsBroadcastRouteSpec
    extends RouteSpec("/assets/broadcast/")
    with RequestGen
    with PathMockFactory
    with PropertyChecks
    with RestAPISettingsHelper {

  private[this] val route = AssetsApiRoute(
    restAPISettings,
    stub[Wallet],
    DummyUtxPoolSynchronizer.rejecting(tx => TransactionValidationError(GenericError("foo"), tx)),
    stub[Blockchain],
    stub[Time]
  ).route

  "returns StateCheckFailed" - {
    val vt = Table[String, G[_ <: Transaction], JsValue => JsValue](
      ("url", "generator", "transform"),
      ("issue", issueGen.retryUntil(_.version == 1), identity),
      ("reissue", reissueGen.retryUntil(_.version == 1), identity),
      ("burn", burnGen.retryUntil(_.version == 1), {
        case o: JsObject => o ++ Json.obj("quantity" -> o.value("amount"))
        case other       => other
      }),
      ("transfer", transferV1Gen, {
        case o: JsObject if o.value.contains("feeAsset") =>
          o ++ Json.obj("feeAssetId" -> o.value("feeAsset"), "quantity" -> o.value("amount"))
        case other => other
      })
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
    "issue transaction" in {
      forAll(broadcastIssueReq) { ir =>
        def posting[A: Writes](v: A): RouteTestResult = Post(routePath("issue"), v) ~> route

        forAll(nonPositiveLong) { q =>
          posting(ir.copy(fee = q)) should produce(InsufficientFee())
        }
        forAll(nonPositiveLong) { q =>
          posting(ir.copy(quantity = q)) should produce(NonPositiveAmount(s"$q of assets"))
        }
        forAll(invalidDecimals) { d =>
          posting(ir.copy(decimals = d)) should produce(TooBigArrayAllocation)
        }
        forAll(longDescription) { d =>
          posting(ir.copy(description = d)) should produce(TooBigArrayAllocation)
        }
        forAll(invalidName) { name =>
          posting(ir.copy(name = name)) should produce(InvalidName)
        }
        forAll(invalidBase58) { name =>
          posting(ir.copy(name = name)) should produce(InvalidName)
        }
        forAll(nonPositiveLong) { fee =>
          posting(ir.copy(fee = fee)) should produce(InsufficientFee())
        }
      }
    }

    "reissue transaction" in {
      forAll(broadcastReissueReq) { rr =>
        def posting[A: Writes](v: A): RouteTestResult = Post(routePath("reissue"), v) ~> route

        // todo: invalid sender
        forAll(nonPositiveLong) { q =>
          posting(rr.copy(quantity = q)) should produce(NonPositiveAmount(s"$q of assets"))
        }
        forAll(nonPositiveLong) { fee =>
          posting(rr.copy(fee = fee)) should produce(InsufficientFee())
        }
      }
    }

    "burn transaction" in {
      forAll(broadcastBurnReq) { br =>
        def posting[A: Writes](v: A): RouteTestResult = Post(routePath("burn"), v) ~> route

        forAll(invalidBase58) { pk =>
          posting(br.copy(senderPublicKey = pk)) should produce(InvalidAddress)
        }
        forAll(nonPositiveLong) { q =>
          posting(br.copy(quantity = q)) should produce(NegativeAmount(s"$q of assets"))
        }
        forAll(nonPositiveLong) { fee =>
          posting(br.copy(fee = fee)) should produce(InsufficientFee())
        }
      }
    }

    "transfer transaction" in {
      forAll(broadcastTransferReq) { tr =>
        def posting[A: Writes](v: A): RouteTestResult = Post(routePath("transfer"), v) ~> route

        forAll(nonPositiveLong) { q =>
          posting(tr.copy(amount = q)) should produce(NonPositiveAmount(s"$q of ${tr.assetId.getOrElse("waves")}"))
        }
        forAll(invalidBase58) { pk =>
          posting(tr.copy(senderPublicKey = pk)) should produce(InvalidAddress)
        }
        forAll(invalidBase58) { a =>
          posting(tr.copy(recipient = a)) should produce(InvalidAddress)
        }
        forAll(invalidBase58) { a =>
          posting(tr.copy(assetId = Some(a))) should produce(CustomValidationError("invalid.assetId"))
        }
        forAll(invalidBase58) { a =>
          posting(tr.copy(feeAssetId = Some(a))) should produce(CustomValidationError("invalid.feeAssetId"))
        }
        forAll(longAttachment) { a =>
          posting(tr.copy(attachment = Some(a))) should produce(CustomValidationError("invalid.attachment"))
        }
        forAll(nonPositiveLong) { fee =>
          posting(tr.copy(fee = fee)) should produce(InsufficientFee())
        }
      }
    }
  }

  "compatibility" - {
    val route = AssetsApiRoute(restAPISettings, stub[Wallet], DummyUtxPoolSynchronizer.accepting, stub[Blockchain], stub[Time]).route

    val seed               = "seed".getBytes("UTF-8")
    val senderPrivateKey   = Wallet.generateNewAccount(seed, 0)
    val receiverPrivateKey = Wallet.generateNewAccount(seed, 1)

    val transferRequest = createSignedTransferRequest(
      TransferTransactionV1
        .selfSigned(
          assetId = Asset.Waves,
          sender = senderPrivateKey,
          recipient = receiverPrivateKey.toAddress,
          amount = 1 * Waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = Asset.Waves,
          feeAmount = Waves / 3,
          attachment = Array.emptyByteArray
        )
        .right
        .get
    )

    val versionedTransferRequest = createSignedVersionedTransferRequest(
      TransferTransactionV2
        .create(
          assetId = Asset.Waves,
          sender = senderPrivateKey,
          recipient = receiverPrivateKey.toAddress,
          amount = 1 * Waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = Asset.Waves,
          feeAmount = Waves / 3,
          attachment = Array.emptyByteArray,
          proofs = Proofs(Seq.empty)
        )
        .right
        .get
    )

    "/transfer" - {
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("transfer"), v).addHeader(ApiKeyHeader) ~> route

      "accepts TransferRequest" in posting(transferRequest) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferTransactions].select[TransferTransactionV1] shouldBe defined
      }

      "accepts VersionedTransferRequest" in posting(versionedTransferRequest) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferTransactions].select[TransferTransactionV2] shouldBe defined
      }

      "returns a error if it is not a transfer request" in posting(issueReq.sample.get) ~> check {
        status shouldBe StatusCodes.BadRequest
      }
    }

  }

  protected def createSignedTransferRequest(tx: TransferTransactionV1): SignedTransferV1Request = {
    import tx._
    SignedTransferV1Request(
      Base58.encode(tx.sender),
      assetId.maybeBase58Repr,
      recipient.stringRepr,
      amount,
      fee,
      feeAssetId.maybeBase58Repr,
      timestamp,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      signature.base58
    )
  }

  protected def createSignedVersionedTransferRequest(tx: TransferTransactionV2): SignedTransferV2Request = {
    import tx._
    SignedTransferV2Request(
      Base58.encode(tx.sender),
      assetId.maybeBase58Repr,
      recipient.stringRepr,
      amount,
      feeAssetId.maybeBase58Repr,
      fee,
      timestamp,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      proofs.proofs.map(_.base58)
    )
  }

}
