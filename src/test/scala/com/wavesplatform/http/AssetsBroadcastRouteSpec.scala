package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.wavesplatform.RequestGen
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.assets._
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Diff
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, Proofs, Transaction}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.{ChannelGroup, ChannelGroupFuture, ChannelMatcher}
import org.scalacheck.{Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import shapeless.Coproduct

class AssetsBroadcastRouteSpec extends RouteSpec("/assets/broadcast/") with RequestGen with PathMockFactory with PropertyChecks {
  private val settings    = RestAPISettings.fromConfig(ConfigFactory.load())
  private val utx         = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]

  (utx.putIfNew _).when(*).onCall((t: Transaction) => Left(TransactionValidationError(GenericError("foo"), t))).anyNumberOfTimes()

  "returns StateCheckFailed" - {

    val route = AssetsBroadcastApiRoute(settings, utx, allChannels).route

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
      val route = AssetsBroadcastApiRoute(settings, utx, allChannels).route
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
      val route = AssetsBroadcastApiRoute(settings, utx, allChannels).route
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
      val route = AssetsBroadcastApiRoute(settings, utx, allChannels).route
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
      val route = AssetsBroadcastApiRoute(settings, utx, allChannels).route
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
    val alwaysApproveUtx = stub[UtxPool]
    (alwaysApproveUtx.putIfNew _).when(*).onCall((_: Transaction) => Right((true, Diff.empty))).anyNumberOfTimes()

    val alwaysSendAllChannels = stub[ChannelGroup]
    (alwaysSendAllChannels
      .writeAndFlush(_: Any, _: ChannelMatcher))
      .when(*, *)
      .onCall((_: Any, _: ChannelMatcher) => stub[ChannelGroupFuture])
      .anyNumberOfTimes()

    val route = AssetsBroadcastApiRoute(settings, alwaysApproveUtx, alwaysSendAllChannels).route

    val seed               = "seed".getBytes()
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
        .get)

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

    "/batch-transfer" - {
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("batch-transfer"), v).addHeader(ApiKeyHeader) ~> route

      "accepts TransferRequest" in posting(List(transferRequest)) ~> check {
        status shouldBe StatusCodes.OK
        val xs = responseAs[Seq[TransferTransactions]]
        xs.size shouldBe 1
        xs.head.select[TransferTransactionV1] shouldBe defined
      }

      "accepts VersionedTransferRequest" in posting(List(versionedTransferRequest)) ~> check {
        status shouldBe StatusCodes.OK
        val xs = responseAs[Seq[TransferTransactions]]
        xs.size shouldBe 1
        xs.head.select[TransferTransactionV2] shouldBe defined
      }

      "accepts both TransferRequest and VersionedTransferRequest" in {
        val reqs = List(
          Coproduct[SignedTransferRequests](transferRequest),
          Coproduct[SignedTransferRequests](versionedTransferRequest)
        )

        posting(reqs) ~> check {
          status shouldBe StatusCodes.OK
          val xs = responseAs[Seq[TransferTransactions]]
          xs.size shouldBe 2
          xs.flatMap(_.select[TransferTransactionV1]) shouldNot be(empty)
          xs.flatMap(_.select[TransferTransactionV2]) shouldNot be(empty)
        }
      }

      "returns a error if it is not a transfer request" in posting(List(issueReq.sample.get)) ~> check {
        status shouldBe StatusCodes.BadRequest
      }
    }

  }

  protected def createSignedTransferRequest(tx: TransferTransactionV1): SignedTransferV1Request = {
    import tx._
    SignedTransferV1Request(
      Base58.encode(tx.sender.publicKey),
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
      Base58.encode(tx.sender.publicKey),
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
