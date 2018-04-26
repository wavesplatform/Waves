package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.wavesplatform.RequestGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Diff
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.utx.{UtxBatchOps, UtxPool}
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Gen._
import org.scalacheck.{Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import scorex.api.http._
import scorex.api.http.assets._
import scorex.crypto.encode.Base58
import scorex.transaction.validation.ValidationError.GenericError
import scorex.transaction.assets.{TransferTransaction, VersionedTransferTransaction}
import scorex.transaction.validation.ValidationError
import scorex.transaction.{Proofs, Transaction}
import scorex.wallet.Wallet
import shapeless.Coproduct

class AssetsBroadcastRouteSpec extends RouteSpec("/assets/broadcast/") with RequestGen with PathMockFactory with PropertyChecks {
  private val settings    = RestAPISettings.fromConfig(ConfigFactory.load())
  private val utx         = stub[UtxPool]
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
        case other       => other
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

      forAll(nonPositiveLong) { q =>
        posting(ir.copy(fee = q)) should produce(InsufficientFee())
      }
      forAll(nonPositiveLong) { q =>
        posting(ir.copy(quantity = q)) should produce(NegativeAmount(s"$q of assets"))
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

    "reissue transaction" in forAll(broadcastReissueReq) { rr =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("reissue"), v) ~> route

      // todo: invalid sender
      forAll(nonPositiveLong) { q =>
        posting(rr.copy(quantity = q)) should produce(NegativeAmount(s"$q of assets"))
      }
      forAll(nonPositiveLong) { fee =>
        posting(rr.copy(fee = fee)) should produce(InsufficientFee())
      }
    }

    "burn transaction" in forAll(broadcastBurnReq) { br =>
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

    "transfer transaction" in forAll(broadcastTransferReq) { tr =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("transfer"), v) ~> route

      forAll(nonPositiveLong) { q =>
        posting(tr.copy(amount = q)) should produce(NegativeAmount(s"$q of waves"))
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
      forAll(posNum[Long]) { quantity =>
        posting(tr.copy(amount = quantity, fee = Long.MaxValue)) should produce(OverflowError)
      }
      forAll(nonPositiveLong) { fee =>
        posting(tr.copy(fee = fee)) should produce(InsufficientFee())
      }
    }
  }

  "compatibility" - {
    val alwaysApproveUtx = stub[UtxPool]
    val utxOps = new UtxBatchOps {
      override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = alwaysApproveUtx.putIfNew(tx)
    }
    (alwaysApproveUtx.batched[Any] _).when(*).onCall((f: UtxBatchOps => Any) => f(utxOps)).anyNumberOfTimes()
    (alwaysApproveUtx.putIfNew _).when(*).onCall((_: Transaction) => Right((true, Diff.empty))).anyNumberOfTimes()

    val alwaysSendAllChannels = stub[ChannelGroup]
    (alwaysSendAllChannels.writeAndFlush(_: Any)).when(*).onCall((_: Any) => null).anyNumberOfTimes()

    val route = AssetsBroadcastApiRoute(settings, alwaysApproveUtx, alwaysSendAllChannels).route

    val seed               = "seed".getBytes()
    val senderPrivateKey   = Wallet.generateNewAccount(seed, 0)
    val receiverPrivateKey = Wallet.generateNewAccount(seed, 1)

    val transferRequest = createSignedTransferRequest(
      TransferTransaction
        .create(
          assetId = None,
          sender = senderPrivateKey,
          recipient = receiverPrivateKey.toAddress,
          amount = 1 * Waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = Waves / 3,
          attachment = Array.emptyByteArray
        )
        .right
        .get
    )

    val versionedTransferRequest = createSignedVersionedTransferRequest(
      VersionedTransferTransaction
        .create(
          version = 2,
          assetId = None,
          sender = senderPrivateKey,
          recipient = receiverPrivateKey.toAddress,
          amount = 1 * Waves,
          timestamp = System.currentTimeMillis(),
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
        responseAs[TransferTransactions].select[TransferTransaction] shouldBe defined
      }

      "accepts VersionedTransferRequest" in posting(versionedTransferRequest) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferTransactions].select[VersionedTransferTransaction] shouldBe defined
      }

      "returns a error if it is not a transfer request" in posting(issueReq.sample.get) ~> check {
        status shouldNot be(StatusCodes.OK)
      }
    }

    "/batch-transfer" - {
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("batch-transfer"), v).addHeader(ApiKeyHeader) ~> route

      "accepts TransferRequest" in posting(List(transferRequest)) ~> check {
        status shouldBe StatusCodes.OK
        val xs = responseAs[Seq[TransferTransactions]]
        xs.size shouldBe 1
        xs.head.select[TransferTransaction] shouldBe defined
      }

      "accepts VersionedTransferRequest" in posting(List(versionedTransferRequest)) ~> check {
        status shouldBe StatusCodes.OK
        val xs = responseAs[Seq[TransferTransactions]]
        xs.size shouldBe 1
        xs.head.select[VersionedTransferTransaction] shouldBe defined
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
          xs.flatMap(_.select[TransferTransaction]) shouldNot be(empty)
          xs.flatMap(_.select[VersionedTransferTransaction]) shouldNot be(empty)
        }
      }

      "returns a error if it is not a transfer request" in posting(List(issueReq.sample.get)) ~> check {
        status shouldNot be(StatusCodes.OK)
      }
    }

  }

  protected def createSignedTransferRequest(tx: TransferTransaction): SignedTransferRequest = {
    import tx._
    SignedTransferRequest(
      Base58.encode(tx.sender.publicKey),
      assetId.map(_.base58),
      recipient.stringRepr,
      amount,
      fee,
      feeAssetId.map(_.base58),
      timestamp,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      signature.base58
    )
  }

  protected def createSignedVersionedTransferRequest(tx: VersionedTransferTransaction): SignedVersionedTransferRequest = {
    import tx._
    SignedVersionedTransferRequest(
      Base58.encode(tx.sender.publicKey),
      assetId.map(_.base58),
      recipient.stringRepr,
      amount,
      fee,
      timestamp,
      version,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      proofs.proofs.map(_.base58).toList
    )
  }

}
