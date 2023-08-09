package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.RequestGen
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi}
import com.wavesplatform.api.http.ApiError.*
import com.wavesplatform.api.http.RouteTimeout
import com.wavesplatform.api.http.assets.*
import com.wavesplatform.api.http.requests.{SignedTransferV1Request, SignedTransferV2Request}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.{Asset, Proofs, Transaction, TxPositiveAmount}
import com.wavesplatform.utils.*
import com.wavesplatform.wallet.Wallet
import org.scalacheck.Gen as G
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.*

import scala.concurrent.duration.*

class AssetsBroadcastRouteSpec
    extends RouteSpec("/assets/broadcast/")
    with RequestGen
    with PathMockFactory
    with RestAPISettingsHelper
    with SharedSchedulerMixin {

  private[this] val route = AssetsApiRoute(
    restAPISettings,
    stub[Wallet],
    DummyTransactionPublisher.rejecting(tx => TransactionValidationError(GenericError("foo"), tx)),
    stub[Blockchain],
    stub[() => SnapshotBlockchain],
    stub[Time],
    stub[CommonAccountsApi],
    stub[CommonAssetsApi],
    1000,
    new RouteTimeout(60.seconds)(sharedScheduler)
  ).route

  private[this] val fixedIssueGen = for {
    (sender, _, _, quantity, decimals, reissuable, fee, timestamp) <- issueParamGen
    nameLength  <- G.choose(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    name        <- G.listOfN(nameLength, G.alphaNumChar)
    description <- G.listOfN(IssueTransaction.MaxAssetDescriptionLength, G.alphaNumChar)
    tx          <- createLegacyIssue(sender, name.mkString.utf8Bytes, description.mkString.utf8Bytes, quantity, decimals, reissuable, fee, timestamp)
  } yield tx

  "returns StateCheckFailed" - {
    val vt = Table[String, G[? <: Transaction], JsValue => JsValue](
      ("url", "generator", "transform"),
      ("issue", fixedIssueGen, identity),
      ("reissue", reissueGen.retryUntil(_.version == 1), identity),
      (
        "burn",
        burnGen.retryUntil(_.version == 1),
        {
          case o: JsObject => o ++ Json.obj("quantity" -> o.value("amount"))
          case other       => other
        }
      ),
      (
        "transfer",
        transferV1Gen,
        {
          case o: JsObject if o.value.contains("feeAsset") =>
            o ++ Json.obj("feeAssetId" -> o.value("feeAsset"), "quantity" -> o.value("amount"))
          case other => other
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
    "issue transaction" in {
      forAll(broadcastIssueReq) { ir =>
        def posting[A: Writes](v: A): RouteTestResult = Post(routePath("issue"), v) ~> route

        forAll(nonPositiveLong) { q =>
          posting(ir.copy(fee = q)) should produce(InsufficientFee)
        }
        forAll(nonPositiveLong) { q =>
          posting(ir.copy(quantity = q)) should produce(NonPositiveAmount(s"$q of assets"))
        }
        forAll(invalidDecimals) { d =>
          posting(ir.copy(decimals = d)) should produce(InvalidDecimals(d.toString))
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
          posting(ir.copy(fee = fee)) should produce(InsufficientFee)
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
          posting(rr.copy(fee = fee)) should produce(InsufficientFee)
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
          posting(br.copy(amount = q)) should produce(NegativeAmount(s"$q of assets"))
        }
        forAll(nonPositiveLong) { fee =>
          posting(br.copy(fee = fee)) should produce(InsufficientFee)
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
          posting(tr.copy(assetId = Some(a))) should produce(
            WrongJson(errors = Seq(JsPath \ "assetId" -> Seq(JsonValidationError(s"Too long assetId: length of $a exceeds 44"))))
          )
        }
        forAll(invalidBase58) { a =>
          posting(tr.copy(feeAssetId = Some(a))) should produce(
            WrongJson(errors = Seq(JsPath \ "feeAssetId" -> Seq(JsonValidationError(s"Too long assetId: length of $a exceeds 44"))))
          )
        }
        forAll(nonPositiveLong) { fee =>
          posting(tr.copy(fee = fee)) should produce(InsufficientFee)
        }
      }
    }
  }

  "compatibility" - {
    val route = AssetsApiRoute(
      restAPISettings,
      stub[Wallet],
      DummyTransactionPublisher.accepting,
      stub[Blockchain],
      stub[() => SnapshotBlockchain],
      stub[Time],
      stub[CommonAccountsApi],
      stub[CommonAssetsApi],
      1000,
      new RouteTimeout(60.seconds)(sharedScheduler)
    ).route

    val seed               = "seed".getBytes("UTF-8")
    val senderPrivateKey   = Wallet.generateNewAccount(seed, 0)
    val receiverPrivateKey = Wallet.generateNewAccount(seed, 1)

    val transferRequest = createSignedTransferRequest(
      TransferTransaction
        .selfSigned(
          1.toByte,
          senderPrivateKey,
          receiverPrivateKey.toAddress,
          Asset.Waves,
          1.waves,
          Asset.Waves,
          0.3.waves,
          ByteStr.empty,
          System.currentTimeMillis()
        )
        .explicitGet()
    )

    val versionedTransferRequest = createSignedVersionedTransferRequest(
      TransferTransaction(
        version = 2.toByte,
        sender = senderPrivateKey.publicKey,
        recipient = receiverPrivateKey.toAddress,
        assetId = Asset.Waves,
        amount = TxPositiveAmount.unsafeFrom(1.waves),
        feeAssetId = Asset.Waves,
        fee = TxPositiveAmount.unsafeFrom(0.3.waves),
        attachment = ByteStr.empty,
        timestamp = System.currentTimeMillis(),
        proofs = Proofs(Seq.empty),
        chainId = receiverPrivateKey.toAddress.chainId
      )
    )

    "/transfer" - {
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("transfer"), v).addHeader(ApiKeyHeader) ~> route

      "accepts TransferRequest" in posting(transferRequest) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferTransaction].version shouldBe 1.toByte
      }

      "accepts VersionedTransferRequest" in posting(versionedTransferRequest) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferTransaction].version shouldBe 2.toByte
      }

      "returns a error if it is not a transfer request" in posting(issueReq.sample.get) ~> check {
        status shouldBe StatusCodes.BadRequest
      }
    }
  }

  protected def createSignedTransferRequest(tx: TransferTransaction): SignedTransferV1Request = {
    import tx.*
    SignedTransferV1Request(
      Base58.encode(tx.sender.arr),
      assetId.maybeBase58Repr,
      recipient.toString,
      amount.value,
      fee.value,
      feeAssetId.maybeBase58Repr,
      timestamp,
      Some(Base58.encode(attachment.arr)),
      proofs.toSignature.toString
    )
  }

  protected def createSignedVersionedTransferRequest(tx: TransferTransaction): SignedTransferV2Request = {
    import tx.*
    SignedTransferV2Request(
      Base58.encode(tx.sender.arr),
      assetId.maybeBase58Repr,
      recipient.toString,
      amount.value,
      feeAssetId.maybeBase58Repr,
      fee.value,
      timestamp,
      Some(Base58.encode(attachment.arr)),
      proofs.proofs.map(_.toString).toList
    )
  }

}
