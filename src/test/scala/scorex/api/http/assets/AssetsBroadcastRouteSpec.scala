package scorex.api.http.assets

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.RestAPISettings
import de.heikoseeberger.akkahttpplayjson.PlayJsonSupport._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import scorex.api.http._
import scorex.api.http.assets.BroadcastRequests._
import scorex.crypto.encode.Base58
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.{Transaction, TransactionGen, TransactionModule, TypedTransaction}


class AssetsBroadcastRouteSpec extends RouteSpec("/assets/broadcast/") with PathMockFactory with PropertyChecks {

  import AssetsBroadcastRouteSpec._

  class ProduceError(error: ApiError) extends Matcher[RouteTestResult] {
    override def apply(left: RouteTestResult): MatchResult = left ~> check {
      if (response.status != error.code) {
        MatchResult(false, "got unexpected status code", "got expected status code")
      } else {
        val responseJson = responseAs[JsObject]
        MatchResult(responseJson == error.json,
          "expected {0}, but instead got {1}",
          "expected not to get {0}, but instead did get it",
          IndexedSeq(error.json, responseJson))
      }
    }
  }

  def produce(error: ApiError) = new ProduceError(error)

  private val settings = RestAPISettings.fromConfig(ConfigFactory.parseString(""))

  "returns StateCheckFiled when state validation fails" - {
    val stmMock = {
      val m = mock[TransactionModule]
      (m.onNewOffchainTransaction _).expects(*).onCall { _: Transaction => false } anyNumberOfTimes()
      m
    }

    val route = AssetsBroadcastApiRoute(settings, stmMock).route

    val vt = Table[String, G[_ <: Transaction], (JsValue) => JsValue](
      ("url", "generator", "transform"),
      ("issue", g.issueGenerator, identity),
      ("reissue", g.reissueGenerator, identity),
      ("burn", g.burnGenerator, {
        case o: JsObject => o ++ Json.obj("quantity" -> o.value("amount"))
        case other => other
      }),
      ("transfer", g.transferGenerator, {
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


    "issue transaction" in forAll(g.issueReq) { ir =>
      def posting[A: Writes](v: A) = Post(routePath("issue"), v) ~> route

      forAll(g.nonPositiveLong) { q => posting(ir.copy(fee = q)) should produce (InsufficientFee) }
      forAll(g.nonPositiveLong) { q => posting(ir.copy(quantity = q)) should produce (NegativeAmount) }
      forAll(g.invalidDecimals) { d => posting(ir.copy(decimals = d)) should produce (TooBigArrayAllocation) }
      forAll(g.longDescription) { d => posting(ir.copy(description = d)) should produce (TooBigArrayAllocation) }
      forAll(g.invalidName) { name => posting(ir.copy(name = name)) should produce (InvalidName) }
      forAll(g.invalidBase58) { name => posting(ir.copy(name = name)) should produce (InvalidName) }
      forAll(g.nonPositiveLong) { fee => posting(ir.copy(fee = fee)) should produce (InsufficientFee) }
    }

    "reissue transaction" in forAll(g.reissueReq) { rr =>
      def posting[A: Writes](v: A) = Post(routePath("reissue"), v) ~> route

      // todo: invalid sender
      forAll(g.nonPositiveLong) { q => posting(rr.copy(quantity = q)) should produce (NegativeAmount) }
      forAll(g.nonPositiveLong) { fee => posting(rr.copy(fee = fee)) should produce (InsufficientFee) }
    }

    "burn transaction" in forAll(g.burnReq) { br =>
      def posting[A: Writes](v: A) = Post(routePath("burn"), v) ~> route

      forAll(g.invalidBase58) { pk => posting(br.copy(senderPublicKey = pk)) should produce (InvalidAddress) }
      forAll(g.nonPositiveLong) { q => posting(br.copy(quantity = q)) should produce (NegativeAmount) }
      forAll(g.nonPositiveLong) { fee => posting(br.copy(fee = fee)) should produce (InsufficientFee) }
    }

    "transfer transaction" in forAll(g.transferReq) { tr =>
      def posting[A: Writes](v: A) = Post(routePath("transfer"), v) ~> route

      forAll(g.nonPositiveLong) { q => posting(tr.copy(amount = q)) should produce (NegativeAmount) }
      forAll(g.invalidBase58) { pk => posting(tr.copy(senderPublicKey = pk)) should produce (InvalidAddress) }
      forAll(g.invalidBase58) { pk => posting(tr.copy(recipient = pk)) should produce (InvalidAddress) }
      forAll(g.invalidBase58) { a => posting(tr.copy(assetId = Some(a))) should produce (CustomValidationError("invalid.assetId")) }
      forAll(g.invalidBase58) { a => posting(tr.copy(feeAssetId = Some(a))) should produce (CustomValidationError("invalid.feeAssetId")) }
      forAll(g.longAttachment) { a => posting(tr.copy(attachment = Some(a))) should produce (TooBigArrayAllocation) }
      forAll(posNum[Long]) { quantity => posting(tr.copy(amount = quantity, fee = Long.MaxValue)) should produce (OverflowError) }
      forAll(g.nonPositiveLong) { fee => posting(tr.copy(fee = fee)) should produce (InsufficientFee) }
    }
  }
}

object AssetsBroadcastRouteSpec {
  private[AssetsBroadcastRouteSpec] object g extends TransactionGen {
    val nonPositiveLong: G[Long] = choose(Long.MinValue, 0).label("non-positive value")
    val invalidDecimals: G[Byte] = oneOf(
      choose[Byte](Byte.MinValue, -1),
      choose((IssueTransaction.MaxDecimals + 1).toByte, Byte.MaxValue)
    ).label("invalid decimals")

    val longAttachment: G[String] =
      genBoundedBytes(TransferTransaction.MaxAttachmentSize + 1, TransferTransaction.MaxAttachmentSize + 50)
        .map(Base58.encode)
    val invalidBase58: G[String] = listOfN(50, oneOf(alphaNumChar, oneOf('O', '0', 'l')))
      .map(_.mkString)
      .label("invalid base58")
    val invalidName: G[String] = oneOf(
      genBoundedString(0, IssueTransaction.MinAssetNameLength - 1),
      genBoundedString(IssueTransaction.MaxAssetNameLength + 1, IssueTransaction.MaxAssetNameLength + 50)
    ).map(new String(_))
    val longDescription: G[String] =
      genBoundedBytes(IssueTransaction.MaxDescriptionLength + 1, IssueTransaction.MaxDescriptionLength + 50)
      .map(Base58.encode)

    val addressGen: G[String] = listOfN(32, Arbitrary.arbByte.arbitrary).map(b => Base58.encode(b.toArray))
    val fee: G[Long] = choose(0, Long.MaxValue)
    val signatureGen: G[String] = listOfN(TypedTransaction.SignatureLength, Arbitrary.arbByte.arbitrary)
      .map(b => Base58.encode(b.toArray))
    private val assetIdStringGen = assetIdGen.map(_.map(Base58.encode))

    private val commonFields = for {
      _account <- addressGen
      _fee <- fee
      _timestamp <- timestampGen
      _signature <- signatureGen
    } yield (_account, _fee, _timestamp, _signature)

    val issueReq: G[AssetIssueRequest] = for {
      (account, fee, timestamp, signature) <- commonFields
      name <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
      quantity <- positiveLongGen
      decimals <- G.choose[Byte](0, IssueTransaction.MaxDecimals.toByte)
      reissuable <- G.oneOf(true, false)
    } yield AssetIssueRequest(account, new String(name), new String(description), quantity, decimals, reissuable, fee, timestamp, signature)

    private val reissueBurnFields = for {
      assetId <- bytes32gen.map(Base58.encode)
      quantity <- positiveLongGen
    } yield (assetId, quantity)

    val reissueReq: G[AssetReissueRequest] = for {
      (account, fee, timestamp, signature) <- commonFields
      (assetId, quantity) <- reissueBurnFields
      reissuable <- G.oneOf(true, false)
    } yield AssetReissueRequest(account, assetId, quantity, reissuable, fee, timestamp, signature)

    val burnReq: G[AssetBurnRequest] = for {
      (account, fee, timestamp, signature) <- commonFields
      (assetId, quantity) <- reissueBurnFields
    } yield AssetBurnRequest(account, assetId, quantity, fee, timestamp, signature)

    val transferReq: G[AssetTransferRequest] = for {
      (account, fee, timestamp, signature) <- commonFields
      recipient <- accountGen.map(_.address)
      amount <- positiveLongGen
      assetId <- assetIdStringGen
      feeAssetId <- assetIdStringGen
      attachment <- genBoundedString(1, 20).map(b => Some(Base58.encode(b)))
    } yield AssetTransferRequest(account, assetId, recipient, amount, fee, feeAssetId, timestamp, attachment, signature)

  }
}
