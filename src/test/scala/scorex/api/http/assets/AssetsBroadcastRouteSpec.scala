package scorex.api.http.assets

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.server.Route
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.RestAPISettings
import de.heikoseeberger.akkahttpplayjson.PlayJsonSupport._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import scorex.api.http._
import scorex.api.http.assets.BroadcastRequests._
import scorex.crypto.encode.Base58
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.{Transaction, TransactionGen, TransactionModule, TypedTransaction}


class AssetsBroadcastRouteSpec extends RouteSpec("/assets/broadcast/") with PathMockFactory with PropertyChecks {

  import AssetsBroadcastRouteSpec._

  private def doCheck[A, B: Writes](route: Route, url: String, gen: G[A], f: (A) => B, expected: JsObject, expectedCode: StatusCode = StatusCodes.BadRequest): Unit = {
    forAll(gen) { v =>
      val post = Post(routePath(url), Json.toJson(f(v)))
      post ~> route ~> check {
        status shouldEqual expectedCode
        responseAs[JsObject] shouldEqual expected
      }
    }
  }

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

    forAll(vt) { (url, gen, transform) =>
      url in doCheck[Transaction, JsValue](route, url, gen, v => transform(v.json), StateCheckFailed.json)
    }
  }

  "returns appropriate error code when validation fails for" - {
    val route = AssetsBroadcastApiRoute(settings, mock[TransactionModule]).route


    "issue transaction" in forAll(g.issueReq) { ir =>
      def _check[A](gen: G[A], f: A => AssetIssueRequest, expected: JsObject) =
        doCheck[A, AssetIssueRequest](route, "issue", gen, f, expected)

      _check[Long](g.nonPositiveLong, q => ir.copy(quantity = q), NegativeAmount.json)
      _check[Byte](g.invalidDecimals, d => ir.copy(decimals = d), TooBigArrayAllocation.json)
      _check[String](g.longDescription, d => ir.copy(description = d), TooBigArrayAllocation.json)
      _check[String](g.invalidName, name => ir.copy(name = name), InvalidName.json)
      _check[String](g.invalidBase58, name => ir.copy(name = name), InvalidName.json)
      // todo: invalid sender
      _check[Long](g.nonPositiveLong, fee => ir.copy(fee = fee), InsufficientFee.json)
    }

    "reissue transaction" in forAll(g.reissueReq) { rr =>
      def _check[A](gen: G[A], f: A => AssetReissueRequest, expected: JsObject) =
        doCheck[A, AssetReissueRequest](route, "reissue", gen, f, expected)
      // todo: invalid sender
      _check[Long](g.nonPositiveLong, q => rr.copy(quantity = q), NegativeAmount.json)
      _check[Long](g.nonPositiveLong, fee => rr.copy(fee = fee), InsufficientFee.json)
    }

    "burn transaction" in forAll(g.burnReq) { br =>
      def _check[A](gen: G[A], f: A => AssetBurnRequest, expected: JsObject) =
        doCheck[A, AssetBurnRequest](route, "burn", gen, f, expected)
      // todo: invalid sender
      _check[String](g.invalidBase58, pk => br.copy(senderPublicKey = pk), Unknown.json)
      _check[Long](g.nonPositiveLong, q => br.copy(amount = q), NegativeAmount.json)
      _check[Long](g.nonPositiveLong, fee => br.copy(fee = fee), InsufficientFee.json)
    }

    "transfer transaction" in forAll(g.transferReq) { tr =>
      def _check[A](gen: G[A], f: A => AssetTransferRequest, expected: JsObject) =
        doCheck[A, AssetTransferRequest](route, "transfer", gen, f, expected)

      _check[Long](g.nonPositiveLong, q => tr.copy(amount = q), NegativeAmount.json)
//      todo: invalid recipient
      _check[String](g.invalidBase58, a => tr.copy(assetId = Some(a)), Unknown.json)
      _check[String](g.longAttachment, a => tr.copy(attachment = Some(a)), TooBigArrayAllocation.json)
      _check[Long](posNum[Long], quantity => tr.copy(amount = quantity, fee = Long.MaxValue), OverflowError.json)
      _check[Long](g.nonPositiveLong, fee => tr.copy(fee = fee), InsufficientFee.json)
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
      genBoundedString(IssueTransaction.MaxAssetNameLength + 1, IssueTransaction.MaxAssetNameLength + 1000)
    ).map(new String(_))
    val longDescription: G[String] =
      genBoundedBytes(IssueTransaction.MaxDescriptionLength + 1, IssueTransaction.MaxDescriptionLength + 1000)
      .map(Base58.encode)

    val fee: G[Long] = choose(0, Long.MaxValue)
    val signatureGen: G[String] = listOfN(TypedTransaction.SignatureLength, Arbitrary.arbByte.arbitrary)
      .map(b => Base58.encode(b.toArray))
    private val assetIdStringGen = assetIdGen.map(_.map(Base58.encode))

    private val commonFields = for {
      _account <- accountGen
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
    } yield AssetBurnRequest(account.address, assetId, quantity, fee, timestamp, signature)

    val transferReq: G[AssetTransferRequest] = for {
      (account, fee, timestamp, signature) <- commonFields
      recipient <- accountGen
      amount <- positiveLongGen
      assetId <- assetIdStringGen
      feeAssetId <- assetIdStringGen
      attachment <- genBoundedString(1, 20).map(b => Some(Base58.encode(b)))
    } yield AssetTransferRequest(account, assetId, recipient, amount, fee, feeAssetId, timestamp, attachment, signature)

  }
}
