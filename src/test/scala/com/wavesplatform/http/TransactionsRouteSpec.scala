package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http.{InvalidAddress, InvalidSignature, TooBigArrayAllocation, TransactionsApiRoute}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lang.StdLibVersion.V1
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.settings.{TestFunctionalitySettings, WalletSettings}
import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{BlockGen, NoShrink, TestTime, TransactionGen}
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._

class TransactionsRouteSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with MockFactory
    with Matchers
    with TransactionGen
    with BlockGen
    with PropertyChecks
    with NoShrink {

  private val wallet      = Wallet(WalletSettings(None, Some("qwerty"), None))
  private val blockchain  = mock[Blockchain]
  private val utx         = mock[UtxPool]
  private val allChannels = mock[ChannelGroup]
  private val route       = TransactionsApiRoute(restAPISettings, TestFunctionalitySettings.Stub, wallet, blockchain, utx, allChannels, new TestTime).route

  private val invalidBase58Gen = alphaNumStr.map(_ + "0")

  routePath("/calculateFee") - {
    "transfer with Waves fee" - {
      "TransferTransaction" in {
        val sender: PublicKeyAccount = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "senderPublicKey" -> Base58.encode(sender.publicKey),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 100)
        )
        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()
        (blockchain.hasScript _).expects(sender.toAddress).returning(false).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)

        val route = TransactionsApiRoute(restAPISettings, featuresSettings, wallet, blockchain, utx, allChannels, new TestTime).route

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
        }
      }

      "MassTransferTransaction" in {
        val sender: PublicKeyAccount = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 11,
          "version"         -> 1,
          "senderPublicKey" -> Base58.encode(sender.publicKey),
          "transfers" -> Json.arr(
            Json.obj(
              "recipient" -> accountGen.sample.get.toAddress,
              "amount"    -> 1000000
            ),
            Json.obj(
              "recipient" -> accountGen.sample.get.toAddress,
              "amount"    -> 2000000
            )
          )
        )

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 100)
        )
        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()
        (blockchain.hasScript _).expects(sender.toAddress).returning(false).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)

        val route = TransactionsApiRoute(restAPISettings, featuresSettings, wallet, blockchain, utx, allChannels, new TestTime).route

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 200000
        }
      }
    }

    "transfer with Asset fee" - {
      "without sponsorship" in {
        val assetId: ByteStr         = issueGen.sample.get.assetId()
        val sender: PublicKeyAccount = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "feeAssetId"      -> assetId.base58,
          "senderPublicKey" -> Base58.encode(sender.publicKey),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 100)
        )
        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()
        (blockchain.hasScript _).expects(sender.toAddress).returning(false).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)

        val route = TransactionsApiRoute(restAPISettings, featuresSettings, wallet, blockchain, utx, allChannels, new TestTime).route

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
        }
      }

      "with sponsorship" in {
        val assetId: IssuedAsset     = IssuedAsset(issueGen.sample.get.assetId())
        val sender: PublicKeyAccount = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "feeAssetId"      -> assetId.id.base58,
          "senderPublicKey" -> Base58.encode(sender.publicKey),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 0)
        )
        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(featuresSettings.featureCheckBlocksPeriod).once()
        (blockchain.hasScript _).expects(sender.toAddress).returning(false).once()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)
        (blockchain.assetDescription _)
          .expects(assetId)
          .returning(Some(AssetDescription(
            issuer = accountGen.sample.get,
            name = "foo".getBytes,
            description = "bar".getBytes,
            decimals = 8,
            reissuable = false,
            totalVolume = Long.MaxValue,
            script = None,
            sponsorship = 5
          )))
          .anyNumberOfTimes()

        val route = TransactionsApiRoute(restAPISettings, featuresSettings, wallet, blockchain, utx, allChannels, new TestTime).route

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").as[String] shouldBe assetId.id.base58
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 5
        }
      }

      "with sponsorship, smart token and smart account" in {
        val assetId: IssuedAsset     = IssuedAsset(issueGen.sample.get.assetId())
        val sender: PublicKeyAccount = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "feeAssetId"      -> assetId.id.base58,
          "senderPublicKey" -> Base58.encode(sender.publicKey),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 0)
        )

        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(featuresSettings.featureCheckBlocksPeriod).once()
        (blockchain.hasScript _).expects(sender.toAddress).returning(true).once()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)
        (blockchain.assetDescription _)
          .expects(assetId)
          .returning(Some(AssetDescription(
            issuer = accountGen.sample.get,
            name = "foo".getBytes,
            description = "bar".getBytes,
            decimals = 8,
            reissuable = false,
            totalVolume = Long.MaxValue,
            script = Some(ExprScript(V1, TRUE, checkSize = false).explicitGet()),
            sponsorship = 5
          )))
          .anyNumberOfTimes()

        val route = TransactionsApiRoute(restAPISettings, featuresSettings, wallet, blockchain, utx, allChannels, new TestTime).route

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").as[String] shouldBe assetId.id.base58
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 45
        }
      }
    }
  }

  routePath("/address/{address}/limit/{limit}") - {
    val bytes32StrGen = bytes32gen.map(Base58.encode)
    val addressGen    = accountGen.map(_.address)

    "handles parameter errors with corresponding responses" - {
      "invalid address" in {
        forAll(bytes32StrGen) { badAddress =>
          Get(routePath(s"/address/$badAddress/limit/1")) ~> route should produce(InvalidAddress)
        }
      }

      "invalid limit" - {
        "limit is too big" in {
          forAll(addressGen, choose(MaxTransactionsPerRequest + 1, Int.MaxValue).label("limitExceeded")) {
            case (address, limit) =>
              Get(routePath(s"/address/$address/limit/$limit")) ~> route should produce(TooBigArrayAllocation)
          }
        }
      }

      "invalid after" in {
        forAll(addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect"), invalidBase58Gen) {
          case (address, limit, invalidBase58) =>
            Get(routePath(s"/address/$address/limit/$limit?after=$invalidBase58")) ~> route ~> check {
              status shouldEqual StatusCodes.BadRequest
              (responseAs[JsObject] \ "message").as[String] shouldEqual s"Unable to decode transaction id $invalidBase58"
            }
        }
      }
    }

    "returns 200 if correct params provided" - {
      def routeGen: Gen[Route] =
        Gen.const({
          val b = mock[Blockchain]
          (b.addressTransactions _).expects(*, *, *, *).returning(Right(Seq.empty[(Int, Transaction)])).anyNumberOfTimes()
          TransactionsApiRoute(restAPISettings, TestFunctionalitySettings.Stub, wallet, b, utx, allChannels, new TestTime).route
        })

      "address and limit" in {
        forAll(routeGen, addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect")) {
          case (r, address, limit) =>
            Get(routePath(s"/address/$address/limit/$limit")) ~> r ~> check {
              status shouldEqual StatusCodes.OK
            }
        }
      }

      "address, limit and after" in {
        forAll(routeGen, addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect"), bytes32StrGen) {
          case (r, address, limit, txId) =>
            Get(routePath(s"/address/$address/limit/$limit?after=$txId")) ~> r ~> check {
              status shouldEqual StatusCodes.OK
            }
        }
      }
    }
  }

  routePath("/info/{signature}") - {
    "handles invalid signature" in {
      forAll(invalidBase58Gen) { invalidBase58 =>
        Get(routePath(s"/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      val txAvailability = for {
        tx     <- randomTransactionGen
        height <- posNum[Int]
      } yield (tx, height)

      forAll(txAvailability) {
        case (tx, height) =>
          (blockchain.transactionInfo _).expects(tx.id()).returning(Some((height, tx))).once()
          Get(routePath(s"/info/${tx.id().base58}")) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            responseAs[JsValue] shouldEqual tx.json() + ("height" -> JsNumber(height))
          }
      }
    }
  }

  routePath("/unconfirmed") - {
    "returns the list of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        (utx.all _).expects().returns(txs).once()
        Get(routePath("/unconfirmed")) ~> route ~> check {
          val resp = responseAs[Seq[JsValue]]
          for ((r, t) <- resp.zip(txs)) {
            if ((r \ "version").as[Int] == 1) {
              (r \ "signature").as[String] shouldEqual t.proofs.proofs(0).base58
            } else {
              (r \ "proofs").as[Seq[String]] shouldEqual t.proofs.proofs.map(_.base58)
            }
          }
        }
      }
    }
  }

  routePath("/unconfirmed/size") - {
    "returns the size of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        (utx.size _).expects().returns(txs.size).once()
        Get(routePath("/unconfirmed/size")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual Json.obj("size" -> JsNumber(txs.size))
        }
      }
    }
  }

  routePath("/unconfirmed/info/{signature}") - {
    "handles invalid signature" in {
      forAll(invalidBase58Gen) { invalidBase58 =>
        Get(routePath(s"/unconfirmed/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/unconfirmed/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/unconfirmed/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      forAll(randomTransactionGen) { tx =>
        (utx.transactionById _).expects(tx.id()).returns(Some(tx)).once()
        Get(routePath(s"/unconfirmed/info/${tx.id().base58}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual tx.json()
        }
      }
    }
  }
}
