package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.api.http.requests.{TransferV1Request, TransferV2Request}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, Height}
import com.wavesplatform.test._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers}
import com.wavesplatform.{TestTime, TestWallet}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsObject, JsValue, Json, Writes}

class AssetsRouteSpec extends RouteSpec("/assets") with WithDomain with RestAPISettingsHelper with TestWallet {

  private val MaxDistributionDepth = 1

  def routeTest[A](f: (Domain, Route) => A): A = withDomain(DomainPresets.RideV4.addFeatures(BlockchainFeatures.ReduceNFTFee)) { d =>
    f(
      d,
      seal(
        AssetsApiRoute(
          restAPISettings,
          testWallet,
          DummyTransactionPublisher.accepting,
          d.blockchain,
          new TestTime(),
          d.accountsApi,
          d.assetsApi,
          MaxDistributionDepth
        ).route
      )
    )
  }

  "/balance/{address}" - {
    "multiple ids" in routeTest { (d, route) =>
      val issuer = testWallet.generateNewAccount().get

      d.appendBlock(TxHelpers.genesis(issuer.toAddress, 100.waves))
      val issueTransactions = Seq.tabulate(4) { i =>
        TxHelpers.issue(issuer, 1000 * (i + 1), 2, name = s"ISSUE_$i")
      } :+ TxHelpers.issue(issuer, 1, reissuable = false)
      d.appendBlock(issueTransactions: _*)

      route.anyParamTest(routePath(s"/balance/${issuer.toAddress}"), "id")(issueTransactions.reverseIterator.map(_.id().toString).toSeq: _*) {
        status shouldBe StatusCodes.OK
        (responseAs[JsObject] \ "balances")
          .as[Seq[JsObject]]
          .zip(issueTransactions.reverse)
          .foreach {
            case (jso, tx) =>
              (jso \ "balance").as[Long] shouldEqual tx.quantity
              (jso \ "assetId").as[ByteStr] shouldEqual tx.id()
              (jso \ "reissuable").as[Boolean] shouldBe tx.reissuable
              (jso \ "minSponsoredAssetFee").asOpt[Long] shouldEqual None
              (jso \ "sponsorBalance").asOpt[Long] shouldEqual None
              (jso \ "quantity").as[Long] shouldEqual tx.quantity
              (jso \ "issueTransaction").as[JsObject] shouldEqual tx.json()
          }

      }

      route.anyParamTest(routePath(s"/balance/${issuer.toAddress}"), "id")("____", "----") {
        status shouldBe StatusCodes.BadRequest
        responseAs[JsValue] should matchJson("""{
                                               |    "error": 116,
                                               |    "message": "Request contains invalid IDs. ____, ----",
                                               |    "ids": [
                                               |        "____",
                                               |        "----"
                                               |    ]
                                               |}""".stripMargin)
      }

      withClue("over limit")(route.anyParamTest(routePath(s"/balance/${issuer.toAddress}"), "id")(Seq.fill(101)("aaa"): _*) {
        status shouldBe StatusCodes.BadRequest
        responseAs[JsValue] should matchJson("""{
                                               |  "error" : 10,
                                               |  "message" : "Too big sequence requested: max limit is 100 entries"
                                               |}""".stripMargin)
      })

      withClue("old GET portfolio does not include NFT")(Get(routePath(s"/balance/${issuer.toAddress}")) ~> route ~> check { // portfolio
        status shouldBe StatusCodes.OK
        val allBalances = (responseAs[JsValue] \ "balances")
          .as[Seq[JsObject]]
          .map { jso =>
            (jso \ "assetId").as[ByteStr] -> (jso \ "balance").as[Long]
          }
          .toMap

        val balancesAfterIssue = issueTransactions.init.map { it =>
          it.id() -> it.quantity
        }.toMap

        allBalances shouldEqual balancesAfterIssue
      })
    }
  }

  "/transfer" - {
    def posting[A: Writes](route: Route, v: A): RouteTestResult = Post(routePath("/transfer"), v).addHeader(ApiKeyHeader) ~> route

    "accepts TransferRequest" in routeTest { (_, route) =>
      val sender    = testWallet.generateNewAccount().get
      val recipient = testWallet.generateNewAccount().get
      val req = TransferV1Request(
        assetId = None,
        feeAssetId = None,
        amount = 1.waves,
        fee = 0.3.waves,
        sender = sender.toAddress.toString,
        attachment = Some("attachment"),
        recipient = recipient.toAddress.toString,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(route, req) ~> check {
        status shouldBe StatusCodes.OK

        responseAs[TransferTransaction]
      }
    }

    "accepts VersionedTransferRequest" in routeTest { (_, route) =>
      val sender    = testWallet.generateNewAccount().get
      val recipient = testWallet.generateNewAccount().get
      val req = TransferV2Request(
        assetId = None,
        amount = 1.waves,
        feeAssetId = None,
        fee = 0.3.waves,
        sender = sender.toAddress.toString,
        attachment = None,
        recipient = recipient.toAddress.toString,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(route, req) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferV2Request]
      }
    }

    "returns a error if it is not a transfer request" in routeTest { (_, route) =>
      posting(route, Json.obj("key" -> "value")) ~> check {
        status shouldBe StatusCodes.BadRequest
      }
    }
  }

  routePath("/{assetId}/distribution/{height}/limit/{limit}") in routeTest { (d, route) =>
    val issuer           = testWallet.generateNewAccount().get
    val issueTransaction = TxHelpers.issue(issuer, 100_0000, 4, "PA_01")
    d.appendBlock(TxHelpers.genesis(issuer.toAddress, 10.waves))
    val recipients = testWallet.generateNewAccounts(5)
    val transfers  = recipients.zipWithIndex.map { case (kp, i) => MassTransferTransaction.ParsedTransfer(kp.toAddress, (i + 1) * 10000) }
    d.appendBlock(
      issueTransaction,
      MassTransferTransaction
        .selfSigned(
          2.toByte,
          issuer,
          issueTransaction.asset,
          transfers,
          0.01.waves,
          ntpTime.getTimestamp(),
          ByteStr.empty
        )
        .explicitGet()
    )

    d.appendBlock()
    Get(routePath(s"/${issueTransaction.id()}/distribution/2/limit/$MaxAddressesPerRequest")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "items").as[JsObject] shouldBe Json.obj(
        transfers.map(pt => pt.address.toString -> (pt.amount: JsValueWrapper)) :+
          (issuer.toAddress.toString -> (issueTransaction.quantity - transfers.map(_.amount).sum: JsValueWrapper)): _*
      )
    }

    Get(routePath(s"/${issueTransaction.id()}/distribution/2/limit/${MaxAddressesPerRequest + 1}")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> s"Limit should be less than or equal to $MaxAddressesPerRequest")
    }

    Get(routePath(s"/${issueTransaction.id()}/distribution/1/limit/1")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj(
        "error"   -> 199,
        "message" -> s"Unable to get distribution past height ${d.blockchain.height - MaxDistributionDepth}"
      )
    }
  }

  private val nonNftTestData = Table(
    ("version", "reissuable", "script"),
    (1.toByte, false, None),
    (1.toByte, true, None),
    (2.toByte, false, None),
    (2.toByte, true, None),
    (2.toByte, false, Some(ExprScript(CONST_BOOLEAN(false)).explicitGet())),
    (2.toByte, true, Some(ExprScript(CONST_BOOLEAN(false)).explicitGet())),
    (3.toByte, false, None),
    (3.toByte, true, None),
    (3.toByte, false, Some(ExprScript(CONST_BOOLEAN(false)).explicitGet())),
    (3.toByte, true, Some(ExprScript(CONST_BOOLEAN(false)).explicitGet()))
  )

  routePath(s"/details/{id}") in routeTest { (d, route) =>
    val sender = testWallet.generateNewAccount().get

    d.appendBlock(GenesisTransaction.create(sender.toAddress, 100.waves, System.currentTimeMillis()).explicitGet())

    forAll(nonNftTestData) {
      case (version, reissuable, script) =>
        val name        = s"IA_$version"
        val description = s"v${version}_${if (reissuable) "" else "non-"}reissuable"
        val issueTransaction =
          TxHelpers.issue(sender, 500000, 4, name, reissuable = reissuable, description = description, version = version, script = script)

        d.appendBlock(issueTransaction)

        route.anyParamTest(routePath("/details"), "id")(issueTransaction.id().toString) {
          status shouldBe StatusCodes.OK
          checkResponse(
            issueTransaction,
            AssetDescription(
              issueTransaction.id(),
              sender.publicKey,
              issueTransaction.name,
              issueTransaction.description,
              issueTransaction.decimals,
              reissuable,
              issueTransaction.quantity,
              Height(d.blockchain.height),
              script.map(s => AssetScriptInfo(s, 1L)),
              0L,
              nft = false
            ),
            responseAs[Seq[JsObject]].head
          )
        }
    }
  }

  routePath("/nft/list") in routeTest { (d, route) =>
    val issuer = testWallet.generateNewAccount().get
    val nfts = Seq.tabulate(5) { i =>
      TxHelpers.issue(issuer, 1, name = s"NFT_0$i", reissuable = false, fee = 0.001.waves)
    }
    d.appendBlock(TxHelpers.genesis(issuer.toAddress, 100.waves))
    val nonNFT = TxHelpers.issue(issuer, 100, 2.toByte)
    d.appendBlock((nfts :+ nonNFT): _*)

    Get(routePath(s"/balance/${issuer.toAddress}/${nonNFT.id()}")) ~> route ~> check {
      val balance = responseAs[JsObject]
      (balance \ "address").as[String] shouldEqual issuer.toAddress.toString
      (balance \ "balance").as[Long] shouldEqual nonNFT.quantity
      (balance \ "assetId").as[String] shouldEqual nonNFT.id().toString
    }

    Get(routePath(s"/nft/${issuer.toAddress}/limit/5")) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val nftList = responseAs[Seq[JsObject]]
      nftList.size shouldEqual nfts.size
      nftList.foreach { jso =>
        val nftId = (jso \ "assetId").as[ByteStr]
        val nft   = nfts.find(_.id() == nftId).get

        nft.name.toStringUtf8 shouldEqual (jso \ "name").as[String]
        nft.timestamp shouldEqual (jso \ "issueTimestamp").as[Long]
        nft.id() shouldEqual (jso \ "originTransactionId").as[ByteStr]
      }
    }
  }

  private def checkResponse(tx: IssueTransaction, desc: AssetDescription, response: JsObject): Unit = {
    (response \ "assetId").as[String] shouldBe tx.id().toString
    (response \ "issueTimestamp").as[Long] shouldBe tx.timestamp
    (response \ "issuer").as[String] shouldBe tx.sender.toAddress.toString
    (response \ "name").as[String] shouldBe tx.name.toStringUtf8
    (response \ "description").as[String] shouldBe tx.description.toStringUtf8
    (response \ "decimals").as[Int] shouldBe tx.decimals
    (response \ "reissuable").as[Boolean] shouldBe tx.reissuable
    (response \ "quantity").as[BigDecimal] shouldBe desc.totalVolume
    (response \ "minSponsoredAssetFee").asOpt[Long] shouldBe empty
    (response \ "originTransactionId").as[String] shouldBe tx.id().toString
  }
}
