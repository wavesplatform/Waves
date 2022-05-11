package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.google.protobuf.ByteString
import com.wavesplatform.TestWallet
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.api.http.requests.{TransferV1Request, TransferV2Request}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{Domain, defaultSigner}
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, BinaryDataEntry, Height}
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.transaction.utils.EthTxGenerator.Arg
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxHelpers, TxNonNegativeAmount, TxVersion}
import org.scalatest.concurrent.Eventually
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsObject, JsValue, Json, Writes}

class AssetsRouteSpec extends RouteSpec("/assets") with Eventually with RestAPISettingsHelper with WithDomain with TestWallet {

  private val MaxDistributionDepth = 1

  def routeTest[A](settings: WavesSettings = DomainPresets.RideV4.addFeatures(BlockchainFeatures.ReduceNFTFee))(f: (Domain, Route) => A): A =
    withDomain(settings) { d =>
      f(
        d,
        seal(
          AssetsApiRoute(
            restAPISettings,
            testWallet,
            DummyTransactionPublisher.accepting,
            d.blockchain,
            TestTime(),
            d.accountsApi,
            d.assetsApi,
            MaxDistributionDepth
          ).route
        )
      )
    }

  private def setScriptTransaction(sender: KeyPair) =
    SetScriptTransaction
      .selfSigned(
        TxVersion.V2,
        sender,
        Some(TestCompiler(V6).compileContract("""
                                                |{-# STDLIB_VERSION 6 #-}
                                                |{-# CONTENT_TYPE DAPP #-}
                                                |{-# SCRIPT_TYPE ACCOUNT #-}
                                                |
                                                |@Callable(inv)
                                                |func issue(name: String, description: String, amount: Int, decimals: Int, isReissuable: Boolean) = {
                                                |  let t = Issue(name, description, amount, decimals, isReissuable)
                                                |  [
                                                |    t,
                                                |    BinaryEntry("assetId", calculateAssetId(t))
                                                |  ]
                                                |}
                                                |""".stripMargin)),
        0.01.waves,
        ntpTime.getTimestamp()
      )
      .explicitGet()

  private def issueTransaction(name: Option[String] = None, script: Option[Script] = None, quantity: Option[Long] = None): IssueTransaction =
    IssueTransaction
      .selfSigned(
        version = TxVersion.V2,
        sender = defaultSigner,
        name = name.getOrElse(assetDesc.name.toStringUtf8),
        description = assetDesc.description.toStringUtf8,
        quantity = quantity.getOrElse(assetDesc.totalVolume.toLong),
        decimals = assetDesc.decimals.toByte,
        reissuable = assetDesc.reissuable,
        script = script,
        fee = 1.waves,
        timestamp = TxHelpers.timestamp
      )
      .explicitGet()

  private val assetDesc = AssetDescription(
    ByteStr.empty,
    issuer = TxHelpers.defaultSigner.publicKey,
    name = ByteString.copyFromUtf8("test"),
    description = ByteString.copyFromUtf8("description"),
    decimals = 0,
    reissuable = true,
    totalVolume = 100,
    lastUpdatedAt = Height(1),
    script = None,
    sponsorship = 0,
    nft = false
  )

  "/balance/{address}" - {
    "multiple ids" in routeTest() { (d, route) =>
      val issuer = testWallet.generateNewAccount().get

      d.appendBlock(TxHelpers.genesis(issuer.toAddress, 100.waves))
      val issueTransactions = Seq.tabulate(4) { i =>
        TxHelpers.issue(issuer, 1000 * (i + 1), 2, name = s"ISSUE_$i")
      } :+ TxHelpers.issue(issuer, 1, reissuable = false)
      d.appendBlock(issueTransactions*)

      route.anyParamTest(routePath(s"/balance/${issuer.toAddress}"), "id")(issueTransactions.reverseIterator.map(_.id().toString).toSeq*) {
        status shouldBe StatusCodes.OK
        (responseAs[JsObject] \ "balances")
          .as[Seq[JsObject]]
          .zip(issueTransactions.reverse)
          .foreach { case (jso, tx) =>
            (jso \ "balance").as[Long] shouldEqual tx.quantity.value
            (jso \ "assetId").as[ByteStr] shouldEqual tx.id()
            (jso \ "reissuable").as[Boolean] shouldBe tx.reissuable
            (jso \ "minSponsoredAssetFee").asOpt[Long] shouldEqual None
            (jso \ "sponsorBalance").asOpt[Long] shouldEqual None
            (jso \ "quantity").as[Long] shouldEqual tx.quantity.value
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

      withClue("over limit")(route.anyParamTest(routePath(s"/balance/${issuer.toAddress}"), "id")(Seq.fill(101)("aaa")*) {
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
          it.id() -> it.quantity.value
        }.toMap

        allBalances shouldEqual balancesAfterIssue
      })
    }
  }

  "/transfer" - {
    def posting[A: Writes](route: Route, v: A): RouteTestResult = Post(routePath("/transfer"), v).addHeader(ApiKeyHeader) ~> route

    "accepts TransferRequest" in routeTest() { (_, route) =>
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

    "accepts VersionedTransferRequest" in routeTest() { (_, route) =>
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

    "returns a error if it is not a transfer request" in routeTest() { (_, route) =>
      posting(route, Json.obj("key" -> "value")) ~> check {
        status shouldBe StatusCodes.BadRequest
      }
    }
  }

  routePath(s"/details/{id} - issued by invoke expression") in routeTest(DomainPresets.ContinuationTransaction) { (d, route) =>
    val tx = TxHelpers.invokeExpression(
      expression = TestCompiler(V6).compileFreeCall(
        s"""
           |let t = Issue("${assetDesc.name.toStringUtf8}", "${assetDesc.description.toStringUtf8}", ${assetDesc.totalVolume}, ${assetDesc.decimals}, ${assetDesc.reissuable})
           |[
           |  t,
           |  BinaryEntry("assetId", calculateAssetId(t))
           |]""".stripMargin
      ),
      fee = 1.01.waves
    )

    d.appendBlock(TxHelpers.genesis(tx.sender.toAddress))
    d.appendBlock(tx)

    val assetId = d.blockchain
      .accountData(tx.sender.toAddress, "assetId")
      .collect { case i: BinaryDataEntry =>
        i.value
      }
      .get

    checkDetails(d, route, tx, assetId.toString, assetDesc)
  }

  routePath(s"/details/{id} - issued by Ethereum transaction") in routeTest(DomainPresets.RideV6) { (d, route) =>
    val tx = EthTxGenerator.generateEthInvoke(
      keyPair = TxHelpers.defaultEthSigner,
      address = defaultSigner.toAddress,
      funcName = "issue",
      args = Seq(
        Arg.Str(assetDesc.name.toStringUtf8),
        Arg.Str(assetDesc.description.toStringUtf8),
        Arg.Integer(assetDesc.totalVolume.toInt),
        Arg.Integer(assetDesc.decimals),
        Arg.Bool(assetDesc.reissuable)
      ),
      payments = Seq.empty,
      fee = 1.01.waves
    )

    d.appendBlock(TxHelpers.genesis(tx.sender.toAddress), TxHelpers.genesis(defaultSigner.toAddress))
    d.appendBlock(setScriptTransaction(defaultSigner), tx)

    val assetId = d.blockchain
      .accountData(defaultSigner.toAddress, "assetId")
      .collect { case i: BinaryDataEntry =>
        i.value
      }
      .get

    checkDetails(d, route, tx, assetId.toString, assetDesc)
  }

  routePath(s"/details/{id} - smart asset") in routeTest() { (d, route) =>
    val issuer = TxHelpers.signer(1)
    val script = ExprScript(CONST_BOOLEAN(true)).explicitGet()
    val assetDescr = assetDesc.copy(
      script = Some(
        AssetScriptInfo(
          script,
          Script.estimate(script, ScriptEstimatorV1, fixEstimateOfVerifier = true, useContractVerifierLimit = false).explicitGet()
        )
      ),
      issuer = issuer.publicKey
    )

    val genesis = TxHelpers.genesis(issuer.toAddress)
    val issue   = TxHelpers.issue(issuer, 100, script = Some(script))

    d.appendBlock(genesis)
    d.appendBlock(issue)

    checkDetails(d, route, issue, issue.id().toString, assetDescr)
  }

  routePath(s"/details/{id} - non-smart asset") in routeTest() { (d, route) =>
    val tx = issueTransaction()

    d.appendBlock(TxHelpers.genesis(tx.sender.toAddress))
    d.appendBlock(tx)

    checkDetails(d, route, tx, tx.id().toString, assetDesc)
  }

  routePath("/{assetId}/distribution/{height}/limit/{limit}") in routeTest() { (d, route) =>
    val issuer           = testWallet.generateNewAccount().get
    val issueTransaction = TxHelpers.issue(issuer, 100_0000, 4, "PA_01")
    d.appendBlock(TxHelpers.genesis(issuer.toAddress, 10.waves))
    val recipients = testWallet.generateNewAccounts(5)
    val transfers = recipients.zipWithIndex.map { case (kp, i) =>
      MassTransferTransaction.ParsedTransfer(kp.toAddress, TxNonNegativeAmount.unsafeFrom((i + 1) * 10000))
    }
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
        (transfers.map(pt => pt.address.toString -> (pt.amount.value: JsValueWrapper)) :+
          issuer.toAddress.toString -> (issueTransaction.quantity.value - transfers.map(_.amount.value).sum: JsValueWrapper))*
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

  routePath(s"/details/{id}") in routeTest() { (d, route) =>
    val sender = testWallet.generateNewAccount().get

    d.appendBlock(GenesisTransaction.create(sender.toAddress, 100.waves, System.currentTimeMillis()).explicitGet())

    forAll(nonNftTestData) { case (version, reissuable, script) =>
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
            issueTransaction.decimals.value,
            reissuable,
            issueTransaction.quantity.value,
            Height(d.blockchain.height),
            script.map(s => AssetScriptInfo(s, 1L)),
            0L,
            nft = false
          ),
          issueTransaction.id().toString,
          responseAs[Seq[JsObject]].head
        )
      }
    }
  }

  routePath("/nft/list") in routeTest() { (d, route) =>
    val issuer = testWallet.generateNewAccount().get
    val nfts = Seq.tabulate(5) { i =>
      TxHelpers.issue(issuer, 1, name = s"NFT_0$i", reissuable = false, fee = 0.001.waves)
    }
    d.appendBlock(TxHelpers.genesis(issuer.toAddress, 100.waves))
    val nonNFT = TxHelpers.issue(issuer, 100, 2.toByte)
    d.appendBlock((nfts :+ nonNFT)*)

    Get(routePath(s"/balance/${issuer.toAddress}/${nonNFT.id()}")) ~> route ~> check {
      val balance = responseAs[JsObject]
      (balance \ "address").as[String] shouldEqual issuer.toAddress.toString
      (balance \ "balance").as[Long] shouldEqual nonNFT.quantity.value
      (balance \ "assetId").as[String] shouldEqual nonNFT.id().toString
    }

    Get(routePath(s"/nft/${issuer.toAddress}/limit/6")) ~> route ~> check {
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

  private def checkDetails(domain: Domain, route: Route, tx: Transaction, assetId: String, assetDesc: AssetDescription): Unit = {
    domain.liquidAndSolidAssert { () =>
      Get(routePath(s"/details/$assetId")) ~> route ~> check {
        val response = responseAs[JsObject]
        checkResponse(tx, assetDesc, assetId, response)
      }
      Get(routePath(s"/details?id=$assetId")) ~> route ~> check {
        val responses = responseAs[List[JsObject]]
        responses.foreach(response => checkResponse(tx, assetDesc, assetId, response))
      }
      Post(routePath("/details"), Json.obj("ids" -> List(s"$assetId"))) ~> route ~> check {
        val responses = responseAs[List[JsObject]]
        responses.foreach(response => checkResponse(tx, assetDesc, assetId, response))
      }
    }
  }

  private def checkResponse(tx: Transaction, desc: AssetDescription, assetId: String, response: JsObject): Unit = {
    (response \ "assetId").as[String] shouldBe assetId
    (response \ "issueTimestamp").as[Long] shouldBe tx.timestamp
    (response \ "issuer").as[String] shouldBe desc.issuer.toAddress.toString
    (response \ "name").as[String] shouldBe desc.name.toStringUtf8
    (response \ "description").as[String] shouldBe desc.description.toStringUtf8
    (response \ "decimals").as[Int] shouldBe desc.decimals
    (response \ "reissuable").as[Boolean] shouldBe desc.reissuable
    (response \ "quantity").as[BigDecimal] shouldBe desc.totalVolume
    (response \ "minSponsoredAssetFee").asOpt[Long] shouldBe empty
    (response \ "originTransactionId").as[String] shouldBe tx.id().toString
  }
}
