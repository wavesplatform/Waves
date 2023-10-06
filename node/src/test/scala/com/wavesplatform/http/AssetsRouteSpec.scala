package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, FormData, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Route
import com.google.protobuf.ByteString
import com.wavesplatform.TestWallet
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.ApiError.{AssetIdNotSpecified, AssetsDoesNotExist, InvalidIds, TooBigArrayAllocation}
import com.wavesplatform.api.http.RouteTimeout
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
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
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.EthTxGenerator.Arg
import com.wavesplatform.transaction.{AssetIdLength, EthTxGenerator, GenesisTransaction, Transaction, TxHelpers, TxNonNegativeAmount, TxVersion}
import com.wavesplatform.utils.SharedSchedulerMixin
import org.scalatest.concurrent.Eventually
import play.api.libs.json.*
import play.api.libs.json.Json.JsValueWrapper

import scala.concurrent.duration.*

class AssetsRouteSpec
    extends RouteSpec("/assets")
    with Eventually
    with RestAPISettingsHelper
    with WithDomain
    with TestWallet
    with SharedSchedulerMixin {
  private val MaxDistributionDepth = 1

  def routeTest[A](
      settings: WavesSettings = DomainPresets.RideV4.addFeatures(BlockchainFeatures.ReduceNFTFee),
      balances: Seq[AddrWithBalance] = Seq.empty
  )(f: (Domain, Route) => A): A =
    withDomain(settings, balances) { d =>
      f(
        d,
        seal(
          AssetsApiRoute(
            restAPISettings,
            testWallet,
            DummyTransactionPublisher.accepting,
            d.blockchain,
            () => d.blockchain.snapshotBlockchain,
            TestTime(),
            d.accountsApi,
            d.assetsApi,
            MaxDistributionDepth,
            new RouteTimeout(60.seconds)(sharedScheduler)
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
    nft = false,
    1,
    Height(1)
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

  routePath(s"/details/{id} - non-smart asset") in routeTest(RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { (d, route) =>
    val issues = (1 to 10).map(i => (i, issueTransaction())).toMap

    d.appendBlock()
    d.appendMicroBlock(issues(1))
    checkDetails(route, issues(1), issues(1).id().toString, assetDesc)

    (2 to 6).foreach { i =>
      d.appendMicroBlock(issues(i))
      checkDetails(route, issues(i), issues(i).id().toString, assetDesc.copy(sequenceInBlock = i))
    }

    d.appendKeyBlock()
    (1 to 6).foreach { i =>
      checkDetails(route, issues(i), issues(i).id().toString, assetDesc.copy(sequenceInBlock = i))
    }

    d.appendBlock((7 to 10).map(issues): _*)
    (1 to 6).foreach { i =>
      checkDetails(route, issues(i), issues(i).id().toString, assetDesc.copy(sequenceInBlock = i))
    }
    (7 to 10).foreach { i =>
      checkDetails(route, issues(i), issues(i).id().toString, assetDesc.copy(sequenceInBlock = i - 6, issueHeight = Height @@ 2))
    }
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
            nft = false,
            1,
            Height(1)
          ),
          issueTransaction.id().toString,
          responseAs[Seq[JsObject]].head
        )
      }
    }
  }

  routePath(s"/details - handles assets ids limit") in routeTest() { (d, route) =>
    val inputLimitErrMsg = TooBigArrayAllocation(restAPISettings.assetDetailsLimit).message
    val emptyInputErrMsg = AssetIdNotSpecified.message

    def checkErrorResponse(errMsg: String): Unit = {
      response.status shouldBe StatusCodes.BadRequest
      (responseAs[JsObject] \ "message").as[String] shouldBe errMsg
    }

    def checkResponse(issueTx: IssueTransaction, idsCount: Int): Unit = {
      response.status shouldBe StatusCodes.OK

      val result = responseAs[JsArray].value
      result.size shouldBe idsCount
      (1 to idsCount).zip(responseAs[JsArray].value) foreach { case (_, json) =>
        json should matchJson(s"""
                                 |{
                                 |  "assetId" : "${issueTx.id()}",
                                 |  "issueHeight" : 2,
                                 |  "issueTimestamp" : ${issueTx.timestamp},
                                 |  "issuer" : "${issueTx.sender.toAddress}",
                                 |  "issuerPublicKey" : "${issueTx.sender.toString}",
                                 |  "name" : "${issueTx.name.toStringUtf8}",
                                 |  "description" : "${issueTx.description.toStringUtf8}",
                                 |  "decimals" : ${issueTx.decimals.value},
                                 |  "reissuable" : ${issueTx.reissuable},
                                 |  "quantity" : ${issueTx.quantity.value},
                                 |  "scripted" : false,
                                 |  "minSponsoredAssetFee" : null,
                                 |  "originTransactionId" : "${issueTx.id()}",
                                 |  "sequenceInBlock" : 1
                                 |}
                                 |""".stripMargin)
      }
    }

    val issuer = TxHelpers.signer(1)

    val issue = TxHelpers.issue(issuer = issuer)

    d.appendBlock(TxHelpers.genesis(issuer.toAddress))
    d.appendBlock(issue)

    val maxLimitIds      = Seq.fill(restAPISettings.assetDetailsLimit)(issue.id().toString)
    val moreThanLimitIds = issue.id().toString +: maxLimitIds

    Get(routePath(s"/details?${maxLimitIds.map("id=" + _).mkString("&")}")) ~> route ~> check(checkResponse(issue, maxLimitIds.size))
    Get(routePath(s"/details?${moreThanLimitIds.map("id=" + _).mkString("&")}")) ~> route ~> check(checkErrorResponse(inputLimitErrMsg))
    Get(routePath("/details")) ~> route ~> check(checkErrorResponse(emptyInputErrMsg))

    Post(routePath("/details"), FormData(maxLimitIds.map("id" -> _)*)) ~> route ~> check(checkResponse(issue, maxLimitIds.size))
    Post(routePath("/details"), FormData(moreThanLimitIds.map("id" -> _)*)) ~> route ~> check(checkErrorResponse(inputLimitErrMsg))
    Post(routePath("/details"), FormData()) ~> route ~> check(checkErrorResponse(emptyInputErrMsg))

    Post(
      routePath("/details"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(maxLimitIds.map(id => id: JsValueWrapper)*)).toString())
    ) ~> route ~> check(checkResponse(issue, maxLimitIds.size))
    Post(
      routePath("/details"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(moreThanLimitIds.map(id => id: JsValueWrapper)*)).toString())
    ) ~> route ~> check(checkErrorResponse(inputLimitErrMsg))
    Post(
      routePath("/details"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> JsArray.empty).toString())
    ) ~> route ~> check(checkErrorResponse(emptyInputErrMsg))
  }

  routePath(s"/details - handles not existed assets error") in routeTest() { (_, route) =>
    val unexistedAssetIds = Seq(
      ByteStr.fill(AssetIdLength)(1),
      ByteStr.fill(AssetIdLength)(2)
    ).map(IssuedAsset.apply)

    def checkErrorResponse(): Unit = {
      response.status shouldBe StatusCodes.BadRequest
      (responseAs[JsObject] \ "message").as[String] shouldBe AssetsDoesNotExist(unexistedAssetIds).message
      (responseAs[JsObject] \ "ids").as[Seq[String]] shouldBe unexistedAssetIds.map(_.id.toString)
    }

    Get(routePath(s"/details?${unexistedAssetIds.map("id=" + _.id.toString).mkString("&")}")) ~> route ~> check(checkErrorResponse())

    Post(routePath("/details"), FormData(unexistedAssetIds.map("id" -> _.id.toString)*)) ~> route ~> check(checkErrorResponse())

    Post(
      routePath("/details"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(unexistedAssetIds.map(id => id: JsValueWrapper)*)).toString())
    ) ~> route ~> check(checkErrorResponse())
  }

  routePath(s"/details - handles invalid asset ids") in routeTest() { (_, route) =>
    val invalidAssetIds = Seq(
      ByteStr.fill(AssetIdLength)(1),
      ByteStr.fill(AssetIdLength)(2)
    ).map(bs => s"${bs}0")

    def checkErrorResponse(): Unit = {
      response.status shouldBe StatusCodes.BadRequest
      (responseAs[JsObject] \ "message").as[String] shouldBe InvalidIds(invalidAssetIds).message
      (responseAs[JsObject] \ "ids").as[Seq[String]] shouldBe invalidAssetIds
    }

    Get(routePath(s"/details?${invalidAssetIds.map("id=" + _).mkString("&")}")) ~> route ~> check(checkErrorResponse())

    Post(routePath("/details"), FormData(invalidAssetIds.map("id" -> _)*)) ~> route ~> check(checkErrorResponse())

    Post(
      routePath("/details"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(invalidAssetIds.map(id => id: JsValueWrapper)*)).toString())
    ) ~> route ~> check(checkErrorResponse())
  }

  routePath("/nft/list") - {
    "NFTs in 1 block" in {
      routeTest() { (d, route) =>
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
    }
    "NFTs in multiple blocks" in {
      routeTest() { (d, route) =>
        d.appendBlock(genesis(secondAddress, 100.waves))
        val indexes =
          (1 to 5).flatMap { block =>
            val txs = (0 to 9).map { count =>
              val i   = block * 10 + count
              val tx1 = issue(defaultSigner, 1, name = s"NFT$i", reissuable = false)
              val tx2 = issue(secondSigner, 1, name = s"NFT$i", reissuable = false)
              (i, Seq(tx1, tx2))
            }
            d.appendBlock(txs.flatMap(_._2)*)
            txs.map(_._1)
          }
        Seq(defaultAddress, secondAddress).foreach { address =>
          Get(routePath(s"/nft/$address/limit/50")) ~> route ~> check {
            val nftList = responseAs[Seq[JsObject]]
            nftList.size shouldEqual 50
            nftList.map { jso => (jso \ "name").as[String].drop(3).toInt } shouldBe indexes
          }
        }
      }
    }
  }

  private def checkDetails(domain: Domain, route: Route, tx: Transaction, assetId: String, assetDesc: AssetDescription): Unit = {
    domain.liquidAndSolidAssert { () =>
      checkDetails(route, tx, assetId, assetDesc)
    }
  }

  private def checkDetails(route: Route, tx: Transaction, assetId: String, assetDesc: AssetDescription): Unit = {
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
    (response \ "sequenceInBlock").as[Int] shouldBe desc.sequenceInBlock
  }
}
