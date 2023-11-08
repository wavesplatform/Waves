package com.wavesplatform.http

import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.HttpEntity.{Chunk, LastChunk}
import akka.http.scaladsl.model.headers.{Accept, `Content-Type`, `Transfer-Encoding`}
import akka.http.scaladsl.testkit.RouteTestTimeout
import akka.stream.scaladsl.Source
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.ApiError.{ApiKeyNotValid, DataKeysNotSpecified, TooBigArrayAllocation}
import com.wavesplatform.api.http.{AddressApiRoute, RouteTimeout}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.{AccountScriptInfo, Blockchain}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.{Schedulers, SharedSchedulerMixin}
import com.wavesplatform.wallet.Wallet
import io.netty.util.HashedWheelTimer
import monix.execution.schedulers.SchedulerService
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.*
import play.api.libs.json.Json.JsValueWrapper

import scala.concurrent.duration.*

class AddressRouteSpec extends RouteSpec("/addresses") with PathMockFactory with RestAPISettingsHelper with WithDomain with SharedSchedulerMixin {

  private val wallet = Wallet(WalletSettings(None, Some("123"), Some(ByteStr(Longs.toByteArray(System.nanoTime())))))
  wallet.generateNewAccounts(10)
  private val allAccounts  = wallet.privateKeyAccounts
  private val allAddresses = allAccounts.map(_.toAddress)
  private val blockchain   = stub[Blockchain]("globalBlockchain")
  (() => blockchain.activatedFeatures).when().returning(Map())

  private[this] val utxPoolSynchronizer = DummyTransactionPublisher.accepting

  private val commonAccountApi = mock[CommonAccountsApi]("globalAccountApi")

  private val timeLimited: SchedulerService = Schedulers.timeBoundedFixedPool(
    new HashedWheelTimer(),
    5.seconds,
    1,
    "rest-time-limited"
  )

  override def afterAll(): Unit = {
    timeLimited.shutdown()
    super.afterAll()
  }
  private val addressApiRoute: AddressApiRoute = AddressApiRoute(
    restAPISettings,
    wallet,
    blockchain,
    utxPoolSynchronizer,
    new TestTime,
    timeLimited,
    new RouteTimeout(60.seconds)(sharedScheduler),
    commonAccountApi,
    5
  )
  private val route = seal(addressApiRoute.route)

  private val generatedMessages = for {
    account <- Gen.oneOf(allAccounts).label("account")
    length  <- Gen.chooseNum(10, 1000)
    message <- Gen.listOfN(length, Gen.alphaNumChar).map(_.mkString).label("message")
  } yield (account, message)

  routePath("/balance/{address}/{confirmations}") in withDomain(balances = Seq(AddrWithBalance(TxHelpers.defaultAddress))) { d =>
    val route =
      addressApiRoute
        .copy(
          blockchain = d.blockchainUpdater,
          commonAccountsApi = CommonAccountsApi(
            () => d.blockchainUpdater.snapshotBlockchain,
            d.rdb,
            d.blockchainUpdater
          )
        )
        .route
    val address = TxHelpers.signer(1).toAddress

    for (_ <- 1 until 10) d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, address))

    Get(routePath(s"/balance/$address/10")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> "Unable to get balance past height 5")
    }

    Get(routePath(s"/balance?address=$address&height=1")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> "Unable to get balance past height 5")
    }
  }

  routePath("/balance") in withDomain(balances = Seq(AddrWithBalance(TxHelpers.defaultAddress))) { d =>
    val route =
      addressApiRoute
        .copy(
          blockchain = d.blockchainUpdater,
          commonAccountsApi = CommonAccountsApi(
            () => d.blockchainUpdater.snapshotBlockchain,
            d.rdb,
            d.blockchainUpdater
          )
        )
        .route
    val address       = TxHelpers.signer(1).toAddress
    val transferCount = 5

    val issue = TxHelpers.issue(TxHelpers.defaultSigner)
    d.appendBlock(issue)

    for (_ <- 1 until transferCount)
      d.appendBlock(
        TxHelpers.transfer(TxHelpers.defaultSigner, address, amount = 1),
        TxHelpers.transfer(TxHelpers.defaultSigner, address, asset = issue.asset, amount = 2)
      )

    Get(routePath(s"/balance?address=$address&height=$transferCount")) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(Json.obj("id" -> address.toString, "balance" -> (transferCount - 2)))
    }
    Post(routePath(s"/balance"), Json.obj("height" -> transferCount, "addresses" -> Seq(address.toString))) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(Json.obj("id" -> address.toString, "balance" -> (transferCount - 2)))
    }

    Get(routePath(s"/balance?address=$address&height=$transferCount&asset=${issue.assetId}")) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(Json.obj("id" -> address.toString, "balance" -> 2 * (transferCount - 2)))
    }
    Post(
      routePath(s"/balance"),
      Json.obj("height" -> transferCount, "addresses" -> Seq(address.toString), "asset" -> issue.assetId)
    ) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(Json.obj("id" -> address.toString, "balance" -> 2 * (transferCount - 2)))
    }
  }

  routePath("/seq/{from}/{to}") in {
    val r1 = Get(routePath("/seq/1/4")) ~> route ~> check {
      val response = responseAs[Seq[String]]
      response.length shouldBe 3
      allAddresses.map(_.toString) should contain allElementsOf response
      response
    }

    val r2 = Get(routePath("/seq/5/9")) ~> route ~> check {
      val response = responseAs[Seq[String]]
      response.length shouldBe 4
      allAddresses.map(_.toString) should contain allElementsOf response
      response
    }

    r1 shouldNot contain allElementsOf r2

    Get(routePath("/seq/1/9000")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj("error" -> 10, "message" -> "Too big sequence requested: max limit is 1000 entries")
    }

    Get(routePath("/seq/10/1")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> "Invalid sequence")
    }
  }

  routePath("/validate/{address}") in {
    val t = Table(("address", "valid"), (allAddresses.map(_ -> true) :+ "3P2HNUd5VUPLMQkJmctTPEeeHumiPN2GkTb" -> false)*)

    forAll(t) { (a, v) =>
      Get(routePath(s"/validate/$a")) ~> route ~> check {
        val r = responseAs[JsObject]
        (r \ "address").as[String] shouldEqual a.toString
        (r \ "valid").as[Boolean] shouldBe v
      }
    }
  }

  routePath("/seed/{address}") in {
    val account = allAccounts.head
    val path    = routePath(s"/seed/${account.toAddress}")
    Get(path) ~> route should produce(ApiKeyNotValid)
    Get(path) ~> ApiKeyHeader ~> route ~> check {
      val json = responseAs[JsObject]
      (json \ "address").as[String] shouldEqual account.toAddress.toString
      (json \ "seed").as[String] shouldEqual Base58.encode(account.seed)
    }
  }

  private def testSign(path: String, encode: Boolean): Unit =
    forAll(generatedMessages) { case (account, message) =>
      val uri = routePath(s"/$path/${account.toAddress}")
      Post(uri, message) ~> route should produce(ApiKeyNotValid)
      Post(uri, message) ~> ApiKeyHeader ~> route ~> check {
        val resp      = responseAs[JsObject]
        val signature = ByteStr.decodeBase58((resp \ "signature").as[String]).get

        (resp \ "message").as[String] shouldEqual (if (encode) Base58.encode(message.getBytes("UTF-8")) else message)
        (resp \ "publicKey").as[String] shouldEqual account.publicKey.toString

        crypto.verify(signature, message.getBytes("UTF-8"), account.publicKey) shouldBe true
      }
    }

  routePath("/sign/{address}") in testSign("sign", true)
  routePath("/signText/{address}") in testSign("signText", false)

  private def testVerify(path: String, encode: Boolean): Unit = {

    forAll(generatedMessages.flatMap(m => Gen.oneOf(true, false).map(b => (m, b)))) { case ((account, message), b58) =>
      val uri          = routePath(s"/$path/${account.toAddress}")
      val messageBytes = message.getBytes("UTF-8")
      val signature    = crypto.sign(account.privateKey, messageBytes)
      val validBody = Json.obj(
        "message"   -> JsString(if (encode) if (b58) Base58.encode(messageBytes) else "base64:" ++ Base64.encode(messageBytes) else message),
        "publickey" -> JsString(Base58.encode(account.publicKey.arr)),
        "signature" -> JsString(signature.toString)
      )

      val emptySignature =
        Json.obj("message" -> JsString(""), "publickey" -> JsString(Base58.encode(account.publicKey.arr)), "signature" -> JsString(""))

      Post(uri, validBody) ~> route should produce(ApiKeyNotValid)
      Post(uri, emptySignature) ~> ApiKeyHeader ~> route ~> check {
        (responseAs[JsObject] \ "valid").as[Boolean] shouldBe false
      }
      Post(uri, validBody) ~> ApiKeyHeader ~> route ~> check {
        (responseAs[JsObject] \ "valid").as[Boolean] shouldBe true
      }
    }
  }
  routePath("/verifyText/{address}") in testVerify("verifyText", false)
  routePath("/verify/{address}") in testVerify("verify", true)

  routePath("") in {
    Post(routePath("")) ~> route should produce(ApiKeyNotValid)
    Post(routePath("")) ~> ApiKeyHeader ~> route ~> check {
      allAddresses should not contain (responseAs[JsObject] \ "address").as[String]
    }
  }

  routePath("/{address}") in {
    Delete(routePath(s"/${allAddresses.head}")) ~> ApiKeyHeader ~> route ~> check {
      (responseAs[JsObject] \ "deleted").as[Boolean] shouldBe true
    }
  }

  routePath(s"/scriptInfo/${allAddresses(1)}") in {
    val script = ExprScript(TRUE).explicitGet()

    (commonAccountApi.script _).expects(allAccounts(1).toAddress).returning(Some(AccountScriptInfo(allAccounts(1).publicKey, script, 123L))).once()
    (blockchain.accountScript _).when(allAccounts(1).toAddress).returns(Some(AccountScriptInfo(allAccounts(1).publicKey, script, 123L))).once()
    (blockchain.hasAccountScript _).when(allAccounts(1).toAddress).returns(true).once()

    Get(routePath(s"/scriptInfo/${allAddresses(1)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(1).toString
      (response \ "script").as[String] shouldBe "base64:AQa3b8tH"
      (response \ "scriptText").as[String] shouldBe "true"
      (response \ "version").as[Int] shouldBe 1
      (response \ "complexity").as[Long] shouldBe 123
      (response \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    (commonAccountApi.script _).expects(allAccounts(2).toAddress).returning(None).once()
    (blockchain.accountScript _).when(allAccounts(2).toAddress).returns(None).once()
    (blockchain.hasAccountScript _).when(allAccounts(2).toAddress).returns(false).once()

    Get(routePath(s"/scriptInfo/${allAddresses(2)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(2).toString
      (response \ "script").asOpt[String] shouldBe None
      (response \ "scriptText").asOpt[String] shouldBe None
      (response \ "version").asOpt[Int] shouldBe None
      (response \ "complexity").as[Long] shouldBe 0
      (response \ "extraFee").as[Long] shouldBe 0
    }

    val contractWithMeta = DApp(
      meta = DAppMeta(
        version = 1,
        List(
          CallableFuncSignature(ByteString.copyFrom(Array[Byte](1, 2, 3))),
          CallableFuncSignature(ByteString.copyFrom(Array[Byte](8))),
          CallableFuncSignature(ByteString.EMPTY)
        )
      ),
      decs = List(),
      callableFuncs = List(
        CallableFunction(
          CallableAnnotation("i"),
          FUNC("call1", List("a", "b", "c"), CONST_BOOLEAN(true))
        ),
        CallableFunction(
          CallableAnnotation("i"),
          FUNC("call2", List("d"), CONST_BOOLEAN(true))
        ),
        CallableFunction(
          CallableAnnotation("i"),
          FUNC("call3", Nil, CONST_BOOLEAN(true))
        )
      ),
      verifierFuncOpt = Some(VerifierFunction(VerifierAnnotation("t"), FUNC("verify", List(), TRUE)))
    )

    val contractScript       = ContractScript(V3, contractWithMeta).explicitGet()
    val callableComplexities = Map("a" -> 1L, "b" -> 2L, "c" -> 3L, "d" -> 100L, "verify" -> 11L)
    (commonAccountApi.script _)
      .expects(allAccounts(3).toAddress)
      .returning(Some(AccountScriptInfo(allAccounts(3).publicKey, contractScript, 11L)))
      .once()
    (blockchain.accountScript _)
      .when(allAccounts(3).toAddress)
      .returns(Some(AccountScriptInfo(allAccounts(3).publicKey, contractScript, 11L, complexitiesByEstimator = Map(1 -> callableComplexities))))
    (blockchain.hasAccountScript _).when(allAccounts(3).toAddress).returns(true).once()

    Get(routePath(s"/scriptInfo/${allAddresses(3)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(3).toString
      (response \ "script").as[String] should fullyMatch regex "base64:.+".r
      (response \ "scriptText").as[String] should fullyMatch regex "DApp\\(.+\\)".r
      (response \ "version").as[Int] shouldBe 3
      (response \ "complexity").as[Long] shouldBe 100
      (response \ "verifierComplexity").as[Long] shouldBe 11
      (response \ "callableComplexities").as[Map[String, Long]] shouldBe callableComplexities - "verify"
      (response \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Get(routePath(s"/scriptInfo/${allAddresses(3)}/meta")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(3).toString
      (response \ "meta" \ "version").as[String] shouldBe "1"
      (response \ "meta" \ "callableFuncTypes" \ "call1" \ 0 \ "name").as[String] shouldBe "a"
      (response \ "meta" \ "callableFuncTypes" \ "call1" \ 0 \ "type").as[String] shouldBe "Int"
      (response \ "meta" \ "callableFuncTypes" \ "call1" \ 1 \ "name").as[String] shouldBe "b"
      (response \ "meta" \ "callableFuncTypes" \ "call1" \ 1 \ "type").as[String] shouldBe "ByteVector"
      (response \ "meta" \ "callableFuncTypes" \ "call1" \ 2 \ "name").as[String] shouldBe "c"
      (response \ "meta" \ "callableFuncTypes" \ "call1" \ 2 \ "type").as[String] shouldBe "ByteVector|Int"
      (response \ "meta" \ "callableFuncTypes" \ "call2" \ 0 \ "name").as[String] shouldBe "d"
      (response \ "meta" \ "callableFuncTypes" \ "call2" \ 0 \ "type").as[String] shouldBe "String"
      (response \ "meta" \ "callableFuncTypes" \ "call3").as[JsArray] shouldBe JsArray()
    }

    val contractWithoutMeta = contractWithMeta.copy(meta = DAppMeta())
    (blockchain.accountScript _)
      .when(allAccounts(4).toAddress)
      .onCall((_: Address) => Some(AccountScriptInfo(allAccounts(4).publicKey, ContractScript(V3, contractWithoutMeta).explicitGet(), 11L)))

    Get(routePath(s"/scriptInfo/${allAddresses(4)}/meta")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(4).toString
      (response \ "meta" \ "version").as[String] shouldBe "0"
    }

    (blockchain.accountScript _)
      .when(allAccounts(5).toAddress)
      .onCall((_: Address) => Thread.sleep(100000).asInstanceOf[Nothing])

    implicit val routeTestTimeout = RouteTestTimeout(10.seconds)
    implicit val timeout          = routeTestTimeout.duration
    Get(routePath(s"/scriptInfo/${allAddresses(5)}")) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "message").as[String] shouldBe "The request took too long to complete"
    }

    val contractWithoutVerifier             = contractWithMeta.copy(verifierFuncOpt = None)
    val contractWithoutVerifierComplexities = Map("a" -> 1L, "b" -> 2L, "c" -> 3L)
    (blockchain.accountScript _)
      .when(allAccounts(6).toAddress)
      .onCall((_: Address) =>
        Some(
          AccountScriptInfo(
            allAccounts(6).publicKey,
            ContractScript(V3, contractWithoutVerifier).explicitGet(),
            0L,
            complexitiesByEstimator = Map(1 -> contractWithoutVerifierComplexities)
          )
        )
      )
    (blockchain.hasAccountScript _).when(allAccounts(6).toAddress).returns(true).once()

    Get(routePath(s"/scriptInfo/${allAddresses(6)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(6).toString
      (response \ "version").as[Int] shouldBe 3
      (response \ "complexity").as[Long] shouldBe 3
      (response \ "verifierComplexity").as[Long] shouldBe 0
      (response \ "callableComplexities").as[Map[String, Long]] shouldBe contractWithoutVerifierComplexities
      (response \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }
  }

  routePath(s"/scriptInfo/ after ${BlockchainFeatures.SynchronousCalls}") in {
    val blockchain = stub[Blockchain]("blockchain")
    val route      = seal(addressApiRoute.copy(blockchain = blockchain).route)
    (() => blockchain.activatedFeatures).when().returning(Map(BlockchainFeatures.SynchronousCalls.id -> 0))

    val script                            = ExprScript(TRUE).explicitGet()
    def info(complexity: Int, index: Int) = Some(AccountScriptInfo(allAccounts(index).publicKey, script, complexity))

    (blockchain.accountScript _).when(allAddresses(1)).returns(info(201, 1))
    Get(routePath(s"/scriptInfo/${allAddresses(1)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(1).toString
      (response \ "version").as[Int] shouldBe 1
      (response \ "complexity").as[Long] shouldBe 201
      (response \ "verifierComplexity").as[Long] shouldBe 201
      (response \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    (blockchain.accountScript _).when(allAddresses(2)).returns(info(199, 2))
    Get(routePath(s"/scriptInfo/${allAddresses(2)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(2).toString
      (response \ "version").as[Int] shouldBe 1
      (response \ "complexity").as[Long] shouldBe 199
      (response \ "verifierComplexity").as[Long] shouldBe 199
      (response \ "extraFee").as[Long] shouldBe 0
    }

    (blockchain.accountScript _).when(allAddresses(3)).returns(None)
    Get(routePath(s"/scriptInfo/${allAddresses(3)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(3).toString
      (response \ "version").asOpt[Int] shouldBe None
      (response \ "complexity").as[Long] shouldBe 0
      (response \ "verifierComplexity").as[Long] shouldBe 0
      (response \ "extraFee").as[Long] shouldBe 0
    }
  }

  routePath(s"/data/${allAddresses(1)}?matches=regex") in {
    val invalidRegexps = List("[a-z", "([a-z]{0}", "[a-z]{0", "[a-z]{,5}")
    for (regex <- invalidRegexps) {
      Get(routePath(s"""/data/${allAddresses(1)}?matches=$regex""")) ~> route ~> check {
        responseAs[String] should include("Cannot compile regex")
      }
    }
  }

  routePath(s"/data/{address} with Transfer-Encoding: chunked") in {
    val account = TxHelpers.signer(1)

    withDomain(DomainPresets.RideV5, balances = AddrWithBalance.enoughBalances(account)) { d =>
      d.appendBlock(TxHelpers.dataSingle(account))

      val route =
        addressApiRoute
          .copy(
            blockchain = d.blockchainUpdater,
            commonAccountsApi = CommonAccountsApi(
              () => d.blockchainUpdater.snapshotBlockchain,
              d.rdb,
              d.blockchainUpdater
            )
          )
          .route

      val requestBody = Json.obj("keys" -> Seq("test"))

      val headers: Seq[HttpHeader] =
        Seq(`Transfer-Encoding`(TransferEncodings.chunked), `Content-Type`(ContentTypes.`application/json`), Accept(MediaTypes.`application/json`))

      Post(
        routePath(s"/data/${account.toAddress}"),
        HttpEntity.Chunked(ContentTypes.`application/json`, Source(Seq(Chunk(akka.util.ByteString.fromString(requestBody.toString)), LastChunk)))
      ).withHeaders(headers) ~> route ~> check {
        responseAs[JsValue] should matchJson("""[{"key":"test","type":"string","value":"test"}]""")
      }
    }
  }

  routePath(s"/data/{address} - handles keys limit") in {
    def checkErrorResponse(): Unit = {
      response.status shouldBe StatusCodes.BadRequest
      (responseAs[JsObject] \ "message").as[String] shouldBe TooBigArrayAllocation(addressApiRoute.settings.dataKeysRequestLimit).message
    }

    def checkResponse(key: String, value: String, idsCount: Int): Unit = {
      response.status shouldBe StatusCodes.OK

      val result = responseAs[JsArray].value
      result.size shouldBe idsCount
      (1 to idsCount).zip(responseAs[JsArray].value) foreach { case (_, json) =>
        json should matchJson(s"""
                                 |{
                                 |  "key" : "$key",
                                 |  "type" : "string",
                                 |  "value" : "$value"
                                 |}
                                 |""".stripMargin)
      }
    }

    val account = TxHelpers.signer(1)
    val key     = "testKey"
    val value   = "testValue"

    withDomain(DomainPresets.RideV5, balances = AddrWithBalance.enoughBalances(account)) { d =>
      d.appendBlock(TxHelpers.dataSingle(account, key = key, value = value))

      val route =
        addressApiRoute
          .copy(
            blockchain = d.blockchainUpdater,
            commonAccountsApi = CommonAccountsApi(
              () => d.blockchainUpdater.snapshotBlockchain,
              d.rdb,
              d.blockchainUpdater
            )
          )
          .route

      val maxLimitKeys      = Seq.fill(addressApiRoute.settings.dataKeysRequestLimit)(key)
      val moreThanLimitKeys = key +: maxLimitKeys

      Get(routePath(s"/data/${account.toAddress}?${maxLimitKeys.map("key=" + _).mkString("&")}")) ~> route ~> check(
        checkResponse(key, value, maxLimitKeys.size)
      )
      Get(routePath(s"/data/${account.toAddress}?${moreThanLimitKeys.map("key=" + _).mkString("&")}")) ~> route ~> check(
        checkErrorResponse()
      )

      Post(routePath(s"/data/${account.toAddress}"), FormData(maxLimitKeys.map("key" -> _)*)) ~> route ~> check(
        checkResponse(key, value, maxLimitKeys.size)
      )
      Post(routePath(s"/data/${account.toAddress}"), FormData(moreThanLimitKeys.map("key" -> _)*)) ~> route ~> check(
        checkErrorResponse()
      )

      Post(
        routePath(s"/data/${account.toAddress}"),
        HttpEntity(ContentTypes.`application/json`, Json.obj("keys" -> Json.arr(maxLimitKeys.map(key => key: JsValueWrapper)*)).toString())
      ) ~> route ~> check(checkResponse(key, value, maxLimitKeys.size))
      Post(
        routePath(s"/data/${account.toAddress}"),
        HttpEntity(ContentTypes.`application/json`, Json.obj("keys" -> Json.arr(moreThanLimitKeys.map(key => key: JsValueWrapper)*)).toString())
      ) ~> route ~> check(checkErrorResponse())
    }
  }

  routePath(s"/data/{address} - handles empty keys input in POST") in {
    def checkErrorResponse(): Unit = {
      response.status shouldBe StatusCodes.BadRequest
      (responseAs[JsObject] \ "message").as[String] shouldBe DataKeysNotSpecified.message
    }

    val account = TxHelpers.signer(1)

    Post(routePath(s"/data/${account.toAddress}"), FormData()) ~> route ~> check(checkErrorResponse())

    Post(
      routePath(s"/data/${account.toAddress}"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("keys" -> JsArray.empty).toString())
    ) ~> route ~> check(checkErrorResponse())
  }
}
