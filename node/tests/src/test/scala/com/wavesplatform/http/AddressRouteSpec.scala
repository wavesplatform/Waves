package com.wavesplatform.http

import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.HttpEntity.{Chunk, LastChunk}
import akka.http.scaladsl.model.headers.{Accept, `Content-Type`, `Transfer-Encoding`}
import akka.stream.scaladsl.Source
import com.google.common.primitives.Longs
import com.wavesplatform.api.http.ApiError.{ApiKeyNotValid, DataKeysNotSpecified, TooBigArrayAllocation}
import com.wavesplatform.api.http.{AddressApiRoute, RouteTimeout}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.db.WithState
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.{V5, V6}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.{Schedulers, SharedSchedulerMixin}
import com.wavesplatform.wallet.Wallet
import io.netty.util.HashedWheelTimer
import monix.execution.schedulers.SchedulerService
import play.api.libs.json.*
import play.api.libs.json.Json.JsValueWrapper

import scala.concurrent.duration.*

class AddressRouteSpec extends RouteSpec("/addresses") with RestAPISettingsHelper with SharedDomain with SharedSchedulerMixin {

  private val richAccount = TxHelpers.signer(0xaaff)

  override def settings: WavesSettings                         = DomainPresets.RideV6.copy(restAPISettings = restAPISettings)
  override def genesisBalances: Seq[WithState.AddrWithBalance] = Seq(AddrWithBalance(richAccount.toAddress, 10_000.waves))

  private val wallet = Wallet(WalletSettings(None, Some("123"), Some(ByteStr(Longs.toByteArray(System.nanoTime())))))
  wallet.generateNewAccounts(10)
  private val allAccounts  = wallet.privateKeyAccounts
  private val allAddresses = allAccounts.map(_.toAddress)

  private val utxPoolSynchronizer = DummyTransactionPublisher.accepting

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

  private val MaxBalanceDepth = 5

  private val route = seal(
    AddressApiRoute(
      restAPISettings,
      wallet,
      domain.blockchain,
      utxPoolSynchronizer,
      new TestTime,
      timeLimited,
      new RouteTimeout(60.seconds)(sharedScheduler),
      domain.accountsApi,
      MaxBalanceDepth
    ).route
  )

  routePath("/balance/{address}/{confirmations}") in {
    val address = TxHelpers.signer(1).toAddress

    for (_ <- 1 until 10) domain.appendBlock(TxHelpers.transfer(richAccount, address))

    val height        = domain.blockchain.height
    val minimumHeight = height - MaxBalanceDepth

    Get(routePath(s"/balance/$address/${MaxBalanceDepth + 1}")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> s"Unable to get balance past height $minimumHeight")
    }

    Get(routePath(s"/balance?address=$address&height=1")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> s"Unable to get balance past height $minimumHeight")
    }
  }

  routePath("/balance") in {
    val address       = TxHelpers.address(0xaaff01)
    val transferCount = 4

    val issue = TxHelpers.issue(richAccount)
    domain.appendBlock(issue)

    for (_ <- 1 to transferCount)
      domain.appendBlock(
        TxHelpers.transfer(richAccount, address, amount = 1),
        TxHelpers.transfer(richAccount, address, asset = issue.asset, amount = 2)
      )

    val balanceCheckHeight = domain.blockchain.height

    Get(routePath(s"/balance?address=$address&height=$balanceCheckHeight")) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(Json.obj("id" -> address.toString, "balance" -> transferCount))
    }
    Post(routePath(s"/balance"), Json.obj("height" -> balanceCheckHeight, "addresses" -> Seq(address.toString))) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(Json.obj("id" -> address.toString, "balance" -> transferCount))
    }

    Get(routePath(s"/balance?address=$address&height=$balanceCheckHeight&asset=${issue.assetId}")) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(Json.obj("id" -> address.toString, "balance" -> 2 * transferCount))
    }
    Post(
      routePath(s"/balance"),
      Json.obj("height" -> balanceCheckHeight, "addresses" -> Seq(address.toString), "asset" -> issue.assetId)
    ) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(Json.obj("id" -> address.toString, "balance" -> 2 * transferCount))
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

  routePath(s"/scriptInfo/{address}") in {
    val script = TestCompiler(V5).compileExpression("""
      sigVerify(base58'', base58'', base58'') ||
      sigVerify(base58'', base58'', base58'')
    """)

    val exprScriptOwner = TxHelpers.signer(0xffaa88)

    domain.appendBlock(
      TxHelpers.transfer(richAccount, exprScriptOwner.toAddress),
      TxHelpers.setScript(exprScriptOwner, script)
    )

    Get(routePath(s"/scriptInfo/${exprScriptOwner.toAddress}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe exprScriptOwner.toAddress.toString
      (response \ "script").as[String] shouldBe "base64:BQMJAAH0AAAAAwEAAAAAAQAAAAABAAAAAAYJAAH0AAAAAwEAAAAAAQAAAAABAAAAAHBPrxY="
      (response \ "scriptText").as[String] shouldBe "IF(FUNCTION_CALL(Native(500),List(, , )),true,FUNCTION_CALL(Native(500),List(, , )))"
      (response \ "version").as[Int] shouldBe 5
      (response \ "complexity").as[Long] shouldBe 400
      (response \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
      (response \ "publicKey").as[String] shouldBe exprScriptOwner.publicKey.toString
    }

    val nonScriptedAddress = TxHelpers.address(0xffaa89)
    Get(routePath(s"/scriptInfo/$nonScriptedAddress")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe nonScriptedAddress.toString
      (response \ "script").asOpt[String] shouldBe None
      (response \ "scriptText").asOpt[String] shouldBe None
      (response \ "version").asOpt[Int] shouldBe None
      (response \ "complexity").as[Long] shouldBe 0
      (response \ "extraFee").as[Long] shouldBe 0
      (response \ "publicKey").asOpt[String] shouldBe None
    }

    val contractWithMeta = TestCompiler(V5).compileContract("""
      @Callable(i)
      func call1(a: Int, b: ByteVector, c: ByteVector|Int) = []

      @Callable(i)
      func call2(d: String) = []

      @Callable(i)
      func call3() = []

      @Verifier(tx)
      func check() = sigVerify(base58'', base58'', base58'') || sigVerify(base58'', base58'', base58'')
    """)

    val dappOwner = TxHelpers.signer(0xffaa90)

    domain.appendBlock(
      TxHelpers.transfer(richAccount, dappOwner.toAddress),
      TxHelpers.setScript(dappOwner, contractWithMeta)
    )

    Get(routePath(s"/scriptInfo/${dappOwner.toAddress}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe dappOwner.toAddress.toString
      (response \ "script").as[String] should fullyMatch regex "base64:.+".r
      (response \ "scriptText").as[String] should fullyMatch regex "DApp\\(.+\\)".r
      (response \ "version").as[Int] shouldBe 5
      (response \ "complexity").as[Long] shouldBe 400
      (response \ "verifierComplexity").as[Long] shouldBe 400
      (response \ "callableComplexities").as[Map[String, Long]] shouldBe Map("call1" -> 1, "call2" -> 1, "call3" -> 1)
      (response \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
      (response \ "publicKey").as[String] shouldBe dappOwner.publicKey.toString
    }

    Get(routePath(s"/scriptInfo/${dappOwner.toAddress}/meta")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe dappOwner.toAddress.toString
      (response \ "meta" \ "version").as[String] shouldBe "2"
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

    val dappWoMetaOwner     = TxHelpers.signer(0xffaa91)
    val contractWithoutMeta = contractWithMeta.copy(expr = contractWithMeta.expr.copy(meta = DAppMeta()))
    domain.appendBlock(
      TxHelpers.transfer(richAccount, dappWoMetaOwner.toAddress),
      TxHelpers.setScript(dappWoMetaOwner, contractWithoutMeta)
    )

    Get(routePath(s"/scriptInfo/${dappWoMetaOwner.toAddress}/meta")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe dappWoMetaOwner.toAddress.toString
      (response \ "meta" \ "version").as[String] shouldBe "0"
    }

    val dappWoVerifierOwner     = TxHelpers.signer(0xffaa92)
    val contractWithoutVerifier = contractWithMeta.copy(expr = contractWithMeta.expr.copy(verifierFuncOpt = None))
    domain.appendBlock(
      TxHelpers.transfer(richAccount, dappWoVerifierOwner.toAddress),
      TxHelpers.setScript(dappWoVerifierOwner, contractWithoutVerifier)
    )
    val contractWithoutVerifierComplexities = Map("call1" -> 1L, "call2" -> 1L, "call3" -> 1L)

    Get(routePath(s"/scriptInfo/${dappWoVerifierOwner.toAddress}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe dappWoVerifierOwner.toAddress.toString
      (response \ "version").as[Int] shouldBe 5
      (response \ "complexity").as[Long] shouldBe 1
      (response \ "verifierComplexity").as[Long] shouldBe 0
      (response \ "callableComplexities").as[Map[String, Long]] shouldBe contractWithoutVerifierComplexities
      (response \ "extraFee").as[Long] shouldBe 0
      (response \ "publicKey").as[String] shouldBe dappWoVerifierOwner.publicKey.toString
    }
  }

  routePath(s"/scriptInfo/ after ${BlockchainFeatures.SynchronousCalls}") in {
    val simpleScript = TestCompiler(V6).compileExpression("""
      sigVerify_32Kb(base58'', base58'', base58'')
    """)

    val noExtraFeeOwner = TxHelpers.signer(0xffaa95)

    domain.appendBlock(
      TxHelpers.transfer(richAccount, noExtraFeeOwner.toAddress),
      TxHelpers.setScript(noExtraFeeOwner, simpleScript)
    )

    Get(routePath(s"/scriptInfo/${noExtraFeeOwner.toAddress}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe noExtraFeeOwner.toAddress.toString
      (response \ "version").as[Int] shouldBe 6
      (response \ "complexity").as[Long] shouldBe 64
      (response \ "verifierComplexity").as[Long] shouldBe 64
      (response \ "extraFee").as[Long] shouldBe 0
      (response \ "publicKey").as[String] shouldBe noExtraFeeOwner.publicKey.toString
    }
  }

  routePath(s"/data/{address}?matches=regex") in {
    val invalidRegexps = List("[a-z", "([a-z]{0}", "[a-z]{0", "[a-z]{,5}")
    for (regex <- invalidRegexps) {
      Get(routePath(s"""/data/${richAccount.toAddress}?matches=$regex""")) ~> route ~> check {
        responseAs[String] should include("Cannot compile regex")
      }
    }
  }

  routePath(s"/data/{address} with Transfer-Encoding: chunked") in {
    val account = TxHelpers.signer(1)

    domain.appendBlock(TxHelpers.dataSingle(account))

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

  routePath(s"/data/{address} - handles keys limit") in {
    def checkErrorResponse(): Unit = {
      response.status shouldBe StatusCodes.BadRequest
      (responseAs[JsObject] \ "message").as[String] shouldBe TooBigArrayAllocation(restAPISettings.dataKeysRequestLimit).message
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

    domain.appendBlock(TxHelpers.dataSingle(account, key = key, value = value))

    val maxLimitKeys      = Seq.fill(restAPISettings.dataKeysRequestLimit)(key)
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

  "handles stack overflow" in {
    import monix.execution.Scheduler.Implicits.global
    val dataOwner        = TxHelpers.signer(0xaaff05)
    val dataTransactions = (1 to 500) map { d => TxHelpers.data(dataOwner, Seq.tabulate(100)(i => IntegerDataEntry(s"k_${d}_$i", i))) }

    domain.appendBlock(TxHelpers.transfer(richAccount, dataOwner.toAddress, 100.waves))
    domain.appendBlock(dataTransactions*)
    domain.appendBlock()

    domain.accountsApi
      .dataStream(dataOwner.toAddress, Some("nomatch"))
      .toListL
      .runSyncUnsafe(15.seconds) should be(empty)

  }
}
