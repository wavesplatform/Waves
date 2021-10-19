package com.wavesplatform.api.eth

import com.wavesplatform.BlockchainStubHelpers
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonTransactionsApi, TransactionMeta}
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.eth.EthRpcRoute
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.http.RouteSpec
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.utils.EthConverters._
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.utils.{EthEncoding, EthHelpers, EthSetChainId}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.concurrent.Future

class EthRpcRouteSpec
    extends RouteSpec("/eth")
    with Matchers
    with PathMockFactory
    with BlockchainStubHelpers
    with EthHelpers
    with EthSetChainId
    with BeforeAndAfterEach {
  var blockchain      = stub[Blockchain]
  var transactionsApi = stub[CommonTransactionsApi]
  var route           = new EthRpcRoute(blockchain, transactionsApi, ntpTime)

  "eth_chainId" in testRpc("eth_chainId")(resultInt shouldBe 'E'.toLong)

  "eth_gasPrice" in testRpc("eth_gasPrice")(resultInt shouldBe 10000000000L)

  "eth_blockNumber" in {
    (() => blockchain.height).when().returning(123)
    testRpc("eth_blockNumber")(resultInt shouldBe 123)
  }

  "eth_getBalance" in {
    (blockchain.balance _).when(*, *).returning(123)
    testRpc("eth_getBalance", EthEncoding.toHexString(EthStubBytes32.take(20)))(resultInt shouldBe 1230000000000L)
  }

  "eth_getCode" - {
    "no contract" in {
      testRpc("eth_getCode", EthEncoding.toHexString(EthStubBytes32.take(20)))(result shouldBe "0x")
    }

    "has contract" in {
      val testAddress = Address(EthStubBytes32.take(20))
      blockchain.stub.setScript(testAddress, TxHelpers.scriptV5(""))
      testRpc("eth_getCode", EthEncoding.toHexString(testAddress.publicKeyHash))(result shouldBe "0xff")
    }
  }

  "eth_estimateGas" in {
    (transactionsApi.calculateFee _).when(*).returns(Right((Waves, 500000L, 500000L)))
    testRpc("eth_estimateGas", Json.obj("to" -> TxHelpers.secondAddress.toEthAddress, "value" -> 0, "data" -> "0x00")) {
      resultInt shouldBe 500000
    }
  }

  "eth_call" - {
    "asset calls" in {
      val assetId       = ByteStr(EthStubBytes32)
      val fakeAddress   = Address(assetId.take(20).arr)
      val assetContract = EthEncoding.toHexString(fakeAddress.publicKeyHash)
      blockchain.stub.issueAsset(assetId)
      blockchain.stub.creditBalance(fakeAddress, IssuedAsset(assetId), 255)

      withClue("asset name")(
        testRpc("eth_call", Json.obj("to" -> assetContract, "data" -> "0x95d89b41"))(
          result shouldBe "000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000047465737400000000000000000000000000000000000000000000000000000000"
        )
      )

      withClue("asset decimals")(
        testRpc("eth_call", Json.obj("to" -> assetContract, "data" -> "0x313ce567"))(
          result shouldBe "0000000000000000000000000000000000000000000000000000000000000008"
        )
      )

      withClue("asset balance")(
        testRpc("eth_call", Json.obj("to" -> assetContract, "data" -> ("70a08231" + assetContract.drop(2))))(
          result shouldBe "00000000000000000000000000000000000000000000000000000000000000ff"
        )
      )
    }
  }

  "eth_getTransactionReceipt" in {
    val block       = TestBlock.create(Nil)
    val transaction = EthTxGenerator.generateEthInvoke(TxHelpers.defaultSigner.toEthKeyPair, TxHelpers.secondAddress, "test", Nil, Nil)
    (() => blockchain.height).when().returns(1)
    (blockchain.blockHeader _).when(1).returns {
      Some(SignedBlockHeader(block.header, block.signature))
    }
    (transactionsApi.transactionById _)
      .when(transaction.id())
      .returns(Some(TransactionMeta.Ethereum(Height(1), transaction, succeeded = true, None, None)))

    testRpc("eth_getTransactionReceipt", transaction.id().toHexString)(resultJson should matchJson(s"""{
                                                                                                     |  "transactionHash" : "${transaction
                                                                                                        .id()
                                                                                                        .toHexString}",
                                                                                                     |  "transactionIndex" : "0x01",
                                                                                                     |  "blockHash" : "${block.id().toHexString}",
                                                                                                     |  "blockNumber" : "0x01",
                                                                                                     |  "from" : "0xf1f6bdabc1b48e7d75957b361881be9c40e4b424",
                                                                                                     |  "to" : "0x3d3ad884fa042927b9d6c37df70af5c0bd9516c5",
                                                                                                     |  "cumulativeGasUsed" : "0x7a120",
                                                                                                     |  "gasUsed" : "0x7a120",
                                                                                                     |  "contractAddress" : null,
                                                                                                     |  "logs" : [ ],
                                                                                                     |  "logsBloom" : "0x0000000000000000000000000000000000000000000000000000000000000000",
                                                                                                     |  "status" : "0x1"
                                                                                                     |}""".stripMargin))
  }

  "eth_sendRawTransaction" in {
    val transaction = EthTxGenerator.generateEthInvoke(TxHelpers.defaultSigner.toEthKeyPair, TxHelpers.secondAddress, "test", Nil, Nil)
    (transactionsApi.broadcastTransaction _).when(*).returns(Future.successful(TracedResult(Right(true))))
    testRpc("eth_sendRawTransaction", EthEncoding.toHexString(transaction.bytes()))(
      result shouldBe transaction.id().toHexString
    )
  }

  "eth/assets" in {
    val testAsset1 = ByteStr(Array.fill(32)(1.toByte))
    val testAsset2 = ByteStr(Array.fill(32)(2.toByte))
    blockchain.stub.issueAsset(testAsset1)
    blockchain.stub.issueAsset(testAsset2)

    route.route
      .anyParamTest(routePath("/assets"), "id")(EthEncoding.toHexString(testAsset1.take(20).arr), EthEncoding.toHexString(testAsset2.take(20).arr)) {
        responseAs[JsArray] should matchJson("""[ {
                                             |  "assetId" : "4vJ9JU1bJJE96FWSJKvHsmmFADCg4gpZQff4P3bkLKi",
                                             |  "issueHeight" : 1,
                                             |  "issueTimestamp" : 123,
                                             |  "issuer" : "3FrCwv8uFRxQazhX6Lno45aZ68Bof6ScaeF",
                                             |  "issuerPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
                                             |  "name" : "test",
                                             |  "description" : "test",
                                             |  "decimals" : 8,
                                             |  "reissuable" : false,
                                             |  "quantity" : 10000,
                                             |  "scripted" : false,
                                             |  "minSponsoredAssetFee" : null,
                                             |  "originTransactionId" : "4vJ9JU1bJJE96FWSJKvHsmmFADCg4gpZQff4P3bkLKi"
                                             |}, {
                                             |  "assetId" : "8qbHbw2BbbTHBW1sbeqakYXVKRQM8Ne7pLK7m6CVfeR",
                                             |  "issueHeight" : 1,
                                             |  "issueTimestamp" : 123,
                                             |  "issuer" : "3FrCwv8uFRxQazhX6Lno45aZ68Bof6ScaeF",
                                             |  "issuerPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
                                             |  "name" : "test",
                                             |  "description" : "test",
                                             |  "decimals" : 8,
                                             |  "reissuable" : false,
                                             |  "quantity" : 10000,
                                             |  "scripted" : false,
                                             |  "minSponsoredAssetFee" : null,
                                             |  "originTransactionId" : "8qbHbw2BbbTHBW1sbeqakYXVKRQM8Ne7pLK7m6CVfeR"
                                             |} ]""".stripMargin)
      }
  }

  // Helpers
  def testRpc(method: String, params: JsValueWrapper*)(doCheck: => Unit): Unit = {
    val entity = Json.obj("method" -> method, "params" -> Json.arr(params: _*), "id" -> "test")
    Post(routePath("/"), entity) ~> route.route ~> check(doCheck)
  }

  def resultJson: JsObject = (responseAs[JsObject] \ "result").as[JsObject]
  def result: String       = (responseAs[JsObject] \ "result").as[String]
  def resultInt: Long      = java.lang.Long.valueOf((responseAs[JsObject] \ "result").as[String].drop(2), 16)

  override protected def beforeEach(): Unit = { // Just resets stubs
    super.beforeEach()
    blockchain = stub[Blockchain]
    transactionsApi = stub[CommonTransactionsApi]
    route = new EthRpcRoute(blockchain, transactionsApi, ntpTime)
  }
}
