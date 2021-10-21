package com.wavesplatform.api.eth

import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.eth.EthRpcRoute
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{DefaultWavesSettings, Domain, settingsWithFeatures}
import com.wavesplatform.http.RouteSpec
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test._
import com.wavesplatform.test.node.{randomAddress, randomKeyPair}
import com.wavesplatform.transaction.utils.EthConverters._
import com.wavesplatform.transaction.utils.{EthTxGenerator, Signed}
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxHelpers}
import com.wavesplatform.utils.{EthEncoding, EthHelpers}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsArray, JsObject, Json}

class EthRpcRouteSpec extends RouteSpec("/eth") with WithDomain with EthHelpers {

  def routeTest[T](d: Domain, method: String, params: JsValueWrapper*)(body: => T): Unit = {
    Post(
      routePath(""),
      Json.obj("method" -> method, "params" -> Json.arr(params: _*), "id" -> "test")
    ) ~> new EthRpcRoute(d.blockchain, d.commonApi.transactions, ntpTime).route ~> check(body)
  }

  "eth_chainId" in withDomain(DefaultWavesSettings) { d =>
    routeTest(d, "eth_chainId")(resultInt shouldBe DefaultWavesSettings.blockchainSettings.addressSchemeCharacter.toLong)
  }

  "eth_gasPrice" in withDomain() { d =>
    routeTest(d, "eth_gasPrice")(resultInt shouldBe 10000000000L)
  }

  "eth_blockNumber" in withDomain() { d =>
    1 to 11 foreach (_ => d.appendBlock())
    routeTest(d, "eth_blockNumber")(resultInt shouldBe 11)
  }

  "eth_getBalance" in withDomain() { d =>
    val address = randomAddress()
    d.appendBlock(GenesisTransaction.create(address, 123L, ntpTime.getTimestamp()).explicitGet())
    routeTest(d, "eth_getBalance", address.toEthAddress)(resultInt shouldBe 1230000000000L)
  }

  "eth_getCode" - {
    "no contract" in withDomain() { d =>
      val address = randomAddress()
      routeTest(d, "eth_getCode", address.toEthAddress)(result shouldBe "0x")
    }

    "has contract" in withDomain(settingsWithFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls, BlockchainFeatures.Ride4DApps)) { d =>
      val testKP = randomKeyPair()
      d.appendBlock(
        GenesisTransaction.create(testKP.toAddress, 1.waves, ntpTime.getTimestamp()).explicitGet(),
        Signed.setScript(2.toByte, testKP, Some(TestCompiler(V5).compileContract(
          """{-# STDLIB_VERSION 4 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |@Callable(inv)
            |func foo() = {
            |  [
            |    BinaryEntry("leaseId", base64'AAA')
            |  ]
            |}
            |""".stripMargin)), 0.01.waves, ntpTime.getTimestamp())
      )
      routeTest(d, "eth_getCode", testKP.toAddress.toEthAddress)(result shouldBe "0xff")
    }
  }

  "eth_estimateGas" in withDomain() { d =>
    routeTest(d, "eth_estimateGas", Json.obj("to" -> TxHelpers.secondAddress.toEthAddress, "value" -> 0, "data" -> "0x00")) {
      resultInt shouldBe 500000
    }
  }

  "eth_call" - {
    "asset calls" in withDomain() { d =>
      val randomKP         = randomKeyPair()
      val issueTransaction = Signed.issue(2.toByte, randomKP, "TEST", "test asset", 100000, 2, false, None, 1.waves, ntpTime.getTimestamp())

      d.appendBlock(
        GenesisTransaction.create(randomKP.toAddress, 5.waves, ntpTime.getTimestamp()).explicitGet(),
        issueTransaction
      )

      val assetContract = EthEncoding.toHexString(issueTransaction.id().arr.take(20))

      withClue("asset name")(
        routeTest(d, "eth_call", Json.obj("to" -> assetContract, "data" -> "0x95d89b41"))(
          result shouldBe "000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000045445535400000000000000000000000000000000000000000000000000000000"
        )
      )

      withClue("asset decimals")(
        routeTest(d, "eth_call", Json.obj("to" -> assetContract, "data" -> "0x313ce567"))(
          result shouldBe "0000000000000000000000000000000000000000000000000000000000000002"
        )
      )

      withClue("asset balance")(
        routeTest(d, "eth_call", Json.obj("to" -> assetContract, "data" -> ("0x70a08231" + randomKP.toAddress.toEthAddress)))(
          result shouldBe "00000000000000000000000000000000000000000000000000000000000186a0"
        )
      )
    }
  }

  "eth_getTransactionReceipt" in withDomain(settingsWithFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6)) { d =>
    val transaction = EthTxGenerator.generateEthTransfer(TxHelpers.defaultSigner.toEthKeyPair, TxHelpers.secondAddress, 10L, Asset.Waves)

    d.appendBlock(GenesisTransaction.create(transaction.senderAddress(), 50.waves, ntpTime.getTimestamp()).explicitGet())
    d.appendBlock(transaction)

    routeTest(d, "eth_getTransactionReceipt", transaction.id().toHexString)(
      resultJson should matchJson(s"""{
      |  "transactionHash" : "${transaction.id().toHexString}",
      |  "transactionIndex" : "0x01",
      |  "blockHash" : "${d.blockchain.lastBlockId.get.toHexString}",
      |  "blockNumber" : "0x02",
      |  "from" : "0xf1f6bdabc1b48e7d75957b361881be9c40e4b424",
      |  "to" : "0x3d3ad884fa042927b9d6c37df70af5c0bd9516c5",
      |  "cumulativeGasUsed" : "0x186a0",
      |  "gasUsed" : "0x186a0",
      |  "contractAddress" : null,
      |  "logs" : [ ],
      |  "logsBloom" : "0x0000000000000000000000000000000000000000000000000000000000000000",
      |  "status" : "0x1"
      |}""".stripMargin)
    )
  }

  "eth_sendRawTransaction" in withDomain(settingsWithFeatures(BlockchainFeatures.RideV6, BlockchainFeatures.BlockV5)) { d =>
    val transaction = EthTxGenerator.generateEthTransfer(TxHelpers.defaultSigner.toEthKeyPair, TxHelpers.secondAddress, 10, Asset.Waves)
    d.appendBlock(
      GenesisTransaction.create(transaction.senderAddress(), 50.waves, ntpTime.getTimestamp()).explicitGet(),
    )
    routeTest(d, "eth_sendRawTransaction", EthEncoding.toHexString(transaction.bytes()))(
      result shouldBe transaction.id().toHexString
    )
  }

  "eth/assets" in withDomain() { d =>
    val randomKP = randomKeyPair()
    val issue1   = Signed.issue(2.toByte, randomKP, "TEST1", "test asset", 100000, 2, false, None, 1.waves, ntpTime.getTimestamp())
    val issue2   = Signed.issue(2.toByte, randomKP, "TEST2", "test asset", 200000, 3, false, None, 1.waves, ntpTime.getTimestamp())

    d.appendBlock(
      GenesisTransaction.create(randomKP.toAddress, 5.waves, ntpTime.getTimestamp()).explicitGet(),
      issue1,
      issue2
    )

    new EthRpcRoute(d.blockchain, d.commonApi.transactions, ntpTime).route
      .anyParamTest(routePath("/assets"), "id")(
        EthEncoding.toHexString(issue1.id().arr.take(20)),
        EthEncoding.toHexString(issue2.id().arr.take(20))
      ) {
        responseAs[JsArray] should matchJson(s"""[ {
                                             |  "assetId" : "${issue1.id()}",
                                             |  "issueHeight" : 1,
                                             |  "issueTimestamp" : ${issue1.timestamp},
                                             |  "issuer" : "${randomKP.toAddress}",
                                             |  "issuerPublicKey" : "${randomKP.publicKey.toString}",
                                             |  "name" : "TEST1",
                                             |  "description" : "test asset",
                                             |  "decimals" : 2,
                                             |  "reissuable" : false,
                                             |  "quantity" : 100000,
                                             |  "scripted" : false,
                                             |  "minSponsoredAssetFee" : null,
                                             |  "originTransactionId" : "${issue1.id()}"
                                             |}, {
                                             |  "assetId" : "${issue2.id()}",
                                             |  "issueHeight" : 1,
                                             |  "issueTimestamp" : ${issue2.timestamp},
                                             |  "issuer" : "${randomKP.toAddress}",
                                             |  "issuerPublicKey" : "${randomKP.publicKey.toString}",
                                             |  "name" : "TEST2",
                                             |  "description" : "test asset",
                                             |  "decimals" : 3,
                                             |  "reissuable" : false,
                                             |  "quantity" : 200000,
                                             |  "scripted" : false,
                                             |  "minSponsoredAssetFee" : null,
                                             |  "originTransactionId" : "${issue2.id()}"
                                             |} ]""".stripMargin)
      }
  }

  def resultJson: JsObject = (responseAs[JsObject] \ "result").as[JsObject]
  def result: String       = (responseAs[JsObject] \ "result").as[String]
  def resultInt: Long      = java.lang.Long.valueOf((responseAs[JsObject] \ "result").as[String].drop(2), 16)
}
