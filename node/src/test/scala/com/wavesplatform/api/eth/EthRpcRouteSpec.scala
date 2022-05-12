package com.wavesplatform.api.eth

import com.wavesplatform.api.http.eth.EthRpcRoute
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{DefaultWavesSettings, Domain, settingsWithFeatures}
import com.wavesplatform.http.RouteSpec
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.BinaryDataEntry
import com.wavesplatform.test.*
import com.wavesplatform.test.node.{randomAddress, randomKeyPair}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.utils.{EthTxGenerator, Signed}
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxHelpers}
import com.wavesplatform.utils.{EthEncoding, EthHelpers}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsArray, JsObject, Json}

class EthRpcRouteSpec extends RouteSpec("/eth") with WithDomain with EthHelpers {

  def routeTest[T](d: Domain, method: String, params: JsValueWrapper*)(body: => T): Unit = {
    Post(
      routePath(""),
      Json.obj("method" -> method, "params" -> Json.arr(params*), "id" -> "test")
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

    "has contract" in withDomain(settingsWithFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls, BlockchainFeatures.Ride4DApps)) {
      d =>
        val testKP = randomKeyPair()
        d.appendBlock(
          GenesisTransaction.create(testKP.toAddress, 1.waves, ntpTime.getTimestamp()).explicitGet(),
          Signed.setScript(
            2.toByte,
            testKP,
            Some(TestCompiler(V5).compileContract("""{-# STDLIB_VERSION 4 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |@Callable(inv)
            |func foo() = {
            |  [
            |    BinaryEntry("leaseId", base64'AAA')
            |  ]
            |}
            |""".stripMargin)),
            0.01.waves,
            ntpTime.getTimestamp()
          )
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
      |  "transactionIndex" : "0x1",
      |  "blockHash" : "${d.blockchain.lastBlockId.get.toHexString}",
      |  "blockNumber" : "0x2",
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
      GenesisTransaction.create(transaction.senderAddress(), 50.waves, ntpTime.getTimestamp()).explicitGet()
    )
    routeTest(d, "eth_sendRawTransaction", EthEncoding.toHexString(transaction.bytes()))(
      result shouldBe transaction.id().toHexString
    )
  }

  "eth/assets" in withDomain(settingsWithFeatures(BlockchainFeatures.Ride4DApps, BlockchainFeatures.ReduceNFTFee, BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)) { d =>
    val randomKP = randomKeyPair()
    val invoker  = randomKeyPair()
    val issue1   = Signed.issue(2.toByte, randomKP, "TEST1", "test asset", 100000, 2, true, None, 1.waves, ntpTime.getTimestamp())
    val issue2   = Signed.issue(2.toByte, randomKP, "NFT1", "test asset", 1, 0, false, None, 0.001.waves, ntpTime.getTimestamp())

    d.appendBlock(
      GenesisTransaction.create(randomKP.toAddress, 5.waves, ntpTime.getTimestamp()).explicitGet(),
      GenesisTransaction.create(invoker.toAddress, 5.waves, ntpTime.getTimestamp()).explicitGet()
    )

    val invoke = Signed.invokeScript(
      2.toByte,
      invoker,
      randomKP.toAddress,
      None,
      Seq(InvokeScriptTransaction.Payment(1.waves, Asset.Waves)),
      1.005.waves,
      Asset.Waves,
      ntpTime.getTimestamp()
    )
    d.appendBlock(
      issue1,
      issue2,
      Signed.setScript(2.toByte, randomKP, Some(TestCompiler(V5).compileContract(
        """{-# STDLIB_VERSION 4 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |
          |@Callable(inv)
          |func default() = {
          |  let issue = Issue("INVASSET", "", 10000, 2, false)
          |  [
          |    issue,
          |    BinaryEntry("assetId", calculateAssetId(issue))
          |  ]
          |}
          |""".stripMargin)), 0.01.waves, ntpTime.getTimestamp()),
      invoke
    )

    val issue3 = d.blockchain.accountData(randomKP.toAddress, "assetId").get.asInstanceOf[BinaryDataEntry].value

    new EthRpcRoute(d.blockchain, d.commonApi.transactions, ntpTime).route
      .anyParamTest(routePath("/assets"), "id")(
        EthEncoding.toHexString(issue1.id().arr.take(20)),
        EthEncoding.toHexString(issue2.id().arr.take(20)),
        EthEncoding.toHexString(issue3.arr.take(20))
      ) {
        responseAs[JsArray] should matchJson(s"""[ {
                                             |  "assetId" : "${issue1.id()}",
                                             |  "issueHeight" : 2,
                                             |  "issueTimestamp" : ${issue1.timestamp},
                                             |  "issuer" : "${randomKP.toAddress}",
                                             |  "issuerPublicKey" : "${randomKP.publicKey.toString}",
                                             |  "name" : "TEST1",
                                             |  "description" : "test asset",
                                             |  "decimals" : 2,
                                             |  "reissuable" : true,
                                             |  "quantity" : 100000,
                                             |  "scripted" : false,
                                             |  "minSponsoredAssetFee" : null,
                                             |  "originTransactionId" : "${issue1.id()}"
                                             |}, {
                                             |  "assetId" : "${issue2.id()}",
                                             |  "issueHeight" : 2,
                                             |  "issueTimestamp" : ${issue2.timestamp},
                                             |  "issuer" : "${randomKP.toAddress}",
                                             |  "issuerPublicKey" : "${randomKP.publicKey.toString}",
                                             |  "name" : "NFT1",
                                             |  "description" : "test asset",
                                             |  "decimals" : 0,
                                             |  "reissuable" : false,
                                             |  "quantity" : 1,
                                             |  "scripted" : false,
                                             |  "minSponsoredAssetFee" : null,
                                             |  "originTransactionId" : "${issue2.id()}"
                                             |}, {
                                             |  "assetId" : "$issue3",
                                             |  "issueHeight" : 2,
                                             |  "issueTimestamp" : ${invoke.timestamp},
                                             |  "issuer" : "${randomKP.toAddress}",
                                             |  "issuerPublicKey" : "${randomKP.publicKey.toString}",
                                             |  "name" : "INVASSET",
                                             |  "description" : "",
                                             |  "decimals" : 2,
                                             |  "reissuable" : false,
                                             |  "quantity" : 10000,
                                             |  "scripted" : false,
                                             |  "minSponsoredAssetFee" : null,
                                             |  "originTransactionId" : "${invoke.id()}"
                                             |} ]""".stripMargin)
      }
  }

  def resultJson: JsObject = (responseAs[JsObject] \ "result").as[JsObject]
  def result: String       = (responseAs[JsObject] \ "result").as[String]
  def resultInt: Long      = java.lang.Long.valueOf((responseAs[JsObject] \ "result").as[String].drop(2), 16)
}
