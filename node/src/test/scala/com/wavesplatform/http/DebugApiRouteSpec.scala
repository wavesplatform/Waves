package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.util._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.traits.domain.Issue
import com.wavesplatform.network.PeerDatabase
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, Blockchain, Height, LeaseBalance, NG, StateHash, VolumeAndFee}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.{NTPTime, TestValues, TestWallet}
import monix.eval.Task
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import scala.util.Random

//noinspection ScalaStyle
class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet with NTPTime with PathMockFactory {

  val wavesSettings = WavesSettings.default()
  val configObject  = wavesSettings.config.root()
  trait Blockchain1 extends Blockchain with NG
  val blockchain = stub[Blockchain1]
  val block      = TestBlock.create(Nil)
  val testStateHash = {
    def randomHash: ByteStr = ByteStr(Array.fill(32)(Random.nextInt(256).toByte))
    val hashes              = SectionId.values.map((_, randomHash)).toMap
    StateHash(randomHash, hashes)
  }

  val debugApiRoute =
    DebugApiRoute(
      wavesSettings,
      ntpTime,
      blockchain,
      null,
      null,
      null,
      null,
      PeerDatabase.NoOp,
      null,
      (_, _) => Task.raiseError(new NotImplementedError("")),
      null,
      null,
      null,
      null,
      null,
      null,
      configObject,
      _ => Seq.empty, {
        case 2 => Some(testStateHash)
        case _ => None
      }
    )
  import debugApiRoute._

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }

  routePath("/stateHash") - {
    "works" in {
      (blockchain.blockHeader(_: Int)).when(*).returning(Some(SignedBlockHeader(block.header, block.signature)))
      Get(routePath("/stateHash/2")) ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[JsObject] shouldBe (Json.toJson(testStateHash).as[JsObject] ++ Json.obj("blockId" -> block.id().toString))
      }

      Get(routePath("/stateHash/3")) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }
  }

  routePath("/validate") - {
    def createBlockchainStub(f: Blockchain => Unit = _ => ()): Blockchain with NG = {
      trait Blockchain1 extends Blockchain with NG
      val blockchain = stub[Blockchain1]
      f(blockchain) // Overrides
      (() => blockchain.settings).when().returns(WavesSettings.default().blockchainSettings)
      (() => blockchain.activatedFeatures)
        .when()
        .returns(
          Map(
            BlockchainFeatures.BlockV5.id             -> 0,
            BlockchainFeatures.SmartAccounts.id       -> 0,
            BlockchainFeatures.SmartAssets.id         -> 0,
            BlockchainFeatures.SmartAccountTrading.id -> 0,
            BlockchainFeatures.OrderV3.id             -> 0,
            BlockchainFeatures.Ride4DApps.id          -> 0
          )
        )
      (blockchain.accountScript _).when(*).returns(None)
      (blockchain.leaseBalance _).when(*).returns(LeaseBalance.empty)
      (() => blockchain.height).when().returns(1)
      (blockchain.blockHeader _).when(*).returns {
        val block = TestBlock.create(Nil)
        Some(SignedBlockHeader(block.header, block.signature))
      }
      (blockchain.filledVolumeAndFee _).when(*).returns(VolumeAndFee.empty)
      (blockchain.assetDescription _).when(*).returns(None)
      blockchain
    }

    "valid tx" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(Long.MaxValue)

      val route = debugApiRoute.copy(blockchain = blockchain).route

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, 1.waves)
      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, tx.json().toString())) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
      }
    }

    "invalid tx" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(0)

      val route = debugApiRoute.copy(blockchain = blockchain).route

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, 1.waves)
      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, tx.json().toString())) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("Attempt to transfer unavailable funds")
      }
    }

    "exchange tx with fail script" in {
      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(TxHelpers.defaultAddress, *).returns(Long.MaxValue)

        val (assetScript, comp) = ScriptCompiler.compile("if true then throw(\"error\") else false", ScriptEstimatorV3).explicitGet()
        (blockchain.assetScript _).when(TestValues.asset).returns(Some(AssetScriptInfo(assetScript, comp)))
        (blockchain.assetDescription _)
          .when(TestValues.asset)
          .returns(
            Some(
              AssetDescription(
                null,
                null,
                null,
                null,
                0,
                reissuable = false,
                null,
                Height(1),
                Some(AssetScriptInfo(assetScript, comp)),
                0,
                nft = false
              )
            )
          )
      }

      val route = debugApiRoute.copy(blockchain = blockchain).route
      val tx    = TxHelpers.exchange(TxHelpers.order(OrderType.BUY, TestValues.asset), TxHelpers.order(OrderType.SELL, TestValues.asset))
      jsonPost(routePath("/validate"), tx.json()) ~> ApiKeyHeader ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("not allowed by script of the asset")
        (json \ "trace").as[JsArray] shouldBe Json.parse(
          "[{\"type\":\"asset\",\"assetType\":\"orderAmount\",\"id\":\"5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx\",\"error\":{\"type\":\"asset\",\"vars\":[],\"reason\":\"error\"}}]"
        )
      }
    }

    "invoke tx with asset failing" in {
      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(*, *).returns(Long.MaxValue / 2)

        val (assetScript, assetScriptComplexity) = ScriptCompiler.compile("if true then throw(\"error\") else false", ScriptEstimatorV3).explicitGet()
        (blockchain.assetScript _).when(TestValues.asset).returns(Some(AssetScriptInfo(assetScript, assetScriptComplexity)))
        (blockchain.assetDescription _)
          .when(TestValues.asset)
          .returns(
            Some(
              AssetDescription(
                TestValues.asset.id,
                TxHelpers.defaultSigner.publicKey,
                null,
                null,
                0,
                reissuable = true,
                BigInt(1),
                Height(1),
                Some(AssetScriptInfo(assetScript, assetScriptComplexity)),
                0,
                nft = false
              )
            )
          )

        val (dAppScript, _) = ScriptCompiler
          .compile(
            s"""
               |{-# STDLIB_VERSION 4 #-}
               |{-# SCRIPT_TYPE ACCOUNT #-}
               |{-# CONTENT_TYPE DAPP #-}
               |
               |@Callable(i)
               |func test() = []
               |
               |@Callable(i)
               |func dataAndTransfer() = [
               |     IntegerEntry("key", 1),
               |     BooleanEntry("key", true),
               |     StringEntry("key", "str"),
               |     BinaryEntry("key", base58''),
               |     DeleteEntry("key"),
               |     ScriptTransfer(Address(base58'${TxHelpers.signer(1).toAddress}'), 1, base58'${TestValues.asset}')
               |]
               |
               |@Callable(i)
               |func issue() = [Issue("name", "description", 1000, 4, true, unit, 0)]
               |
               |@Callable(i)
               |func reissue() = [Reissue(base58'${TestValues.asset}', 1, false)]
               |
               |@Callable(i)
               |func burn() = [Burn(base58'${TestValues.asset}', 1)]
               |""".stripMargin,
            ScriptEstimatorV3
          )
          .explicitGet()
        (blockchain.accountScript _)
          .when(*)
          .returns(
            Some(
              AccountScriptInfo(
                TxHelpers.defaultSigner.publicKey,
                dAppScript,
                0L,
                Map(3 -> Seq("test", "dataAndTransfer", "issue", "reissue", "burn", "sponsorFee").map(_ -> 0L).toMap)
              )
            )
          )
        (blockchain.hasAccountScript _).when(TxHelpers.defaultAddress).returns(true)
      }

      val route = debugApiRoute.copy(blockchain = blockchain).route

      def testFunction(name: String, result: InvokeScriptTransaction => String = _ => "") = {
        val tx = TxHelpers.invoke(TxHelpers.defaultAddress, name, fee = 102500000)

        jsonPost(routePath("/validate"), tx.json()) ~> ApiKeyHeader ~> route ~> check {
          val json  = Json.parse(responseAs[String])
          val trace = Json.prettyPrint((json \ "trace").as[JsArray])
          if (trace != result(tx)) println(trace)
          trace shouldBe result(tx)
        }
      }

      def testPayment(result: String) = {
        val tx = TxHelpers.invoke(TxHelpers.signer(1).toAddress, "test", fee = 800000, payments = Seq(Payment(1L, TestValues.asset)))

        jsonPost(routePath("/validate"), tx.json()) ~> ApiKeyHeader ~> route ~> check {
          val json  = Json.parse(responseAs[String])
          val trace = Json.prettyPrint((json \ "trace").as[JsArray])
          if (trace != result) println(trace)
          trace shouldBe result
        }
      }

      testPayment("""[ {
                    |  "type" : "verifier",
                    |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                    |  "result" : "ok"
                    |}, {
                    |  "type" : "dApp",
                    |  "id" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                    |  "function" : "test",
                    |  "args" : [ ],
                    |  "result" : {
                    |    "data" : [ ],
                    |    "transfers" : [ ],
                    |    "issues" : [ ],
                    |    "reissues" : [ ],
                    |    "burns" : [ ],
                    |    "sponsorFees" : [ ],
                    |    "vars" : [ ]
                    |  }
                    |}, {
                    |  "type" : "asset",
                    |  "assetType" : "payment",
                    |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
                    |  "error" : {
                    |    "type" : "asset",
                    |    "vars" : [ ],
                    |    "reason" : "error"
                    |  }
                    |} ]""".stripMargin)

      testFunction(
        "dataAndTransfer",
        _ => """[ {
          |  "type" : "verifier",
          |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
          |  "result" : "ok"
          |}, {
          |  "type" : "dApp",
          |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
          |  "function" : "dataAndTransfer",
          |  "args" : [ ],
          |  "result" : {
          |    "data" : [ {
          |      "key" : "key",
          |      "type" : "integer",
          |      "value" : 1
          |    }, {
          |      "key" : "key",
          |      "type" : "boolean",
          |      "value" : true
          |    }, {
          |      "key" : "key",
          |      "type" : "string",
          |      "value" : "str"
          |    }, {
          |      "key" : "key",
          |      "type" : "binary",
          |      "value" : "base64:"
          |    }, {
          |      "key" : "key",
          |      "value" : null
          |    } ],
          |    "transfers" : [ {
          |      "address" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
          |      "asset" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
          |      "amount" : 1
          |    } ],
          |    "issues" : [ ],
          |    "reissues" : [ ],
          |    "burns" : [ ],
          |    "sponsorFees" : [ ],
          |    "vars" : [ ]
          |  }
          |}, {
          |  "type" : "asset",
          |  "assetType" : "transfer",
          |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
          |  "error" : "FailedTransactionError(code = 3, error = Transaction is not allowed by script of the asset 5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx: error, log =)"
          |} ]""".stripMargin
      )

      testFunction(
        "issue",
        tx => s"""[ {
          |  "type" : "verifier",
          |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
          |  "result" : "ok"
          |}, {
          |  "type" : "dApp",
          |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
          |  "function" : "issue",
          |  "args" : [ ],
          |  "result" : {
          |    "data" : [ ],
          |    "transfers" : [ ],
          |    "issues" : [ {
          |      "assetId" : "${Issue.calculateId(4, "description", true, "name", 1000, 0, tx.id())}",
          |      "name" : "name",
          |      "description" : "description",
          |      "quantity" : 1000,
          |      "decimals" : 4,
          |      "isReissuable" : true,
          |      "compiledScript" : null,
          |      "nonce" : 0
          |    } ],
          |    "reissues" : [ ],
          |    "burns" : [ ],
          |    "sponsorFees" : [ ],
          |    "vars" : [ ]
          |  }
          |} ]""".stripMargin
      )

      testFunction(
        "reissue",
        _ => """[ {
               |  "type" : "verifier",
               |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
               |  "result" : "ok"
               |}, {
               |  "type" : "dApp",
               |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
               |  "function" : "reissue",
               |  "args" : [ ],
               |  "result" : {
               |    "data" : [ ],
               |    "transfers" : [ ],
               |    "issues" : [ ],
               |    "reissues" : [ {
               |      "assetId" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
               |      "isReissuable" : false,
               |      "quantity" : 1
               |    } ],
               |    "burns" : [ ],
               |    "sponsorFees" : [ ],
               |    "vars" : [ ]
               |  }
               |}, {
               |  "type" : "asset",
               |  "assetType" : "reissue",
               |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
               |  "error" : "FailedTransactionError(code = 3, error = Transaction is not allowed by script of the asset 5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx: error, log =)"
               |} ]""".stripMargin
      )

      testFunction(
        "burn",
        _ => """[ {
          |  "type" : "verifier",
          |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
          |  "result" : "ok"
          |}, {
          |  "type" : "dApp",
          |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
          |  "function" : "burn",
          |  "args" : [ ],
          |  "result" : {
          |    "data" : [ ],
          |    "transfers" : [ ],
          |    "issues" : [ ],
          |    "reissues" : [ ],
          |    "burns" : [ {
          |      "assetId" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
          |      "quantity" : 1
          |    } ],
          |    "sponsorFees" : [ ],
          |    "vars" : [ ]
          |  }
          |}, {
          |  "type" : "asset",
          |  "assetType" : "burn",
          |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
          |  "error" : "FailedTransactionError(code = 3, error = Transaction is not allowed by script of the asset 5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx: error, log =)"
          |} ]""".stripMargin
      )

    }
  }

  private[this] def jsonPost(path: String, json: JsValue) = {
    Post(path, HttpEntity(ContentTypes.`application/json`, json.toString()))
  }
}
