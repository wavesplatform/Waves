package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.common.CommonTransactionsApi.TransactionMeta
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.api.http.DebugApiRoute
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.util._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease, LeaseCancel, Recipient}
import com.wavesplatform.network.PeerDatabase
import com.wavesplatform.settings.{TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, Blockchain, Height, InvokeScriptResult, NG, StateHash}
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.{BlockchainStubHelpers, NTPTime, TestValues, TestWallet, TransactionGen}
import monix.eval.Task
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import scala.util.Random

//noinspection ScalaStyle
class DebugApiRouteSpec
    extends RouteSpec("/debug")
    with RestAPISettingsHelper
    with TestWallet
    with NTPTime
    with PathMockFactory
    with BlockchainStubHelpers
    with TransactionGen
    with WithDomain {

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
      stub[CommonTransactionsApi],
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
      },
      () => blockchain
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
    def routeWithBlockchain(blockchain: Blockchain with NG) =
      debugApiRoute.copy(blockchain = blockchain, priorityPoolBlockchain = () => blockchain).route

    def validatePost(tx: TransferTransaction) =
      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, tx.json().toString()))

    "takes the priority pool into account" in withDomain(DomainPresets.NG) { d =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
      d.appendBlock(TxHelpers.transfer(to = TxHelpers.secondAddress, amount = 1.waves + TestValues.fee))

      val route = debugApiRoute.copy(priorityPoolBlockchain = () => d.blockchain).route
      val tx    = TxHelpers.transfer(TxHelpers.secondSigner, TestValues.address, 1.waves)
      validatePost(tx) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
      }
    }

    "valid tx" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(Long.MaxValue)

      val route = routeWithBlockchain(blockchain)

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, 1.waves)
      validatePost(tx) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
      }
    }

    "invalid tx" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(0)

      val route = routeWithBlockchain(blockchain)

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, Long.MaxValue)
      validatePost(tx) ~> route ~> check {
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

      val route = routeWithBlockchain(blockchain)
      val tx    = TxHelpers.exchange(TxHelpers.order(OrderType.BUY, TestValues.asset), TxHelpers.order(OrderType.SELL, TestValues.asset))
      jsonPost(routePath("/validate"), tx.json()) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("not allowed by script of the asset")
        (json \ "trace").as[JsArray] shouldBe Json.parse(
          "[{\"type\":\"asset\",\"context\":\"orderAmount\",\"id\":\"5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx\",\"result\":\"failure\",\"vars\":[],\"error\":\"error\"}]"
        )
      }
    }

    "invoke tx with asset failing" in {
      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(*, *).returns(Long.MaxValue / 2)

        val (assetScript, assetScriptComplexity) = ScriptCompiler
          .compile(
            "let test = true\n" +
              "if test then throw(\"error\") else !test",
            ScriptEstimatorV3
          )
          .explicitGet()

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
               |     ScriptTransfer(Address(base58'${TxHelpers.secondAddress}'), 1, base58'${TestValues.asset}')
               |]
               |
               |@Callable(i)
               |func issue() = {
               |  let docimals = 4
               |  [Issue("name", "description", 1000, docimals, true, unit, 0)]
               |}
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
                Map(3 -> Seq("test", "dataAndTransfer", "issue", "reissue", "burn", "sponsorFee").map(_ -> 1L).toMap)
              )
            )
          )

        (blockchain.hasAccountScript _).when(*).returns(true)
      }

      val route = routeWithBlockchain(blockchain)
      def testFunction(name: String, result: InvokeScriptTransaction => String) = withClue(s"function $name") {
        val tx = TxHelpers.invoke(TxHelpers.defaultAddress, name, fee = 102500000)

        jsonPost(routePath("/validate"), tx.json()) ~> route ~> check {
          val json = Json.parse(responseAs[String])

          if ((json \ "valid").as[Boolean])
            assert(tx.json().fieldSet subsetOf json.as[JsObject].fieldSet)
          else
            (json \ "transaction").as[JsObject] shouldBe tx.json()

          (json \ "trace").as[JsArray] shouldBe Json.parse(result(tx))
        }
      }

      def testPayment(result: String) = withClue("payment") {
        val tx = TxHelpers.invoke(TxHelpers.secondAddress, "test", fee = 1300000, payments = Seq(Payment(1L, TestValues.asset)))

        jsonPost(routePath("/validate"), tx.json()) ~> route ~> check {
          val json = Json.parse(responseAs[String])

          if ((json \ "valid").as[Boolean])
            assert(tx.json().fieldSet subsetOf json.as[JsObject].fieldSet)
          else
            (json \ "transaction").as[JsObject] shouldBe tx.json()

          (json \ "trace").as[JsArray] shouldBe Json.parse(result)
        }
      }

      testPayment("""[ {
                    |  "type" : "verifier",
                    |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                    |  "result" : "success",
                    |  "error" : null
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
                    |    "leases" : [ ],
                    |    "leaseCancels" : [ ],
                    |    "invokes" : [ ]
                    |  },
                    |  "error" : null,
                    |  "vars" : [ ]
                    |}, {
                    |  "type" : "asset",
                    |  "context" : "payment",
                    |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
                    |  "result" : "failure",
                    |  "vars" : [ {
                    |    "name" : "test",
                    |    "type" : "Boolean",
                    |    "value" : true
                    |  } ],
                    |  "error" : "error"
                    |} ]""".stripMargin)

      testFunction(
        "dataAndTransfer",
        _ => """[ {
               |  "type" : "verifier",
               |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
               |  "result" : "success",
               |  "error" : null
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
               |    "leases" : [ ],
               |    "leaseCancels" : [ ],
               |    "invokes" : [ ]
               |  },
               |  "error" : null,
               |  "vars" : [ ]
               |}, {
               |  "type" : "asset",
               |  "context" : "transfer",
               |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
               |  "result" : "failure",
               |  "vars" : [ {
               |    "name" : "test",
               |    "type" : "Boolean",
               |    "value" : true
               |  } ],
               |  "error" : "error"
               |} ]""".stripMargin
      )

      testFunction(
        "issue",
        tx => s"""[ {
          |  "type" : "verifier",
          |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
          |  "result" : "success",
          |  "error" : null
          |}, {
          |  "type" : "dApp",
          |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
          |  "function" : "issue",
          |  "args" : [ ],
          |  "result" : {
          |    "data" : [ ],
          |    "transfers" : [ ],
          |    "issues" : [ {
          |      "assetId" : "${Issue.calculateId(4, "description", isReissuable = true, "name", 1000, 0, tx.id())}",
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
          |    "leases" : [ ],
          |    "leaseCancels" : [ ],
          |    "invokes" : [ ]
          |  },
          |  "error" : null,
          |  "vars" : [ {
          |    "name" : "docimals",
          |    "type" : "Int",
          |    "value" : 4
          |  } ]
          |} ]""".stripMargin
      )

      testFunction(
        "reissue",
        _ => """[ {
               |  "type" : "verifier",
               |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
               |  "result" : "success",
               |  "error" : null
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
               |    "leases" : [ ],
               |    "leaseCancels" : [ ],
               |    "invokes" : [ ]
               |  },
               |  "error" : null,
               |  "vars" : [ ]
               |}, {
               |  "type" : "asset",
               |  "context" : "reissue",
               |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
               |  "result" : "failure",
               |  "vars" : [ {
               |    "name" : "test",
               |    "type" : "Boolean",
               |    "value" : true
               |  } ],
               |  "error" : "error"
               |} ]""".stripMargin
      )

      testFunction(
        "burn",
        _ => """[ {
               |  "type" : "verifier",
               |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
               |  "result" : "success",
               |  "error" : null
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
               |    "leases" : [ ],
               |    "leaseCancels" : [ ],
               |    "invokes" : [ ]
               |  },
               |  "error" : null,
               |  "vars" : [ ]
               |}, {
               |  "type" : "asset",
               |  "context" : "burn",
               |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
               |  "result" : "failure",
               |  "vars" : [ {
               |    "name" : "test",
               |    "type" : "Boolean",
               |    "value" : true
               |  } ],
               |  "error" : "error"
               |} ]""".stripMargin
      )

    }

    "invoke tx returning leases" in {
      val dAppPk        = accountGen.sample.get.publicKey
      val invoke        = TxHelpers.invoke(dAppPk.toAddress, "test")
      val leaseCancelId = ByteStr(bytes32gen.sample.get)

      val amount1    = 100
      val recipient1 = Recipient.Address(ByteStr.decodeBase58("3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd").get)
      val nonce1     = 0
      val leaseId1   = Lease.calculateId(Lease(recipient1, amount1, nonce1), invoke.id.value())

      val amount2    = 20
      val recipient2 = Recipient.Alias("some_alias")
      val nonce2     = 2
      val leaseId2   = Lease.calculateId(Lease(recipient2, amount2, nonce2), invoke.id.value())

      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(*, *).returns(Long.MaxValue)

        val (dAppScript, _) = ScriptCompiler
          .compile(
            s"""
               |{-# STDLIB_VERSION 5 #-}
               |{-# SCRIPT_TYPE ACCOUNT #-}
               |{-# CONTENT_TYPE DAPP #-}
               |
               |@Callable(i)
               |func test() = {
               |  strict a = parseBigIntValue("${PureContext.BigIntMax}")
               |  let test = 1
               |  if (test == 1)
               |    then
               |      [
               |        Lease(Address(base58'${recipient1.bytes}'), $amount1, $nonce1),
               |        Lease(Alias("${recipient2.name}"), $amount2, $nonce2),
               |        LeaseCancel(base58'$leaseCancelId')
               |      ]
               |    else []
               |}
               |""".stripMargin,
            ScriptEstimatorV3
          )
          .explicitGet()

        (blockchain.accountScript _)
          .when(*)
          .returns(
            Some(
              AccountScriptInfo(
                dAppPk,
                dAppScript,
                0L,
                Map(3 -> Seq("test").map(_ -> 0L).toMap)
              )
            )
          )

        (blockchain.hasAccountScript _).when(*).returns(true)

        (blockchain.leaseDetails _)
          .when(leaseCancelId)
          .returns(Some(LeaseDetails(dAppPk, accountGen.sample.get.toAddress, 100, LeaseDetails.Status.Active, leaseCancelId, 1)))
          .anyNumberOfTimes()

        (blockchain.leaseDetails _)
          .when(*)
          .returns(None)
          .anyNumberOfTimes()

        (blockchain.resolveAlias _)
          .when(*)
          .returns(Right(accountGen.sample.get.toAddress))
          .anyNumberOfTimes()
      }
      val route = debugApiRoute
        .copy(
          blockchain = blockchain,
          priorityPoolBlockchain = () => blockchain
        )
        .route

      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, invoke.json().toString())) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "trace").as[JsArray] shouldBe Json.parse(
          s"""
            | [
            |  {
            |    "type": "verifier",
            |    "id": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
            |    "result": "success",
            |    "error": null
            |  },
            |  {
            |    "type": "dApp",
            |    "id": "${dAppPk.toAddress}",
            |    "function": "test",
            |    "args": [],
            |    "result": {
            |      "data": [],
            |      "transfers": [],
            |      "issues": [],
            |      "reissues": [],
            |      "burns": [],
            |      "sponsorFees": [],
            |      "leases": [
            |        {
            |          "recipient": "${recipient1.bytes}",
            |          "amount": $amount1,
            |          "nonce": $nonce1,
            |          "id": "$leaseId1"
            |        },
            |        {
            |          "recipient": "alias:T:${recipient2.name}",
            |          "amount": $amount2,
            |          "nonce": $nonce2,
            |          "id": "$leaseId2"
            |        }
            |      ],
            |      "leaseCancels": [
            |        {
            |          "id": "$leaseCancelId"
            |        }
            |      ],
            |      "invokes": []
            |    },
            |    "error": null,
            |    "vars": [
            |      {
            |        "name":"a",
            |        "type":"BigInt",
            |        "value":6.703903964971298549787012499102923E+153
            |      },
            |      {
            |        "name": "test",
            |        "type": "Int",
            |        "value": 1
            |      }
            |    ]
            |  }
            | ]
          """.stripMargin
        )
      }
    }

    "transfer transaction with asset fail" in {
      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(*, *).returns(Long.MaxValue / 2)

        val (assetScript, assetScriptComplexity) = ScriptCompiler.compile("false", ScriptEstimatorV3).explicitGet()
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
      }
      val route = routeWithBlockchain(blockchain)
      val tx    = TxHelpers.transfer(TxHelpers.defaultSigner, TxHelpers.defaultAddress, 1, TestValues.asset)

      jsonPost(routePath("/validate"), tx.json()) ~> route ~> check {
        val json = responseAs[JsObject]
        (json \ "trace").as[JsArray] shouldBe Json.parse("""[ {
                                                           |    "type" : "asset",
                                                           |    "context" : "transfer",
                                                           |    "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
                                                           |    "result" : "failure",
                                                           |    "vars" : [ ],
                                                           |    "error" : null
                                                           |  } ]""".stripMargin)

        (json \ "valid").as[Boolean] shouldBe false
        (json \ "transaction").as[JsObject] shouldBe tx.json()
      }
    }

    "txs with empty and small verifier" in {
      val blockchain = createBlockchainStub { blockchain =>
        val settings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = Map(
            BlockchainFeatures.SmartAccounts.id    -> 0,
            BlockchainFeatures.SmartAssets.id      -> 0,
            BlockchainFeatures.Ride4DApps.id       -> 0,
            BlockchainFeatures.FeeSponsorship.id   -> 0,
            BlockchainFeatures.DataTransaction.id  -> 0,
            BlockchainFeatures.BlockReward.id      -> 0,
            BlockchainFeatures.BlockV5.id          -> 0,
            BlockchainFeatures.SynchronousCalls.id -> 0
          ),
          featureCheckBlocksPeriod = 1,
          blocksForFeatureActivation = 1
        )
        (() => blockchain.settings).when().returns(WavesSettings.default().blockchainSettings.copy(functionalitySettings = settings))
        (() => blockchain.activatedFeatures).when().returns(settings.preActivatedFeatures)
        (blockchain.balance _).when(*, *).returns(ENOUGH_AMT)

        val script                = ExprScript(TRUE).explicitGet()
        def info(complexity: Int) = Some(AccountScriptInfo(TxHelpers.secondSigner.publicKey, script, complexity))

        (blockchain.accountScript _).when(TxHelpers.defaultSigner.toAddress).returns(info(199))
        (blockchain.accountScript _).when(TxHelpers.secondSigner.toAddress).returns(info(201))
        (blockchain.accountScript _).when(TxHelpers.signer(3).toAddress).returns(None)
      }
      val route       = routeWithBlockchain(blockchain)
      val transferFee = 100000

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TxHelpers.secondSigner.toAddress, 1.waves, fee = transferFee, version = TxVersion.V2)
      validatePost(tx) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe true
      }

      val tx2 = TxHelpers.transfer(TxHelpers.secondSigner, TestValues.address, 1.waves, fee = transferFee, version = TxVersion.V2)
      validatePost(tx2) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "error").as[String] should include("Requires 400000 extra fee")
      }

      val tx3 = TxHelpers.transfer(TxHelpers.signer(3), TestValues.address, 1.waves, fee = transferFee, version = TxVersion.V2)
      validatePost(tx3) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe true
      }
    }
  }

  routePath("/stateChanges/info/") - {
    "provides lease and lease cancel actions stateChanges" in {
      val invokeAddress    = accountGen.sample.get.toAddress
      val leaseId1         = ByteStr(bytes32gen.sample.get)
      val leaseId2         = ByteStr(bytes32gen.sample.get)
      val leaseCancelId    = ByteStr(bytes32gen.sample.get)
      val recipientAddress = accountGen.sample.get.toAddress
      val recipientAlias   = aliasGen.sample.get
      val invoke           = TxHelpers.invoke(invokeAddress, "test")
      val scriptResult = InvokeScriptResult(
        leases = Seq(InvokeScriptResult.Lease(recipientAddress, 100, 1, leaseId1), InvokeScriptResult.Lease(recipientAlias, 200, 3, leaseId2)),
        leaseCancels = Seq(LeaseCancel(leaseCancelId))
      )

      (() => blockchain.activatedFeatures).when().returning(Map.empty).anyNumberOfTimes()
      (transactionsApi.transactionById _)
        .when(invoke.id())
        .returning(Some(TransactionMeta.Invoke(Height(1), invoke, succeeded = true, Some(scriptResult))))
        .once()

      (blockchain.leaseDetails _)
        .when(leaseId1)
        .returning(Some(LeaseDetails(invoke.sender, recipientAddress, 100, LeaseDetails.Status.Active, invoke.id(), 1)))
      (blockchain.leaseDetails _)
        .when(leaseId2)
        .returning(Some(LeaseDetails(invoke.sender, recipientAddress, 100, LeaseDetails.Status.Active, invoke.id(), 1)))
      (blockchain.leaseDetails _)
        .when(leaseCancelId)
        .returning(Some(LeaseDetails(invoke.sender, recipientAddress, 100, LeaseDetails.Status.Cancelled(2, Some(leaseCancelId)), invoke.id(), 1)))
      (blockchain.transactionMeta _).when(invoke.id()).returning(Some((1, true)))

      Get(routePath(s"/stateChanges/info/${invoke.id()}")) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        val json = (responseAs[JsObject] \ "stateChanges").as[JsObject]
        json should matchJson(s"""
                                   |{
                                   |  "data" : [ ],
                                   |  "transfers" : [ ],
                                   |  "issues" : [ ],
                                   |  "reissues" : [ ],
                                   |  "burns" : [ ],
                                   |  "sponsorFees" : [ ],
                                   |  "leases" : [ {
                                   |    "id" : "$leaseId1",
                                   |    "originTransactionId" : "${invoke.id()}",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "$recipientAddress",
                                   |    "amount" : 100,
                                   |    "height" : 1,
                                   |    "status" : "active"
                                   |  }, {
                                   |    "id" : "$leaseId2",
                                   |    "originTransactionId" : "${invoke.id()}",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "$recipientAddress",
                                   |    "amount" : 100,
                                   |    "height" : 1,
                                   |    "status" : "active"
                                   |  } ],
                                   |  "leaseCancels" : [ {
                                   |    "id" : "$leaseCancelId",
                                   |    "originTransactionId" : "${invoke.id()}",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "$recipientAddress",
                                   |    "amount" : 100,
                                   |    "height" : 1,
                                   |    "status" : "canceled"
                                   |  } ],
                                   |  "invokes" : [ ]
                                   |}""".stripMargin)
      }
    }
  }

  private[this] def jsonPost(path: String, json: JsValue) = {
    Post(path, HttpEntity(ContentTypes.`application/json`, json.toString()))
  }
}
