package com.wavesplatform.http

import java.util.concurrent.TimeUnit

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import com.typesafe.config.ConfigObject
import com.wavesplatform.account.{Alias, KeyPair}
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.api.http.DebugApiRoute.AccountMiningInfo
import com.wavesplatform.api.http.{DebugApiRoute, RouteTimeout, handleAllExceptions}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease, Recipient}
import com.wavesplatform.mining.{Miner, MinerDebugInfo}
import com.wavesplatform.network.PeerDatabase
import com.wavesplatform.settings.{TestFunctionalitySettings, WalletSettings, WavesSettings}
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, Blockchain, Height, NG, StateHash, TxMeta}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{ERC20Address, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.SharedSchedulerMixin
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.*
import monix.eval.Task
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{Assertion, OptionValues}
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import scala.concurrent.duration.*
import scala.util.Random

//noinspection ScalaStyle
class DebugApiRouteSpec
    extends RouteSpec("/debug")
    with RestAPISettingsHelper
    with TestWallet
    with NTPTime
    with PathMockFactory
    with BlockchainStubHelpers
    with WithDomain
    with OptionValues
    with SharedSchedulerMixin {
  import DomainPresets.*

  val wavesSettings: WavesSettings = WavesSettings.default().copy(restAPISettings = restAPISettings)
  val configObject: ConfigObject   = wavesSettings.config.root()

  trait Blockchain1 extends Blockchain with NG
  val blockchain: Blockchain1 = stub[Blockchain1]
  (blockchain.hasAccountScript _).when(*).returns(false)
  (() => blockchain.microblockIds).when().returns(Seq.empty)
  (blockchain.heightOf _).when(*).returns(None)
  (() => blockchain.height).when().returns(0)
  (blockchain.balanceSnapshots _).when(*, *, *).returns(Seq.empty)
  (blockchain.effectiveBalanceBanHeights _).when(*).returns(Seq.empty)

  val miner: Miner & MinerDebugInfo = new Miner with MinerDebugInfo {
    override def scheduleMining(blockchain: Option[Blockchain]): Unit                           = ()
    override def getNextBlockGenerationOffset(account: KeyPair): Either[String, FiniteDuration] = Right(FiniteDuration(0, TimeUnit.SECONDS))
    override def state: MinerDebugInfo.State                                                    = MinerDebugInfo.Disabled
  }

  val block: Block = TestBlock.create(Nil).block
  val testStateHash: StateHash = {
    def randomHash: ByteStr = ByteStr(Array.fill(32)(Random.nextInt(256).toByte))
    val hashes              = SectionId.values.map((_, randomHash)).toMap
    StateHash(randomHash, hashes)
  }

  val wallet: Wallet = Wallet(WalletSettings(None, Some("password"), Some(ByteStr(TxHelpers.defaultSigner.seed))))
  val debugApiRoute: DebugApiRoute =
    DebugApiRoute(
      wavesSettings,
      ntpTime,
      blockchain,
      wallet,
      null,
      stub[CommonTransactionsApi],
      null,
      PeerDatabase.NoOp,
      null,
      (_, _) => Task.raiseError(new NotImplementedError("")),
      null,
      miner,
      null,
      null,
      null,
      null,
      configObject,
      _ => Seq.empty,
      {
        case 2 => Some(testStateHash)
        case _ => None
      },
      () => Some(blockchain),
      new RouteTimeout(60.seconds)(sharedScheduler),
      sharedScheduler
    )
  import debugApiRoute.*

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }

  routePath("/balances/history/{address}") - {
    val acc1 = TxHelpers.defaultSigner
    val acc2 = TxHelpers.secondSigner

    val initBalance = 5.waves

    "works" in withDomain(balances = Seq(AddrWithBalance(acc2.toAddress, initBalance), AddrWithBalance(acc1.toAddress))) { d =>
      val tx1 = TxHelpers.transfer(acc2, acc1.toAddress, 1.waves)
      val tx2 = TxHelpers.transfer(acc1, acc2.toAddress, 3.waves)
      val tx3 = TxHelpers.transfer(acc2, acc1.toAddress, 4.waves)
      val tx4 = TxHelpers.transfer(acc1, acc2.toAddress, 5.waves)

      d.appendBlock(tx1)
      d.appendBlock(tx2)
      d.appendBlock()
      d.appendBlock(tx3)
      d.appendBlock(tx4)
      d.appendBlock()

      val expectedBalance2 = initBalance - tx1.fee.value - tx1.amount.value
      val expectedBalance3 = expectedBalance2 + tx2.amount.value
      val expectedBalance5 = expectedBalance3 - tx3.fee.value - tx3.amount.value
      val expectedBalance6 = expectedBalance5 + tx4.amount.value

      Get(routePath(s"/balances/history/${acc2.toAddress}")) ~> routeWithBlockchain(d) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[JsArray] shouldBe Json.toJson(
          Seq(
            6 -> expectedBalance6,
            5 -> expectedBalance5,
            3 -> expectedBalance3,
            2 -> expectedBalance2,
            1 -> initBalance
          ).map { case (height, balance) =>
            Json.obj("height" -> height, "balance" -> balance)
          }
        )
      }
    }
  }

  routePath("/stateHash") - {
    "works" - {
      val settingsWithStateHashes = DomainPresets.SettingsFromDefaultConfig.copy(
        dbSettings = DomainPresets.SettingsFromDefaultConfig.dbSettings.copy(storeStateHashes = true)
      )

      "at nonexistent height" in withDomain(settingsWithStateHashes) { d =>
        d.appendBlock(TestBlock.create(Nil).block)
        Get(routePath("/stateHash/2")) ~> routeWithBlockchain(d) ~> check {
          status shouldBe StatusCodes.NotFound
        }
      }

      "at existing height" in expectStateHashAt2("2")
      "last" in expectStateHashAt2("last")

      def expectStateHashAt2(suffix: String): Assertion = withDomain(settingsWithStateHashes) { d =>
        val genesisBlock = TestBlock.create(Nil).block
        d.appendBlock(genesisBlock)

        val blockAt2 = TestBlock.create(0, genesisBlock.id(), Nil).block
        d.appendBlock(blockAt2)
        d.appendBlock(TestBlock.create(0, blockAt2.id(), Nil).block)

        val stateHashAt2 = d.rocksDBWriter.loadStateHash(2).value
        Get(routePath(s"/stateHash/$suffix")) ~> routeWithBlockchain(d) ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe (Json.toJson(stateHashAt2).as[JsObject] ++ Json.obj(
            "blockId" -> blockAt2.id().toString,
            "height"  -> 2,
            "version" -> Version.VersionString
          ))
        }
      }
    }
  }

  routePath("/validate") - {
    def validatePost(tx: Transaction) =
      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, tx.json().toString()))

    "takes the priority pool into account" in withDomain(balances = Seq(AddrWithBalance(TxHelpers.defaultAddress))) { d =>
      d.appendBlock(TxHelpers.transfer(to = TxHelpers.secondAddress, amount = 1.waves + TestValues.fee))

      val route = routeWithBlockchain(d.blockchain)
      val tx    = TxHelpers.transfer(TxHelpers.secondSigner, TestValues.address, 1.waves)
      validatePost(tx) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
      }
    }

    "valid tx" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(Long.MaxValue)
      (blockchain.wavesBalances _).when(Seq(TxHelpers.defaultAddress)).returns(Map(TxHelpers.defaultAddress -> Long.MaxValue))

      val route = routeWithBlockchain(blockchain)

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, 1.waves)
      validatePost(tx) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
      }
    }

    "invalid tx" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(0)
      (blockchain.wavesBalances _).when(Seq(TxHelpers.defaultAddress)).returns(Map(TxHelpers.defaultAddress -> 0))

      val route = routeWithBlockchain(blockchain)

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, ENOUGH_AMT)
      validatePost(tx) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("Attempt to transfer unavailable funds")
      }
    }

    "NoSuchElementException" in {
      val blockchain = createBlockchainStub { b =>
        (b.accountScript _).when(*).throws(new NoSuchElementException())
      }
      val route = handleAllExceptions(routeWithBlockchain(blockchain))
      validatePost(TxHelpers.invoke()) ~> route ~> check {
        responseAs[String] shouldBe """{"error":0,"message":"Error is unknown"}"""
        response.status shouldBe StatusCodes.InternalServerError
      }
    }

    "exchange tx with fail script" in {
      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(TxHelpers.defaultAddress, *).returns(Long.MaxValue)
        (blockchain.wavesBalances _).when(Seq(TxHelpers.defaultAddress)).returns(Map(TxHelpers.defaultAddress -> Long.MaxValue))

        val (assetScript, comp) =
          ScriptCompiler.compile("if true then throw(\"error\") else false", ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()
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
                nft = false,
                0,
                Height(1)
              )
            )
          )
        blockchain.stub.activateAllFeatures()
      }

      val route = routeWithBlockchain(blockchain)
      val tx = TxHelpers.exchangeFromOrders(TxHelpers.orderV3(OrderType.BUY, TestValues.asset), TxHelpers.orderV3(OrderType.SELL, TestValues.asset))
      jsonPost(routePath("/validate"), tx.json()) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("not allowed by script of the asset")
        (json \ "trace").as[JsArray] shouldBe Json.parse(
          "[{\"type\":\"asset\",\"context\":\"orderAmount\",\"id\":\"5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx\",\"result\":\"failure\",\"vars\":[{\"name\":\"throw.@args\",\"type\":\"Array\",\"value\":[{\"type\":\"String\",\"value\":\"error\"}]},{\"name\":\"throw.@complexity\",\"type\":\"Int\",\"value\":1},{\"name\":\"@complexityLimit\",\"type\":\"Int\",\"value\":2147483646}],\"error\":\"error\"}]"
        )
      }
    }

    "invoke tx with asset failing" in {
      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(*, *).returns(Long.MaxValue / 2)
        (blockchain.wavesBalances _).when(*).returns(Map(TxHelpers.defaultAddress -> Long.MaxValue / 2))

        val (assetScript, assetScriptComplexity) = ScriptCompiler
          .compile(
            "let test = true\n" +
              "if test then throw(\"error\") else !test",
            ScriptEstimatorV3(fixOverflow = true, overhead = true)
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
                nft = false,
                0,
                Height(1)
              )
            )
          )

        (blockchain.resolveERC20Address _).when(ERC20Address(TestValues.asset)).returns(Some(TestValues.asset))
        (blockchain.resolveERC20Address _).when(*).returns(None)

        val (dAppScript, _) = ScriptCompiler
          .compile(
            s"""
               |{-# STDLIB_VERSION 4 #-}
               |{-# SCRIPT_TYPE ACCOUNT #-}
               |{-# CONTENT_TYPE DAPP #-}
               |
               |@Callable(i)
               |func default() = []
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
               |  let decimals = 4
               |  [Issue("name", "description", 1000, decimals, true, unit, 0)]
               |}
               |
               |@Callable(i)
               |func reissue() = [Reissue(base58'${TestValues.asset}', 1, false)]
               |
               |@Callable(i)
               |func burn() = [Burn(base58'${TestValues.asset}', 1)]
               |""".stripMargin,
            ScriptEstimatorV3(fixOverflow = true, overhead = true)
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
                Map(3 -> Seq("default", "dataAndTransfer", "issue", "reissue", "burn", "sponsorFee").map(_ -> 1L).toMap)
              )
            )
          )

        (blockchain.hasAccountScript _).when(*).returns(true)
        blockchain.stub.activateAllFeatures()
      }

      val route = routeWithBlockchain(blockchain)
      def testFunction(name: String, result: InvokeScriptTransaction => String) = withClue(s"function $name") {
        val tx = TxHelpers.invoke(TxHelpers.defaultAddress, func = Some(name), fee = 102500000)

        jsonPost(routePath("/validate"), tx.json()) ~> route ~> check {
          val json = responseAs[JsValue]

          if ((json \ "valid").as[Boolean])
            assert(tx.json().fieldSet subsetOf json.as[JsObject].fieldSet)
          else
            (json \ "transaction").as[JsObject] should matchJson(tx.json())

          (json \ "trace").as[JsArray] should matchJson(result(tx))
        }
      }

      def testPayment(result: InvokeScriptTransaction => String) = withClue("payment") {
        val tx = TxHelpers.invoke(TxHelpers.secondAddress, fee = 1300000, payments = Seq(Payment(1L, TestValues.asset)))

        jsonPost(routePath("/validate"), tx.json()) ~> route ~> check {
          val json = responseAs[JsValue]

          if ((json \ "valid").as[Boolean])
            assert(tx.json().fieldSet subsetOf json.as[JsObject].fieldSet)
          else
            (json \ "transaction").as[JsObject] should matchJson(tx.json())

          (json \ "trace").as[JsArray] should matchJson(Json.parse(result(tx)))
        }
      }

      testPayment(tx => s"""[ {
                           |  "type" : "verifier",
                           |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                           |  "result" : "success",
                           |  "error" : null
                           |}, {
                           |  "type" : "dApp",
                           |  "id" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                           |  "function" : "default",
                           |  "args" : [ ],
                           |  "invocations" : [ ],
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
                           |  "vars" : [ {
                           |    "name" : "i",
                           |    "type" : "Invocation",
                           |    "value" : {
                           |      "payments" : {
                           |        "type" : "Array",
                           |        "value" : [ {
                           |          "type" : "AttachedPayment",
                           |          "value" : {
                           |            "amount" : {
                           |              "type" : "Int",
                           |              "value" : 1
                           |            },
                           |            "assetId" : {
                           |              "type" : "ByteVector",
                           |              "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                           |            }
                           |          }
                           |        } ]
                           |      },
                           |      "callerPublicKey" : {
                           |        "type" : "ByteVector",
                           |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
                           |      },
                           |      "feeAssetId" : {
                           |        "type" : "Unit",
                           |        "value" : { }
                           |      },
                           |      "transactionId" : {
                           |        "type" : "ByteVector",
                           |        "value" : "${tx.id()}"
                           |      },
                           |      "caller" : {
                           |        "type" : "Address",
                           |        "value" : {
                           |          "bytes" : {
                           |            "type" : "ByteVector",
                           |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
                           |          }
                           |        }
                           |      },
                           |      "fee" : {
                           |        "type" : "Int",
                           |        "value" : 1300000
                           |      }
                           |    }
                           |  }, {
                           |    "name" : "default.@args",
                           |    "type" : "Array",
                           |    "value" : [ ]
                           |  }, {
                           |    "name" : "default.@complexity",
                           |    "type" : "Int",
                           |    "value" : 1
                           |  }, {
                           |    "name" : "@complexityLimit",
                           |    "type" : "Int",
                           |    "value" : 51994
                           |  } ]
                           |}, {
                           |  "type" : "asset",
                           |  "context" : "payment",
                           |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
                           |  "result" : "failure",
                           |  "vars" : [ {
                           |    "name" : "test",
                           |    "type" : "Boolean",
                           |    "value" : true
                           |  }, {
                           |    "name" : "throw.@args",
                           |    "type" : "Array",
                           |    "value" : [ {
                           |      "type" : "String",
                           |      "value" : "error"
                           |    } ]
                           |  }, {
                           |    "name" : "throw.@complexity",
                           |    "type" : "Int",
                           |    "value" : 1
                           |  }, {
                           |    "name" : "@complexityLimit",
                           |    "type" : "Int",
                           |    "value" : 2147483646
                           |  } ],
                           |  "error" : "error"
                           |} ]""".stripMargin)

      testFunction(
        "dataAndTransfer",
        tx => s"""[ {
                 |  "type" : "verifier",
                 |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                 |  "result" : "success",
                 |  "error" : null
                 |}, {
                 |  "type" : "dApp",
                 |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                 |  "function" : "dataAndTransfer",
                 |  "args" : [ ],
                 |  "invocations" : [ ],
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
                 |  "vars" : [ {
                 |    "name" : "i",
                 |    "type" : "Invocation",
                 |    "value" : {
                 |      "payments" : {
                 |        "type" : "Array",
                 |        "value" : [ ]
                 |      },
                 |      "callerPublicKey" : {
                 |        "type" : "ByteVector",
                 |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
                 |      },
                 |      "feeAssetId" : {
                 |        "type" : "Unit",
                 |        "value" : { }
                 |      },
                 |      "transactionId" : {
                 |        "type" : "ByteVector",
                 |        "value" : "${tx.id()}"
                 |      },
                 |      "caller" : {
                 |        "type" : "Address",
                 |        "value" : {
                 |          "bytes" : {
                 |            "type" : "ByteVector",
                 |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
                 |          }
                 |        }
                 |      },
                 |      "fee" : {
                 |        "type" : "Int",
                 |        "value" : 102500000
                 |      }
                 |    }
                 |  }, {
                 |    "name" : "dataAndTransfer.@args",
                 |    "type" : "Array",
                 |    "value" : [ ]
                 |  }, {
                 |    "name" : "IntegerEntry.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "String",
                 |      "value" : "key"
                 |    }, {
                 |      "type" : "Int",
                 |      "value" : 1
                 |    } ]
                 |  }, {
                 |    "name" : "IntegerEntry.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51999
                 |  }, {
                 |    "name" : "BooleanEntry.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "String",
                 |      "value" : "key"
                 |    }, {
                 |      "type" : "Boolean",
                 |      "value" : true
                 |    } ]
                 |  }, {
                 |    "name" : "BooleanEntry.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51998
                 |  }, {
                 |    "name" : "StringEntry.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "String",
                 |      "value" : "key"
                 |    }, {
                 |      "type" : "String",
                 |      "value" : "str"
                 |    } ]
                 |  }, {
                 |    "name" : "StringEntry.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51997
                 |  }, {
                 |    "name" : "BinaryEntry.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "String",
                 |      "value" : "key"
                 |    }, {
                 |      "type" : "ByteVector",
                 |      "value" : ""
                 |    } ]
                 |  }, {
                 |    "name" : "BinaryEntry.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51996
                 |  }, {
                 |    "name" : "DeleteEntry.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "String",
                 |      "value" : "key"
                 |    } ]
                 |  }, {
                 |    "name" : "DeleteEntry.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51995
                 |  }, {
                 |    "name" : "Address.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "ByteVector",
                 |      "value" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
                 |    } ]
                 |  }, {
                 |    "name" : "Address.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51994
                 |  }, {
                 |    "name" : "ScriptTransfer.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "Address",
                 |      "value" : {
                 |        "bytes" : {
                 |          "type" : "ByteVector",
                 |          "value" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Int",
                 |      "value" : 1
                 |    }, {
                 |      "type" : "ByteVector",
                 |      "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |    } ]
                 |  }, {
                 |    "name" : "ScriptTransfer.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51993
                 |  }, {
                 |    "name" : "cons.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "ScriptTransfer",
                 |      "value" : {
                 |        "recipient" : {
                 |          "type" : "Address",
                 |          "value" : {
                 |            "bytes" : {
                 |              "type" : "ByteVector",
                 |              "value" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
                 |            }
                 |          }
                 |        },
                 |        "amount" : {
                 |          "type" : "Int",
                 |          "value" : 1
                 |        },
                 |        "asset" : {
                 |          "type" : "ByteVector",
                 |          "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Array",
                 |      "value" : [ ]
                 |    } ]
                 |  }, {
                 |    "name" : "cons.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51992
                 |  }, {
                 |    "name" : "cons.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "DeleteEntry",
                 |      "value" : {
                 |        "key" : {
                 |          "type" : "String",
                 |          "value" : "key"
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Array",
                 |      "value" : [ {
                 |        "type" : "ScriptTransfer",
                 |        "value" : {
                 |          "recipient" : {
                 |            "type" : "Address",
                 |            "value" : {
                 |              "bytes" : {
                 |                "type" : "ByteVector",
                 |                "value" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
                 |              }
                 |            }
                 |          },
                 |          "amount" : {
                 |            "type" : "Int",
                 |            "value" : 1
                 |          },
                 |          "asset" : {
                 |            "type" : "ByteVector",
                 |            "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |          }
                 |        }
                 |      } ]
                 |    } ]
                 |  }, {
                 |    "name" : "cons.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51991
                 |  }, {
                 |    "name" : "cons.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "BinaryEntry",
                 |      "value" : {
                 |        "key" : {
                 |          "type" : "String",
                 |          "value" : "key"
                 |        },
                 |        "value" : {
                 |          "type" : "ByteVector",
                 |          "value" : ""
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Array",
                 |      "value" : [ {
                 |        "type" : "DeleteEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "ScriptTransfer",
                 |        "value" : {
                 |          "recipient" : {
                 |            "type" : "Address",
                 |            "value" : {
                 |              "bytes" : {
                 |                "type" : "ByteVector",
                 |                "value" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
                 |              }
                 |            }
                 |          },
                 |          "amount" : {
                 |            "type" : "Int",
                 |            "value" : 1
                 |          },
                 |          "asset" : {
                 |            "type" : "ByteVector",
                 |            "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |          }
                 |        }
                 |      } ]
                 |    } ]
                 |  }, {
                 |    "name" : "cons.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51990
                 |  }, {
                 |    "name" : "cons.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "StringEntry",
                 |      "value" : {
                 |        "key" : {
                 |          "type" : "String",
                 |          "value" : "key"
                 |        },
                 |        "value" : {
                 |          "type" : "String",
                 |          "value" : "str"
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Array",
                 |      "value" : [ {
                 |        "type" : "BinaryEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          },
                 |          "value" : {
                 |            "type" : "ByteVector",
                 |            "value" : ""
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "DeleteEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "ScriptTransfer",
                 |        "value" : {
                 |          "recipient" : {
                 |            "type" : "Address",
                 |            "value" : {
                 |              "bytes" : {
                 |                "type" : "ByteVector",
                 |                "value" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
                 |              }
                 |            }
                 |          },
                 |          "amount" : {
                 |            "type" : "Int",
                 |            "value" : 1
                 |          },
                 |          "asset" : {
                 |            "type" : "ByteVector",
                 |            "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |          }
                 |        }
                 |      } ]
                 |    } ]
                 |  }, {
                 |    "name" : "cons.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51989
                 |  }, {
                 |    "name" : "cons.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "BooleanEntry",
                 |      "value" : {
                 |        "key" : {
                 |          "type" : "String",
                 |          "value" : "key"
                 |        },
                 |        "value" : {
                 |          "type" : "Boolean",
                 |          "value" : true
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Array",
                 |      "value" : [ {
                 |        "type" : "StringEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          },
                 |          "value" : {
                 |            "type" : "String",
                 |            "value" : "str"
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "BinaryEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          },
                 |          "value" : {
                 |            "type" : "ByteVector",
                 |            "value" : ""
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "DeleteEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "ScriptTransfer",
                 |        "value" : {
                 |          "recipient" : {
                 |            "type" : "Address",
                 |            "value" : {
                 |              "bytes" : {
                 |                "type" : "ByteVector",
                 |                "value" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
                 |              }
                 |            }
                 |          },
                 |          "amount" : {
                 |            "type" : "Int",
                 |            "value" : 1
                 |          },
                 |          "asset" : {
                 |            "type" : "ByteVector",
                 |            "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |          }
                 |        }
                 |      } ]
                 |    } ]
                 |  }, {
                 |    "name" : "cons.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51988
                 |  }, {
                 |    "name" : "cons.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "IntegerEntry",
                 |      "value" : {
                 |        "key" : {
                 |          "type" : "String",
                 |          "value" : "key"
                 |        },
                 |        "value" : {
                 |          "type" : "Int",
                 |          "value" : 1
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Array",
                 |      "value" : [ {
                 |        "type" : "BooleanEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          },
                 |          "value" : {
                 |            "type" : "Boolean",
                 |            "value" : true
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "StringEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          },
                 |          "value" : {
                 |            "type" : "String",
                 |            "value" : "str"
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "BinaryEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          },
                 |          "value" : {
                 |            "type" : "ByteVector",
                 |            "value" : ""
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "DeleteEntry",
                 |        "value" : {
                 |          "key" : {
                 |            "type" : "String",
                 |            "value" : "key"
                 |          }
                 |        }
                 |      }, {
                 |        "type" : "ScriptTransfer",
                 |        "value" : {
                 |          "recipient" : {
                 |            "type" : "Address",
                 |            "value" : {
                 |              "bytes" : {
                 |                "type" : "ByteVector",
                 |                "value" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
                 |              }
                 |            }
                 |          },
                 |          "amount" : {
                 |            "type" : "Int",
                 |            "value" : 1
                 |          },
                 |          "asset" : {
                 |            "type" : "ByteVector",
                 |            "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |          }
                 |        }
                 |      } ]
                 |    } ]
                 |  }, {
                 |    "name" : "cons.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51987
                 |  } ]
                 |}, {
                 |  "type" : "asset",
                 |  "context" : "transfer",
                 |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
                 |  "result" : "failure",
                 |  "vars" : [ {
                 |    "name" : "test",
                 |    "type" : "Boolean",
                 |    "value" : true
                 |  }, {
                 |    "name" : "throw.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "String",
                 |      "value" : "error"
                 |    } ]
                 |  }, {
                 |    "name" : "throw.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 2147483646
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
                 |  "invocations" : [ ],
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
                 |    "name" : "i",
                 |    "type" : "Invocation",
                 |    "value" : {
                 |      "payments" : {
                 |        "type" : "Array",
                 |        "value" : [ ]
                 |      },
                 |      "callerPublicKey" : {
                 |        "type" : "ByteVector",
                 |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
                 |      },
                 |      "feeAssetId" : {
                 |        "type" : "Unit",
                 |        "value" : { }
                 |      },
                 |      "transactionId" : {
                 |        "type" : "ByteVector",
                 |        "value" : "${tx.id()}"
                 |      },
                 |      "caller" : {
                 |        "type" : "Address",
                 |        "value" : {
                 |          "bytes" : {
                 |            "type" : "ByteVector",
                 |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
                 |          }
                 |        }
                 |      },
                 |      "fee" : {
                 |        "type" : "Int",
                 |        "value" : 102500000
                 |      }
                 |    }
                 |  }, {
                 |    "name" : "issue.@args",
                 |    "type" : "Array",
                 |    "value" : [ ]
                 |  }, {
                 |    "name" : "decimals",
                 |    "type" : "Int",
                 |    "value" : 4
                 |  }, {
                 |    "name" : "Issue.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "String",
                 |      "value" : "name"
                 |    }, {
                 |      "type" : "String",
                 |      "value" : "description"
                 |    }, {
                 |      "type" : "Int",
                 |      "value" : 1000
                 |    }, {
                 |      "type" : "Int",
                 |      "value" : 4
                 |    }, {
                 |      "type" : "Boolean",
                 |      "value" : true
                 |    }, {
                 |      "type" : "Unit",
                 |      "value" : { }
                 |    }, {
                 |      "type" : "Int",
                 |      "value" : 0
                 |    } ]
                 |  }, {
                 |    "name" : "Issue.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51999
                 |  }, {
                 |    "name" : "cons.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "Issue",
                 |      "value" : {
                 |        "isReissuable" : {
                 |          "type" : "Boolean",
                 |          "value" : true
                 |        },
                 |        "nonce" : {
                 |          "type" : "Int",
                 |          "value" : 0
                 |        },
                 |        "description" : {
                 |          "type" : "String",
                 |          "value" : "description"
                 |        },
                 |        "decimals" : {
                 |          "type" : "Int",
                 |          "value" : 4
                 |        },
                 |        "compiledScript" : {
                 |          "type" : "Unit",
                 |          "value" : { }
                 |        },
                 |        "name" : {
                 |          "type" : "String",
                 |          "value" : "name"
                 |        },
                 |        "quantity" : {
                 |          "type" : "Int",
                 |          "value" : 1000
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Array",
                 |      "value" : [ ]
                 |    } ]
                 |  }, {
                 |    "name" : "cons.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51998
                 |  } ]
                 |} ]""".stripMargin
      )

      testFunction(
        "reissue",
        tx => s"""[ {
                 |  "type" : "verifier",
                 |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                 |  "result" : "success",
                 |  "error" : null
                 |}, {
                 |  "type" : "dApp",
                 |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                 |  "function" : "reissue",
                 |  "args" : [ ],
                 |  "invocations" : [ ],
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
                 |  "vars" : [ {
                 |    "name" : "i",
                 |    "type" : "Invocation",
                 |    "value" : {
                 |      "payments" : {
                 |        "type" : "Array",
                 |        "value" : [ ]
                 |      },
                 |      "callerPublicKey" : {
                 |        "type" : "ByteVector",
                 |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
                 |      },
                 |      "feeAssetId" : {
                 |        "type" : "Unit",
                 |        "value" : { }
                 |      },
                 |      "transactionId" : {
                 |        "type" : "ByteVector",
                 |        "value" : "${tx.id()}"
                 |      },
                 |      "caller" : {
                 |        "type" : "Address",
                 |        "value" : {
                 |          "bytes" : {
                 |            "type" : "ByteVector",
                 |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
                 |          }
                 |        }
                 |      },
                 |      "fee" : {
                 |        "type" : "Int",
                 |        "value" : 102500000
                 |      }
                 |    }
                 |  }, {
                 |    "name" : "reissue.@args",
                 |    "type" : "Array",
                 |    "value" : [ ]
                 |  }, {
                 |    "name" : "Reissue.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "ByteVector",
                 |      "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |    }, {
                 |      "type" : "Int",
                 |      "value" : 1
                 |    }, {
                 |      "type" : "Boolean",
                 |      "value" : false
                 |    } ]
                 |  }, {
                 |    "name" : "Reissue.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51999
                 |  }, {
                 |    "name" : "cons.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "Reissue",
                 |      "value" : {
                 |        "assetId" : {
                 |          "type" : "ByteVector",
                 |          "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |        },
                 |        "quantity" : {
                 |          "type" : "Int",
                 |          "value" : 1
                 |        },
                 |        "isReissuable" : {
                 |          "type" : "Boolean",
                 |          "value" : false
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Array",
                 |      "value" : [ ]
                 |    } ]
                 |  }, {
                 |    "name" : "cons.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51998
                 |  } ]
                 |}, {
                 |  "type" : "asset",
                 |  "context" : "reissue",
                 |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
                 |  "result" : "failure",
                 |  "vars" : [ {
                 |    "name" : "test",
                 |    "type" : "Boolean",
                 |    "value" : true
                 |  }, {
                 |    "name" : "throw.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "String",
                 |      "value" : "error"
                 |    } ]
                 |  }, {
                 |    "name" : "throw.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 2147483646
                 |  } ],
                 |  "error" : "error"
                 |} ]""".stripMargin
      )

      testFunction(
        "burn",
        tx => s"""[ {
                 |  "type" : "verifier",
                 |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                 |  "result" : "success",
                 |  "error" : null
                 |}, {
                 |  "type" : "dApp",
                 |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                 |  "function" : "burn",
                 |  "args" : [ ],
                 |  "invocations" : [ ],
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
                 |  "vars" : [ {
                 |    "name" : "i",
                 |    "type" : "Invocation",
                 |    "value" : {
                 |      "payments" : {
                 |        "type" : "Array",
                 |        "value" : [ ]
                 |      },
                 |      "callerPublicKey" : {
                 |        "type" : "ByteVector",
                 |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
                 |      },
                 |      "feeAssetId" : {
                 |        "type" : "Unit",
                 |        "value" : { }
                 |      },
                 |      "transactionId" : {
                 |        "type" : "ByteVector",
                 |        "value" : "${tx.id()}"
                 |      },
                 |      "caller" : {
                 |        "type" : "Address",
                 |        "value" : {
                 |          "bytes" : {
                 |            "type" : "ByteVector",
                 |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
                 |          }
                 |        }
                 |      },
                 |      "fee" : {
                 |        "type" : "Int",
                 |        "value" : 102500000
                 |      }
                 |    }
                 |  }, {
                 |    "name" : "burn.@args",
                 |    "type" : "Array",
                 |    "value" : [ ]
                 |  }, {
                 |    "name" : "Burn.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "ByteVector",
                 |      "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |    }, {
                 |      "type" : "Int",
                 |      "value" : 1
                 |    } ]
                 |  }, {
                 |    "name" : "Burn.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51999
                 |  }, {
                 |    "name" : "cons.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "Burn",
                 |      "value" : {
                 |        "assetId" : {
                 |          "type" : "ByteVector",
                 |          "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
                 |        },
                 |        "quantity" : {
                 |          "type" : "Int",
                 |          "value" : 1
                 |        }
                 |      }
                 |    }, {
                 |      "type" : "Array",
                 |      "value" : [ ]
                 |    } ]
                 |  }, {
                 |    "name" : "cons.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 51998
                 |  } ]
                 |}, {
                 |  "type" : "asset",
                 |  "context" : "burn",
                 |  "id" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
                 |  "result" : "failure",
                 |  "vars" : [ {
                 |    "name" : "test",
                 |    "type" : "Boolean",
                 |    "value" : true
                 |  }, {
                 |    "name" : "throw.@args",
                 |    "type" : "Array",
                 |    "value" : [ {
                 |      "type" : "String",
                 |      "value" : "error"
                 |    } ]
                 |  }, {
                 |    "name" : "throw.@complexity",
                 |    "type" : "Int",
                 |    "value" : 1
                 |  }, {
                 |    "name" : "@complexityLimit",
                 |    "type" : "Int",
                 |    "value" : 2147483646
                 |  } ],
                 |  "error" : "error"
                 |} ]""".stripMargin
      )

    }

    "invoke tx returning leases" in {
      val dAppPk        = TxHelpers.defaultSigner.publicKey
      val dAppAddress   = dAppPk.toAddress
      val invoke        = TxHelpers.invoke(dAppPk.toAddress)
      val leaseCancelId = ByteStr(bytes32gen.sample.get)

      val amount1    = 100
      val recipient1 = Recipient.Address(ByteStr.decodeBase58("3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd").get)
      val nonce1     = 0
      val leaseId1   = Lease.calculateId(Lease(recipient1, amount1, nonce1), invoke.id())

      val amount2    = 20
      val recipient2 = Recipient.Alias("some_alias")
      val nonce2     = 2
      val leaseId2   = Lease.calculateId(Lease(recipient2, amount2, nonce2), invoke.id())

      val leaseCancelAmount = 786

      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(*, *).returns(Long.MaxValue)
        (blockchain.wavesBalances _).when(*).returns(Map(TxHelpers.defaultAddress -> Long.MaxValue))

        (blockchain.resolveAlias _).when(Alias.create(recipient2.name).explicitGet()).returning(Right(TxHelpers.secondAddress))

        blockchain.stub.activateAllFeatures()

        val (dAppScript, _) = ScriptCompiler
          .compile(
            s"""
               |{-# STDLIB_VERSION 5 #-}
               |{-# SCRIPT_TYPE ACCOUNT #-}
               |{-# CONTENT_TYPE DAPP #-}
               |
               |@Callable(i)
               |func default() = {
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
            ScriptEstimatorV3(fixOverflow = true, overhead = true)
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
                Map(3 -> Seq("default", "test1").map(_ -> 0L).toMap)
              )
            )
          )

        (blockchain.hasAccountScript _).when(*).returns(true)

        (blockchain.transactionMeta _)
          .when(leaseCancelId)
          .returns(Some(TxMeta(Height(1), Status.Succeeded, 0L)))
          .anyNumberOfTimes()

        (blockchain.leaseDetails _)
          .when(leaseCancelId)
          .returns(Some(LeaseDetails(dAppPk, TxHelpers.defaultAddress, leaseCancelAmount, LeaseDetails.Status.Active, leaseCancelId, 1)))
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
          priorityPoolBlockchain = () => Some(blockchain)
        )
        .route

      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, invoke.json().toString())) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "stateChanges").as[JsObject] should matchJson(s"""{
                                                                 |  "data" : [ ],
                                                                 |  "transfers" : [ ],
                                                                 |  "issues" : [ ],
                                                                 |  "reissues" : [ ],
                                                                 |  "burns" : [ ],
                                                                 |  "sponsorFees" : [ ],
                                                                 |  "leases" : [ {
                                                                 |    "id" : "$leaseId1",
                                                                 |    "originTransactionId" : "${invoke.id()}",
                                                                 |    "sender" : "$dAppAddress",
                                                                 |    "recipient" : "${recipient1.bytes}",
                                                                 |    "amount" : 100,
                                                                 |    "height" : 1,
                                                                 |    "status" : "active",
                                                                 |    "cancelHeight" : null,
                                                                 |    "cancelTransactionId" : null
                                                                 |  }, {
                                                                 |    "id" : "$leaseId2",
                                                                 |    "originTransactionId" : "${invoke.id()}",
                                                                 |    "sender" : "$dAppAddress",
                                                                 |    "recipient" : "${TxHelpers.secondAddress}",
                                                                 |    "amount" : 20,
                                                                 |    "height" : 1,
                                                                 |    "status" : "active",
                                                                 |    "cancelHeight" : null,
                                                                 |    "cancelTransactionId" : null
                                                                 |  } ],
                                                                 |  "leaseCancels" : [ {
                                                                 |    "id" : "$leaseCancelId",
                                                                 |    "originTransactionId" : "$leaseCancelId",
                                                                 |    "sender" : "$dAppAddress",
                                                                 |    "recipient" : "${TxHelpers.defaultAddress}",
                                                                 |    "amount" : $leaseCancelAmount,
                                                                 |    "height" : 1,
                                                                 |    "status" : "canceled",
                                                                 |    "cancelHeight" : 1,
                                                                 |    "cancelTransactionId" : "${invoke.id()}"
                                                                 |  } ],
                                                                 |  "invokes" : [ ]
                                                                 |}""".stripMargin)
        (json \ "trace").as[JsArray] should matchJson(
          s"""
             |[ {
             |  "type" : "verifier",
             |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |  "result" : "success",
             |  "error" : null
             |}, {
             |  "type" : "dApp",
             |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |  "function" : "default",
             |  "args" : [ ],
             |  "invocations" : [ ],
             |  "result" : {
             |    "data" : [ ],
             |    "transfers" : [ ],
             |    "issues" : [ ],
             |    "reissues" : [ ],
             |    "burns" : [ ],
             |    "sponsorFees" : [ ],
             |    "leases" : [ {
             |      "id" : "$leaseId1",
             |      "originTransactionId" : "${invoke.id()}",
             |      "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |      "recipient" : "3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd",
             |      "amount" : 100,
             |      "height" : 1,
             |      "status" : "active",
             |      "cancelHeight" : null,
             |      "cancelTransactionId" : null
             |    }, {
             |      "id" : "$leaseId2",
             |      "originTransactionId" : "${invoke.id()}",
             |      "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |      "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
             |      "amount" : 20,
             |      "height" : 1,
             |      "status" : "active",
             |      "cancelHeight" : null,
             |      "cancelTransactionId" : null
             |    } ],
             |    "leaseCancels" : [ {
             |      "id" : "$leaseCancelId",
             |      "originTransactionId" : "$leaseCancelId",
             |      "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |      "recipient" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |      "amount" : $leaseCancelAmount,
             |      "height" : 1,
             |      "status" : "canceled",
             |      "cancelHeight" : 1,
             |      "cancelTransactionId" : "${invoke.id()}"
             |    } ],
             |    "invokes" : [ ]
             |  },
             |  "error" : null,
             |  "vars" : [ {
             |    "name" : "i",
             |    "type" : "Invocation",
             |    "value" : {
             |      "originCaller" : {
             |        "type" : "Address",
             |        "value" : {
             |          "bytes" : {
             |            "type" : "ByteVector",
             |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
             |          }
             |        }
             |      },
             |      "payments" : {
             |        "type" : "Array",
             |        "value" : [ ]
             |      },
             |      "callerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
             |      },
             |      "feeAssetId" : {
             |        "type" : "Unit",
             |        "value" : { }
             |      },
             |      "originCallerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
             |      },
             |      "transactionId" : {
             |        "type" : "ByteVector",
             |        "value" : "${invoke.id()}"
             |      },
             |      "caller" : {
             |        "type" : "Address",
             |        "value" : {
             |          "bytes" : {
             |            "type" : "ByteVector",
             |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
             |          }
             |        }
             |      },
             |      "fee" : {
             |        "type" : "Int",
             |        "value" : 500000
             |      }
             |    }
             |  }, {
             |    "name" : "default.@args",
             |    "type" : "Array",
             |    "value" : [ ]
             |  }, {
             |    "name" : "parseBigIntValue.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "String",
             |      "value" : "6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042047"
             |    } ]
             |  }, {
             |    "name" : "parseBigIntValue.@complexity",
             |    "type" : "Int",
             |    "value" : 65
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25935
             |  }, {
             |    "name" : "a",
             |    "type" : "BigInt",
             |    "value" : 6.703903964971298549787012499102923E+153
             |  }, {
             |    "name" : "==.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "BigInt",
             |      "value" : 6.703903964971298549787012499102923E+153
             |    }, {
             |      "type" : "BigInt",
             |      "value" : 6.703903964971298549787012499102923E+153
             |    } ]
             |  }, {
             |    "name" : "==.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25934
             |  }, {
             |    "name" : "test",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "==.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "type" : "Int",
             |      "value" : 1
             |    } ]
             |  }, {
             |    "name" : "==.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25933
             |  }, {
             |    "name" : "Address.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "ByteVector",
             |      "value" : "3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd"
             |    } ]
             |  }, {
             |    "name" : "Address.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25932
             |  }, {
             |    "name" : "Lease.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "Address",
             |      "value" : {
             |        "bytes" : {
             |          "type" : "ByteVector",
             |          "value" : "3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd"
             |        }
             |      }
             |    }, {
             |      "type" : "Int",
             |      "value" : 100
             |    }, {
             |      "type" : "Int",
             |      "value" : 0
             |    } ]
             |  }, {
             |    "name" : "Lease.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25931
             |  }, {
             |    "name" : "Alias.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "String",
             |      "value" : "some_alias"
             |    } ]
             |  }, {
             |    "name" : "Alias.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25930
             |  }, {
             |    "name" : "Lease.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "Alias",
             |      "value" : {
             |        "alias" : {
             |          "type" : "String",
             |          "value" : "some_alias"
             |        }
             |      }
             |    }, {
             |      "type" : "Int",
             |      "value" : 20
             |    }, {
             |      "type" : "Int",
             |      "value" : 2
             |    } ]
             |  }, {
             |    "name" : "Lease.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25929
             |  }, {
             |    "name" : "LeaseCancel.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "ByteVector",
             |      "value" : "$leaseCancelId"
             |    } ]
             |  }, {
             |    "name" : "LeaseCancel.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25928
             |  }, {
             |    "name" : "cons.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "LeaseCancel",
             |      "value" : {
             |        "leaseId" : {
             |          "type" : "ByteVector",
             |          "value" : "$leaseCancelId"
             |        }
             |      }
             |    }, {
             |      "type" : "Array",
             |      "value" : [ ]
             |    } ]
             |  }, {
             |    "name" : "cons.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25927
             |  }, {
             |    "name" : "cons.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "Lease",
             |      "value" : {
             |        "recipient" : {
             |          "type" : "Alias",
             |          "value" : {
             |            "alias" : {
             |              "type" : "String",
             |              "value" : "some_alias"
             |            }
             |          }
             |        },
             |        "amount" : {
             |          "type" : "Int",
             |          "value" : 20
             |        },
             |        "nonce" : {
             |          "type" : "Int",
             |          "value" : 2
             |        }
             |      }
             |    }, {
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "LeaseCancel",
             |        "value" : {
             |          "leaseId" : {
             |            "type" : "ByteVector",
             |            "value" : "$leaseCancelId"
             |          }
             |        }
             |      } ]
             |    } ]
             |  }, {
             |    "name" : "cons.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25926
             |  }, {
             |    "name" : "cons.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "Lease",
             |      "value" : {
             |        "recipient" : {
             |          "type" : "Address",
             |          "value" : {
             |            "bytes" : {
             |              "type" : "ByteVector",
             |              "value" : "3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd"
             |            }
             |          }
             |        },
             |        "amount" : {
             |          "type" : "Int",
             |          "value" : 100
             |        },
             |        "nonce" : {
             |          "type" : "Int",
             |          "value" : 0
             |        }
             |      }
             |    }, {
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "Lease",
             |        "value" : {
             |          "recipient" : {
             |            "type" : "Alias",
             |            "value" : {
             |              "alias" : {
             |                "type" : "String",
             |                "value" : "some_alias"
             |              }
             |            }
             |          },
             |          "amount" : {
             |            "type" : "Int",
             |            "value" : 20
             |          },
             |          "nonce" : {
             |            "type" : "Int",
             |            "value" : 2
             |          }
             |        }
             |      }, {
             |        "type" : "LeaseCancel",
             |        "value" : {
             |          "leaseId" : {
             |            "type" : "ByteVector",
             |            "value" : "$leaseCancelId"
             |          }
             |        }
             |      } ]
             |    } ]
             |  }, {
             |    "name" : "cons.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25925
             |  } ]
             |} ]
             |
          """.stripMargin
        )
        (json \ "height").as[Int] shouldBe 1
      }
    }

    "invoke tx returning leases with error" in {
      val dApp1Kp      = signer(1)
      val dApp2Kp      = signer(2)
      val leaseAddress = signer(3).toAddress
      val amount       = 123

      withDomain(RideV6, AddrWithBalance.enoughBalances(dApp1Kp, dApp2Kp)) { d =>
        val dApp1 = TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func default() = {
             |   strict r = Address(base58'${dApp2Kp.toAddress}').invoke("default", [], [])
             |   if (true) then throw() else []
             | }
           """.stripMargin
        )
        val leaseTx = lease(dApp2Kp, leaseAddress)
        val dApp2 = TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func default() = {
             |   let lease   = Lease(Address(base58'$leaseAddress'), $amount)
             |   let cancel1 = LeaseCancel(calculateLeaseId(lease))
             |   let cancel2 = LeaseCancel(base58'${leaseTx.id()}')
             |   [lease, cancel1, cancel2]
             | }
           """.stripMargin
        )
        d.appendBlock(leaseTx)
        d.appendBlock(setScript(dApp1Kp, dApp1), setScript(dApp2Kp, dApp2))

        val route   = debugApiRoute.copy(blockchain = d.blockchain, priorityPoolBlockchain = () => Some(d.blockchain)).route
        val invoke  = TxHelpers.invoke(dApp1Kp.toAddress)
        val leaseId = Lease.calculateId(Lease(Address(ByteStr(leaseAddress.bytes)), amount, 0), invoke.id())

        Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, invoke.json().toString())) ~> route ~> check {
          val json = responseAs[JsValue]
          json should matchJson(
            s"""
               |{
               |  "valid": false,
               |  "validationTime": ${(json \ "validationTime").as[Int]},
               |  "trace": [
               |    {
               |      "type": "dApp",
               |      "id": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
               |      "function": "default",
               |      "args": [],
               |      "invocations": [
               |        {
               |          "type": "dApp",
               |          "id": "3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy",
               |          "function": "default",
               |          "args": [],
               |          "invocations": [],
               |          "result": {
               |            "data": [],
               |            "transfers": [],
               |            "issues": [],
               |            "reissues": [],
               |            "burns": [],
               |            "sponsorFees": [],
               |            "leases": [
               |              {
               |                "id": "$leaseId",
               |                "originTransactionId": null,
               |                "sender": null,
               |                "recipient": "$leaseAddress",
               |                "amount": $amount,
               |                "height": null,
               |                "status": "canceled",
               |                "cancelHeight": null,
               |                "cancelTransactionId": null
               |              }
               |            ],
               |            "leaseCancels": [
               |              {
               |                "id": "$leaseId",
               |                "originTransactionId": null,
               |                "sender": null,
               |                "recipient": null,
               |                "amount": null,
               |                "height": null,
               |                "status": "canceled",
               |                "cancelHeight": null,
               |                "cancelTransactionId": null
               |              }, {
               |                "id" : "${leaseTx.id()}",
               |                "originTransactionId" : "${leaseTx.id()}",
               |                "sender" : "${dApp2Kp.toAddress}",
               |                "recipient" : "$leaseAddress",
               |                "amount" : ${leaseTx.amount},
               |                "height" : 2,
               |                "status" : "active",
               |                "cancelHeight" : null,
               |                "cancelTransactionId" : null
               |              }
               |
               |            ],
               |            "invokes": []
               |          },
               |          "error": null,
               |          "vars": [
               |            {
               |              "name": "i",
               |              "type": "Invocation",
               |              "value": {
               |                "originCaller": {
               |                  "type": "Address",
               |                  "value": {
               |                    "bytes": {
               |                      "type": "ByteVector",
               |                      "value": "$defaultAddress"
               |                    }
               |                  }
               |                },
               |                "payments": {
               |                  "type": "Array",
               |                  "value": []
               |                },
               |                "callerPublicKey": {
               |                  "type": "ByteVector",
               |                  "value": "8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr"
               |                },
               |                "feeAssetId": {
               |                  "type": "Unit",
               |                  "value": {}
               |                },
               |                "originCallerPublicKey": {
               |                  "type": "ByteVector",
               |                  "value": "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
               |                },
               |                "transactionId": {
               |                  "type": "ByteVector",
               |                  "value": "${invoke.id()}"
               |                },
               |                "caller": {
               |                  "type": "Address",
               |                  "value": {
               |                    "bytes": {
               |                      "type": "ByteVector",
               |                      "value": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
               |                    }
               |                  }
               |                },
               |                "fee": {
               |                  "type": "Int",
               |                  "value": 500000
               |                }
               |              }
               |            },
               |            {
               |              "name": "default.@args",
               |              "type": "Array",
               |              "value": []
               |            },
               |            {
               |              "name": "Address.@args",
               |              "type": "Array",
               |              "value": [
               |                {
               |                  "type": "ByteVector",
               |                  "value": "$leaseAddress"
               |                }
               |              ]
               |            },
               |            {
               |              "name": "Address.@complexity",
               |              "type": "Int",
               |              "value": 1
               |            },
               |            {
               |              "name": "@complexityLimit",
               |              "type": "Int",
               |              "value": 51923
               |            },
               |            {
               |              "name": "Lease.@args",
               |              "type": "Array",
               |              "value": [
               |                {
               |                  "type": "Address",
               |                  "value": {
               |                    "bytes": {
               |                      "type": "ByteVector",
               |                      "value": "$leaseAddress"
               |                    }
               |                  }
               |                },
               |                {
               |                  "type": "Int",
               |                  "value": $amount
               |                }
               |              ]
               |            },
               |            {
               |              "name": "Lease.@complexity",
               |              "type": "Int",
               |              "value": 1
               |            },
               |            {
               |              "name": "@complexityLimit",
               |              "type": "Int",
               |              "value": 51922
               |            },
               |            {
               |              "name": "lease",
               |              "type": "Lease",
               |              "value": {
               |                "recipient": {
               |                  "type": "Address",
               |                  "value": {
               |                    "bytes": {
               |                      "type": "ByteVector",
               |                      "value": "$leaseAddress"
               |                    }
               |                  }
               |                },
               |                "amount": {
               |                  "type": "Int",
               |                  "value": $amount
               |                },
               |                "nonce": {
               |                  "type": "Int",
               |                  "value": 0
               |                }
               |              }
               |            },
               |            {
               |              "name": "calculateLeaseId.@args",
               |              "type": "Array",
               |              "value": [
               |                {
               |                  "type": "Lease",
               |                  "value": {
               |                    "recipient": {
               |                      "type": "Address",
               |                      "value": {
               |                        "bytes": {
               |                          "type": "ByteVector",
               |                          "value": "$leaseAddress"
               |                        }
               |                      }
               |                    },
               |                    "amount": {
               |                      "type": "Int",
               |                      "value": $amount
               |                    },
               |                    "nonce": {
               |                      "type": "Int",
               |                      "value": 0
               |                    }
               |                  }
               |                }
               |              ]
               |            },
               |            {
               |              "name": "calculateLeaseId.@complexity",
               |              "type": "Int",
               |              "value": 1
               |            },
               |            {
               |              "name": "@complexityLimit",
               |              "type": "Int",
               |              "value": 51921
               |            },
               |            {
               |              "name": "LeaseCancel.@args",
               |              "type": "Array",
               |              "value": [
               |                {
               |                  "type": "ByteVector",
               |                  "value": "$leaseId"
               |                }
               |              ]
               |            },
               |            {
               |              "name": "cancel1",
               |              "type": "LeaseCancel",
               |              "value": {
               |                "leaseId": {
               |                  "type": "ByteVector",
               |                  "value": "$leaseId"
               |                }
               |              }
               |            },
               |            {
               |              "name": "LeaseCancel.@complexity",
               |              "type": "Int",
               |              "value": 1
               |            },
               |            {
               |              "name": "@complexityLimit",
               |              "type": "Int",
               |              "value": 51920
               |            },
               |            {
               |              "name" : "LeaseCancel.@args",
               |              "type" : "Array",
               |              "value" : [ {
               |                "type" : "ByteVector",
               |                "value" : "${leaseTx.id()}"
               |              } ]
               |            }, {
               |              "name" : "cancel2",
               |              "type" : "LeaseCancel",
               |              "value" : {
               |                "leaseId" : {
               |                  "type" : "ByteVector",
               |                  "value" : "${leaseTx.id()}"
               |                }
               |              }
               |            }, {
               |              "name" : "LeaseCancel.@complexity",
               |              "type" : "Int",
               |              "value" : 1
               |            }, {
               |              "name" : "@complexityLimit",
               |              "type" : "Int",
               |              "value" : 51919
               |            },
               |            {
               |              "name": "cons.@args",
               |              "type": "Array",
               |              "value": [
               |                {
               |                  "type": "LeaseCancel",
               |                  "value": {
               |                    "leaseId": {
               |                      "type": "ByteVector",
               |                      "value": "${leaseTx.id()}"
               |                    }
               |                  }
               |                },
               |                {
               |                  "type": "Array",
               |                  "value": []
               |                }
               |              ]
               |            },
               |            {
               |              "name": "cons.@complexity",
               |              "type": "Int",
               |              "value": 1
               |            },
               |            {
               |              "name": "@complexityLimit",
               |              "type": "Int",
               |              "value": 51918
               |            }, {
               |              "name" : "cons.@args",
               |              "type" : "Array",
               |              "value" : [ {
               |                "type" : "LeaseCancel",
               |                "value" : {
               |                  "leaseId" : {
               |                    "type" : "ByteVector",
               |                    "value" : "$leaseId"
               |                  }
               |                }
               |              }, {
               |                "type" : "Array",
               |                "value" : [ {
               |                  "type" : "LeaseCancel",
               |                  "value" : {
               |                    "leaseId" : {
               |                      "type" : "ByteVector",
               |                      "value" : "${leaseTx.id()}"
               |                    }
               |                  }
               |                } ]
               |              } ]
               |            }, {
               |              "name" : "cons.@complexity",
               |              "type" : "Int",
               |              "value" : 1
               |            }, {
               |              "name" : "@complexityLimit",
               |              "type" : "Int",
               |              "value" : 51917
               |            }, {
               |              "name": "cons.@args",
               |              "type": "Array",
               |              "value": [
               |                {
               |                  "type": "Lease",
               |                  "value": {
               |                    "recipient": {
               |                      "type": "Address",
               |                      "value": {
               |                        "bytes": {
               |                          "type": "ByteVector",
               |                          "value": "$leaseAddress"
               |                        }
               |                      }
               |                    },
               |                    "amount": {
               |                      "type": "Int",
               |                      "value": $amount
               |                    },
               |                    "nonce": {
               |                      "type": "Int",
               |                      "value": 0
               |                    }
               |                  }
               |           }, {
               |             "type" : "Array",
               |             "value" : [ {
               |               "type" : "LeaseCancel",
               |               "value" : {
               |                 "leaseId" : {
               |                   "type" : "ByteVector",
               |                   "value" : "$leaseId"
               |                 }
               |               }
               |             }, {
               |               "type" : "LeaseCancel",
               |               "value" : {
               |                 "leaseId" : {
               |                   "type" : "ByteVector",
               |                   "value" : "${leaseTx.id()}"
               |                 }
               |               }
               |             } ]
               |           } ]
               |         }, {
               |              "name": "cons.@complexity",
               |              "type": "Int",
               |              "value": 1
               |            },
               |            {
               |              "name": "@complexityLimit",
               |              "type": "Int",
               |              "value": 51916
               |            }
               |          ]
               |        }
               |      ],
               |      "result": "failure",
               |      "error": "InvokeRejectError(error = Explicit script termination)",
               |      "vars" : [ {
               |        "name" : "i",
               |        "type" : "Invocation",
               |        "value" : {
               |          "originCaller" : {
               |            "type" : "Address",
               |            "value" : {
               |              "bytes" : {
               |                "type" : "ByteVector",
               |                "value" : "$defaultAddress"
               |              }
               |            }
               |          },
               |          "payments" : {
               |            "type" : "Array",
               |            "value" : [ ]
               |          },
               |          "callerPublicKey" : {
               |            "type" : "ByteVector",
               |            "value" : "${defaultSigner.publicKey}"
               |          },
               |          "feeAssetId" : {
               |            "type" : "Unit",
               |            "value" : { }
               |          },
               |          "originCallerPublicKey" : {
               |            "type" : "ByteVector",
               |            "value" : "${defaultSigner.publicKey}"
               |          },
               |          "transactionId" : {
               |            "type" : "ByteVector",
               |            "value" : "${invoke.id()}"
               |          },
               |          "caller" : {
               |            "type" : "Address",
               |            "value" : {
               |              "bytes" : {
               |                "type" : "ByteVector",
               |                "value" : "$defaultAddress"
               |              }
               |            }
               |          },
               |          "fee" : {
               |            "type" : "Int",
               |            "value" : 500000
               |          }
               |        }
               |      }, {
               |        "name" : "default.@args",
               |        "type" : "Array",
               |        "value" : [ ]
               |      }, {
               |        "name" : "Address.@args",
               |        "type" : "Array",
               |        "value" : [ {
               |          "type" : "ByteVector",
               |          "value" : "3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy"
               |        } ]
               |      }, {
               |        "name" : "Address.@complexity",
               |        "type" : "Int",
               |        "value" : 1
               |      }, {
               |        "name" : "@complexityLimit",
               |        "type" : "Int",
               |        "value" : 51999
               |      }, {
               |        "name" : "invoke.@args",
               |        "type" : "Array",
               |        "value" : [ {
               |          "type" : "Address",
               |          "value" : {
               |            "bytes" : {
               |              "type" : "ByteVector",
               |              "value" : "3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy"
               |            }
               |          }
               |        }, {
               |          "type" : "String",
               |          "value" : "default"
               |        }, {
               |          "type" : "Array",
               |          "value" : [ ]
               |        }, {
               |          "type" : "Array",
               |          "value" : [ ]
               |        } ]
               |      }, {
               |        "name" : "invoke.@complexity",
               |        "type" : "Int",
               |        "value" : 75
               |      }, {
               |        "name" : "@complexityLimit",
               |        "type" : "Int",
               |        "value" : 51924
               |      }, {
               |        "name" : "default.@complexity",
               |        "type" : "Int",
               |        "value" : 8
               |      }, {
               |        "name" : "@complexityLimit",
               |        "type" : "Int",
               |        "value" : 51916
               |      }, {
               |        "name" : "r",
               |        "type" : "Unit",
               |        "value" : { }
               |      }, {
               |        "name" : "==.@args",
               |        "type" : "Array",
               |        "value" : [ {
               |          "type" : "Unit",
               |          "value" : { }
               |        }, {
               |          "type" : "Unit",
               |          "value" : { }
               |        } ]
               |      }, {
               |        "name" : "==.@complexity",
               |        "type" : "Int",
               |        "value" : 1
               |      }, {
               |        "name" : "@complexityLimit",
               |        "type" : "Int",
               |        "value" : 51915
               |      }, {
               |        "name" : "throw.@args",
               |        "type" : "Array",
               |        "value" : [ ]
               |      }, {
               |        "name" : "throw.@complexity",
               |        "type" : "Int",
               |        "value" : 1
               |      }, {
               |        "name" : "@complexityLimit",
               |        "type" : "Int",
               |        "value" : 51914
               |      }, {
               |        "name" : "throw.@args",
               |        "type" : "Array",
               |        "value" : [ {
               |          "type" : "String",
               |          "value" : "Explicit script termination"
               |        } ]
               |      }, {
               |        "name" : "throw.@complexity",
               |        "type" : "Int",
               |        "value" : 1
               |      }, {
               |        "name" : "@complexityLimit",
               |        "type" : "Int",
               |        "value" : 51913
               |      } ]
               |    }
               |  ],
               |  "height": 3,
               |  "error": "Error while executing dApp: Explicit script termination",
               |  "transaction": {
               |    "type": 16,
               |    "id": "${invoke.id()}",
               |    "fee": 500000,
               |    "feeAssetId": null,
               |    "timestamp": ${(json \ "transaction" \ "timestamp").as[Long]},
               |    "version": 2,
               |    "chainId": 84,
               |    "sender": "$defaultAddress",
               |    "senderPublicKey": "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
               |    "proofs": [ "${(json \ "transaction" \ "proofs" \ 0).as[String]}" ],
               |    "dApp": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
               |    "payment": [],
               |    "call": {
               |      "function": "default",
               |      "args": []
               |    }
               |  }
               |}
             """.stripMargin
          )
        }
      }
    }

    "invoke tx with nested call" in {
      val dAppPk      = TxHelpers.defaultSigner.publicKey
      val dAppAddress = dAppPk.toAddress
      val invoke      = TxHelpers.invoke(dAppPk.toAddress, func = Some("test1"))

      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(*, *).returns(Long.MaxValue)
        (blockchain.wavesBalances _).when(*).returns(Map(TxHelpers.defaultAddress -> Long.MaxValue))
        blockchain.stub.activateAllFeatures()

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
               |    then [IntegerEntry("key", 1)]
               |    else []
               |}
               |
               |@Callable(i)
               |func test1() = {
               |  strict result = reentrantInvoke(this, "test", [], [])
               |  if (result == unit) then [] else []
               |}
               |""".stripMargin,
            ScriptEstimatorV3(fixOverflow = true, overhead = true)
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
                Map(3 -> Seq("test", "test1").map(_ -> 0L).toMap)
              )
            )
          )

        (blockchain.hasAccountScript _).when(dAppAddress).returns(true)
      }
      val route = debugApiRoute
        .copy(
          blockchain = blockchain,
          priorityPoolBlockchain = () => Some(blockchain)
        )
        .route

      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, invoke.json().toString())) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "stateChanges").as[JsObject] should matchJson(s"""{
                                                                 |  "data" : [ ],
                                                                 |  "transfers" : [ ],
                                                                 |  "issues" : [ ],
                                                                 |  "reissues" : [ ],
                                                                 |  "burns" : [ ],
                                                                 |  "sponsorFees" : [ ],
                                                                 |  "leases" : [ ],
                                                                 |  "leaseCancels" : [ ],
                                                                 |  "invokes" : [ {
                                                                 |    "dApp" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                                                 |    "call" : {
                                                                 |      "function" : "test",
                                                                 |      "args" : [ ]
                                                                 |    },
                                                                 |    "payment" : [ ],
                                                                 |    "stateChanges" : {
                                                                 |      "data" : [ {
                                                                 |        "key" : "key",
                                                                 |        "type" : "integer",
                                                                 |        "value" : 1
                                                                 |      } ],
                                                                 |      "transfers" : [ ],
                                                                 |      "issues" : [ ],
                                                                 |      "reissues" : [ ],
                                                                 |      "burns" : [ ],
                                                                 |      "sponsorFees" : [ ],
                                                                 |      "leases" : [ ],
                                                                 |      "leaseCancels" : [ ],
                                                                 |      "invokes" : [ ]
                                                                 |    }
                                                                 |  } ]
                                                                 |}
                                                                 |""".stripMargin)
        (json \ "trace").as[JsArray] should matchJson(
          s"""
             |[ {
             |  "type" : "verifier",
             |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |  "result" : "success",
             |  "error" : null
             |}, {
             |  "type" : "dApp",
             |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |  "function" : "test1",
             |  "args" : [ ],
             |  "invocations" : [ {
             |    "type" : "dApp",
             |    "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |    "function" : "test",
             |    "args" : [ ],
             |    "invocations" : [ ],
             |    "result" : {
             |      "data" : [ {
             |        "key" : "key",
             |        "type" : "integer",
             |        "value" : 1
             |      } ],
             |      "transfers" : [ ],
             |      "issues" : [ ],
             |      "reissues" : [ ],
             |      "burns" : [ ],
             |      "sponsorFees" : [ ],
             |      "leases" : [ ],
             |      "leaseCancels" : [ ],
             |      "invokes" : [ ]
             |    },
             |    "error" : null,
             |    "vars" : [ {
             |      "name" : "i",
             |      "type" : "Invocation",
             |      "value" : {
             |        "originCaller" : {
             |          "type" : "Address",
             |          "value" : {
             |            "bytes" : {
             |              "type" : "ByteVector",
             |              "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
             |            }
             |          }
             |        },
             |        "payments" : {
             |          "type" : "Array",
             |          "value" : [ ]
             |        },
             |        "callerPublicKey" : {
             |          "type" : "ByteVector",
             |          "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
             |        },
             |        "feeAssetId" : {
             |          "type" : "Unit",
             |          "value" : { }
             |        },
             |        "originCallerPublicKey" : {
             |          "type" : "ByteVector",
             |          "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
             |        },
             |        "transactionId" : {
             |          "type" : "ByteVector",
             |          "value" : "${invoke.id()}"
             |        },
             |        "caller" : {
             |          "type" : "Address",
             |          "value" : {
             |            "bytes" : {
             |              "type" : "ByteVector",
             |              "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
             |            }
             |          }
             |        },
             |        "fee" : {
             |          "type" : "Int",
             |          "value" : 500000
             |        }
             |      }
             |    }, {
             |      "name" : "test.@args",
             |      "type" : "Array",
             |      "value" : [ ]
             |    }, {
             |      "name" : "parseBigIntValue.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "String",
             |        "value" : "6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042047"
             |      } ]
             |    }, {
             |      "name" : "parseBigIntValue.@complexity",
             |      "type" : "Int",
             |      "value" : 65
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25860
             |    }, {
             |      "name" : "a",
             |      "type" : "BigInt",
             |      "value" : 6.703903964971298549787012499102923E+153
             |    }, {
             |      "name" : "==.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "BigInt",
             |        "value" : 6.703903964971298549787012499102923E+153
             |      }, {
             |        "type" : "BigInt",
             |        "value" : 6.703903964971298549787012499102923E+153
             |      } ]
             |    }, {
             |      "name" : "==.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25859
             |    }, {
             |      "name" : "test",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "==.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "Int",
             |        "value" : 1
             |      }, {
             |        "type" : "Int",
             |        "value" : 1
             |      } ]
             |    }, {
             |      "name" : "==.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25858
             |    }, {
             |      "name" : "IntegerEntry.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "String",
             |        "value" : "key"
             |      }, {
             |        "type" : "Int",
             |        "value" : 1
             |      } ]
             |    }, {
             |      "name" : "IntegerEntry.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25857
             |    }, {
             |      "name" : "cons.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "IntegerEntry",
             |        "value" : {
             |          "key" : {
             |            "type" : "String",
             |            "value" : "key"
             |          },
             |          "value" : {
             |            "type" : "Int",
             |            "value" : 1
             |          }
             |        }
             |      }, {
             |        "type" : "Array",
             |        "value" : [ ]
             |      } ]
             |    }, {
             |      "name" : "cons.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25856
             |    } ]
             |  } ],
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
             |  "vars" : [ {
             |    "name" : "i",
             |    "type" : "Invocation",
             |    "value" : {
             |      "originCaller" : {
             |        "type" : "Address",
             |        "value" : {
             |          "bytes" : {
             |            "type" : "ByteVector",
             |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
             |          }
             |        }
             |      },
             |      "payments" : {
             |        "type" : "Array",
             |        "value" : [ ]
             |      },
             |      "callerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
             |      },
             |      "feeAssetId" : {
             |        "type" : "Unit",
             |        "value" : { }
             |      },
             |      "originCallerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
             |      },
             |      "transactionId" : {
             |        "type" : "ByteVector",
             |        "value" : "${invoke.id()}"
             |      },
             |      "caller" : {
             |        "type" : "Address",
             |        "value" : {
             |          "bytes" : {
             |            "type" : "ByteVector",
             |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
             |          }
             |        }
             |      },
             |      "fee" : {
             |        "type" : "Int",
             |        "value" : 500000
             |      }
             |    }
             |  }, {
             |    "name" : "test1.@args",
             |    "type" : "Array",
             |    "value" : [ ]
             |  }, {
             |    "name" : "reentrantInvoke.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "Address",
             |      "value" : {
             |        "bytes" : {
             |          "type" : "ByteVector",
             |          "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
             |        }
             |      }
             |    }, {
             |      "type" : "String",
             |      "value" : "test"
             |    }, {
             |      "type" : "Array",
             |      "value" : [ ]
             |    }, {
             |      "type" : "Array",
             |      "value" : [ ]
             |    } ]
             |  }, {
             |    "name" : "reentrantInvoke.@complexity",
             |    "type" : "Int",
             |    "value" : 75
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25925
             |  }, {
             |    "name" : "test.@complexity",
             |    "type" : "Int",
             |    "value" : 69
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25856
             |  }, {
             |    "name" : "result",
             |    "type" : "Unit",
             |    "value" : { }
             |  }, {
             |    "name" : "==.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "Unit",
             |      "value" : { }
             |    }, {
             |      "type" : "Unit",
             |      "value" : { }
             |    } ]
             |  }, {
             |    "name" : "==.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25855
             |  }, {
             |    "name" : "==.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "Unit",
             |      "value" : { }
             |    }, {
             |      "type" : "Unit",
             |      "value" : { }
             |    } ]
             |  }, {
             |    "name" : "==.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 25854
             |  } ]
             |} ]
          """.stripMargin
        )
        (json \ "height").as[Int] shouldBe 1
      }
    }

    "transfer transaction with asset fail" in {
      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.balance _).when(*, *).returns(Long.MaxValue / 2)
        (blockchain.wavesBalances _).when(*).returns(Map(TxHelpers.defaultAddress -> Long.MaxValue / 2))

        val (assetScript, assetScriptComplexity) =
          ScriptCompiler.compile("false", ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()
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
                nft = false,
                0,
                Height(1)
              )
            )
          )
      }
      val route = routeWithBlockchain(blockchain)
      val tx    = TxHelpers.transfer(TxHelpers.defaultSigner, TxHelpers.defaultAddress, 1, TestValues.asset)

      jsonPost(routePath("/validate"), tx.json()) ~> route ~> check {
        val json = responseAs[JsObject]
        (json \ "trace").as[JsArray] should matchJson("""[ {
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
          featureCheckBlocksPeriod = 1,
          blocksForFeatureActivation = 1,
          preActivatedFeatures = Map(
            BlockchainFeatures.SmartAccounts.id    -> 0,
            BlockchainFeatures.SmartAssets.id      -> 0,
            BlockchainFeatures.Ride4DApps.id       -> 0,
            BlockchainFeatures.FeeSponsorship.id   -> 0,
            BlockchainFeatures.DataTransaction.id  -> 0,
            BlockchainFeatures.BlockReward.id      -> 0,
            BlockchainFeatures.BlockV5.id          -> 0,
            BlockchainFeatures.SynchronousCalls.id -> 0
          )
        )
        (() => blockchain.settings).when().returns(WavesSettings.default().blockchainSettings.copy(functionalitySettings = settings))
        (() => blockchain.activatedFeatures).when().returns(settings.preActivatedFeatures)
        (blockchain.balance _).when(*, *).returns(ENOUGH_AMT)
        (blockchain.wavesBalances _)
          .when(*)
          .returns(Map(TxHelpers.defaultAddress -> ENOUGH_AMT, TxHelpers.secondAddress -> ENOUGH_AMT, TxHelpers.address(3) -> ENOUGH_AMT))

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
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
      }

      val tx2 = TxHelpers.transfer(TxHelpers.secondSigner, TestValues.address, 1.waves, fee = transferFee, version = TxVersion.V2)
      validatePost(tx2) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "error").as[String] should include("Requires 400000 extra fee")
      }

      val tx3 = TxHelpers.transfer(TxHelpers.signer(3), TestValues.address, 1.waves, fee = transferFee, version = TxVersion.V2)
      validatePost(tx3) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
      }
    }

    "InvokeExpression" in {
      def assert(wavesSettings: WavesSettings): Assertion = {
        val blockchain = createBlockchainStub { blockchain =>
          val settings = wavesSettings.blockchainSettings.functionalitySettings
          (() => blockchain.settings).when().returns(WavesSettings.default().blockchainSettings.copy(functionalitySettings = settings))
          (() => blockchain.activatedFeatures).when().returns(settings.preActivatedFeatures)
          (blockchain.balance _).when(*, *).returns(ENOUGH_AMT)
          (blockchain.accountScript _).when(*).returns(None)
          (blockchain.assetScript _).when(*).returns(None)
          (blockchain.assetDescription _).when(TestValues.asset).returns(Some(TestValues.assetDescription))
        }
        val route = routeWithBlockchain(blockchain)

        val expression = TestCompiler(V6).compileFreeCall(
          s"""
             | let assetId = base58'${TestValues.asset}'
             | [ Reissue(assetId, 1, true) ]
         """.stripMargin
        )
        val invokeExpression = TxHelpers.invokeExpression(expression)
        jsonPost(routePath("/validate"), invokeExpression.json()) ~> route ~> check {
          val json = responseAs[JsValue]
          (json \ "expression").as[String] shouldBe expression.bytes.value().base64
          (json \ "valid").as[Boolean] shouldBe true
          (json \ "trace").as[JsArray] should matchJson(
            s"""
               |[ {
               |  "type" : "dApp",
               |  "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
               |  "function" : "default",
               |  "args" : [ ],
               |  "invocations" : [ ],
               |  "result" : {
               |    "data" : [ ],
               |    "transfers" : [ ],
               |    "issues" : [ ],
               |    "reissues" : [ {
               |      "assetId" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx",
               |      "isReissuable" : true,
               |      "quantity" : 1
               |    } ],
               |    "burns" : [ ],
               |    "sponsorFees" : [ ],
               |    "leases" : [ ],
               |    "leaseCancels" : [ ],
               |    "invokes" : [ ]
               |  },
               |  "error" : null,
               |  "vars" : [ {
               |    "name" : "i",
               |    "type" : "Invocation",
               |    "value" : {
               |      "originCaller" : {
               |        "type" : "Address",
               |        "value" : {
               |          "bytes" : {
               |            "type" : "ByteVector",
               |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
               |          }
               |        }
               |      },
               |      "payments" : {
               |        "type" : "Array",
               |        "value" : [ ]
               |      },
               |      "callerPublicKey" : {
               |        "type" : "ByteVector",
               |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
               |      },
               |      "feeAssetId" : {
               |        "type" : "Unit",
               |        "value" : { }
               |      },
               |      "originCallerPublicKey" : {
               |        "type" : "ByteVector",
               |        "value" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ"
               |      },
               |      "transactionId" : {
               |        "type" : "ByteVector",
               |        "value" : "${invokeExpression.id()}"
               |      },
               |      "caller" : {
               |        "type" : "Address",
               |        "value" : {
               |          "bytes" : {
               |            "type" : "ByteVector",
               |            "value" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
               |          }
               |        }
               |      },
               |      "fee" : {
               |        "type" : "Int",
               |        "value" : 1000000
               |      }
               |    }
               |  }, {
               |    "name" : "default.@args",
               |    "type" : "Array",
               |    "value" : [ ]
               |  }, {
               |    "name" : "assetId",
               |    "type" : "ByteVector",
               |    "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
               |  }, {
               |    "name" : "Reissue.@args",
               |    "type" : "Array",
               |    "value" : [ {
               |      "type" : "ByteVector",
               |      "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
               |    }, {
               |      "type" : "Int",
               |      "value" : 1
               |    }, {
               |      "type" : "Boolean",
               |      "value" : true
               |    } ]
               |  }, {
               |    "name" : "Reissue.@complexity",
               |    "type" : "Int",
               |    "value" : 1
               |  }, {
               |    "name" : "@complexityLimit",
               |    "type" : "Int",
               |    "value" : 51999
               |  }, {
               |    "name" : "cons.@args",
               |    "type" : "Array",
               |    "value" : [ {
               |      "type" : "Reissue",
               |      "value" : {
               |        "assetId" : {
               |          "type" : "ByteVector",
               |          "value" : "5PjDJaGfSPJj4tFzMRCiuuAasKg5n8dJKXKenhuwZexx"
               |        },
               |        "quantity" : {
               |          "type" : "Int",
               |          "value" : 1
               |        },
               |        "isReissuable" : {
               |          "type" : "Boolean",
               |          "value" : true
               |        }
               |      }
               |    }, {
               |      "type" : "Array",
               |      "value" : [ ]
               |    } ]
               |  }, {
               |    "name" : "cons.@complexity",
               |    "type" : "Int",
               |    "value" : 1
               |  }, {
               |    "name" : "@complexityLimit",
               |    "type" : "Int",
               |    "value" : 51998
               |  } ]
               |} ]
               |
          """.stripMargin
          )
        }
      }

      assert(ContinuationTransaction)
      intercept[Exception](assert(RideV6)).getMessage should include(
        s"${BlockchainFeatures.ContinuationTransaction.description} feature has not been activated yet"
      )
    }
  }

  routePath("/minerInfo") - {
    "returns info from wallet if miner private keys not specified in config" in {
      val acc = wallet.generateNewAccount()

      acc shouldBe defined
      Get(routePath("/minerInfo")) ~> ApiKeyHeader ~> route ~> check {
        responseAs[Seq[AccountMiningInfo]].map(_.address) shouldBe acc.toSeq.map(_.toAddress.toString)
      }
    }

    "returns info only for miner private keys from config when specified" in {
      val minerAccs   = Seq(TxHelpers.signer(1), TxHelpers.signer(2))
      val minerConfig = debugApiRoute.ws.minerSettings.copy(privateKeys = minerAccs.map(_.privateKey))
      val debugRoute  = debugApiRoute.copy(ws = debugApiRoute.ws.copy(minerSettings = minerConfig))

      Get(routePath("/minerInfo")) ~> ApiKeyHeader ~> debugRoute.route ~> check {
        responseAs[Seq[AccountMiningInfo]].map(_.address) shouldBe minerAccs.map(_.toAddress.toString)
      }
    }
  }

  private def routeWithBlockchain(blockchain: Blockchain & NG) =
    debugApiRoute.copy(blockchain = blockchain, priorityPoolBlockchain = () => Some(blockchain)).route

  private def routeWithBlockchain(d: Domain) =
    debugApiRoute
      .copy(
        blockchain = d.blockchain,
        priorityPoolBlockchain = () => Some(d.blockchain),
        loadBalanceHistory = d.rocksDBWriter.loadBalanceHistory,
        loadStateHash = d.rocksDBWriter.loadStateHash
      )
      .route

  private[this] def jsonPost(path: String, json: JsValue) = {
    Post(path, HttpEntity(ContentTypes.`application/json`, json.toString()))
  }
}
