package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import com.typesafe.config.ConfigObject
import com.wavesplatform.*
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.api.http.DebugApiRoute.AccountMiningInfo
import com.wavesplatform.api.http.{DebugApiRoute, RouteTimeout}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{V4, V5, V6}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease, Recipient}
import com.wavesplatform.mining.{Miner, MinerDebugInfo}
import com.wavesplatform.network.PeerDatabase
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{Blockchain, StateHash}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.SharedSchedulerMixin
import monix.eval.Task
import org.scalatest.OptionValues
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.duration.*
import scala.util.Random

//noinspection ScalaStyle
class DebugApiRouteSpec
    extends RouteSpec("/debug")
    with RestAPISettingsHelper
    with TestWallet
    with NTPTime
    with SharedDomain
    with OptionValues
    with SharedSchedulerMixin {

  override def settings: WavesSettings = DomainPresets.ContinuationTransaction.copy(
    dbSettings = DomainPresets.ContinuationTransaction.dbSettings.copy(storeStateHashes = true),
    restAPISettings = restAPISettings
  )
  private val configObject: ConfigObject = settings.config.root()

  private val richAccount                            = TxHelpers.signer(905)
  override def genesisBalances: Seq[AddrWithBalance] = Seq(AddrWithBalance(richAccount.toAddress, 50_000.waves))

  val miner: Miner & MinerDebugInfo = new Miner with MinerDebugInfo {
    override def scheduleMining(blockchain: Option[Blockchain]): Unit                           = ()
    override def getNextBlockGenerationOffset(account: KeyPair): Either[String, FiniteDuration] = Right(FiniteDuration(0, TimeUnit.SECONDS))
    override def state: MinerDebugInfo.State                                                    = MinerDebugInfo.Disabled
  }

  val block: Block = TestBlock.create(Nil).block
  val testStateHash: StateHash = {
    import com.wavesplatform.utils.byteStrOrdering
    def randomHash: ByteStr = ByteStr(Array.fill(32)(Random.nextInt(256).toByte))
    val hashes              = SectionId.values.map((_, randomHash)).toMap
    StateHash(randomHash, hashes)
  }

  val debugApiRoute: DebugApiRoute =
    DebugApiRoute(
      settings,
      ntpTime,
      domain.blockchain,
      domain.wallet,
      domain.accountsApi,
      domain.transactionsApi,
      domain.assetsApi,
      PeerDatabase.NoOp,
      new ConcurrentHashMap(),
      (_, _) => Task.raiseError(new NotImplementedError("")),
      domain.utxPool,
      miner,
      null,
      null,
      null,
      null,
      configObject,
      domain.rocksDBWriter.loadBalanceHistory,
      domain.rocksDBWriter.loadStateHash,
      () => Some(domain.blockchain),
      new RouteTimeout(60.seconds)(sharedScheduler),
      sharedScheduler
    )

  private val route = seal(debugApiRoute.route)

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }

  routePath("/balances/history/{address}") - {
    val acc1 = TxHelpers.signer(1001)
    val acc2 = TxHelpers.signer(1002)

    val initBalance = 10.waves

    "works" in {
      val tx1 = TxHelpers.transfer(acc2, acc1.toAddress, 1.waves)
      val tx2 = TxHelpers.transfer(acc1, acc2.toAddress, 3.waves)
      val tx3 = TxHelpers.transfer(acc2, acc1.toAddress, 4.waves)
      val tx4 = TxHelpers.transfer(acc1, acc2.toAddress, 5.waves)

      domain.appendBlock(
        TxHelpers.massTransfer(
          richAccount,
          Seq(
            acc1.toAddress -> initBalance,
            acc2.toAddress -> initBalance
          ),
          fee = 0.002.waves
        )
      )
      val initialHeight = domain.blockchain.height
      domain.appendBlock(tx1)
      domain.appendBlock(tx2)
      domain.appendBlock()
      domain.appendBlock(tx3)
      domain.appendBlock(tx4)
      domain.appendBlock()

      val expectedBalance2 = initBalance - tx1.fee.value - tx1.amount.value
      val expectedBalance3 = expectedBalance2 + tx2.amount.value
      val expectedBalance5 = expectedBalance3 - tx3.fee.value - tx3.amount.value
      val expectedBalance6 = expectedBalance5 + tx4.amount.value

      Get(routePath(s"/balances/history/${acc2.toAddress}")) ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[JsArray] shouldBe Json.toJson(
          Seq(
            initialHeight + 5 -> expectedBalance6,
            initialHeight + 4 -> expectedBalance5,
            initialHeight + 2 -> expectedBalance3,
            initialHeight + 1 -> expectedBalance2,
            initialHeight + 0 -> initBalance
          ).map { case (height, balance) =>
            Json.obj("height" -> height, "balance" -> balance)
          }
        )
      }
    }
  }

  routePath("/stateHash") - {
    "works" - {
      "at nonexistent height" in {
        Get(routePath(s"/stateHash/${domain.blockchain.height}")) ~> route ~> check {
          status shouldBe StatusCodes.NotFound
        }
      }

      "at existing height" in {
        (0 until (3 - domain.blockchain.height).min(0)) foreach { _ =>
          domain.appendBlock()
        }

        val lastButOneHeight    = domain.blockchain.height - 1
        val lastButOneHeader    = domain.blockchain.blockHeader(lastButOneHeight).value
        val lastButOneStateHash = domain.rocksDBWriter.loadStateHash(lastButOneHeight).value
        val expectedResponse = Json.toJson(lastButOneStateHash).as[JsObject] ++ Json.obj(
          "blockId"    -> lastButOneHeader.id().toString,
          "baseTarget" -> lastButOneHeader.header.baseTarget,
          "height"     -> lastButOneHeight,
          "version"    -> Version.VersionString
        )

        Get(routePath(s"/stateHash/last")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponse
        }

        Get(routePath(s"/stateHash/$lastButOneHeight")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponse
        }
      }
    }
  }

  routePath("/validate") - {
    def validatePost(tx: Transaction) =
      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, tx.json().toString()))

    "takes the priority pool into account" in {
      domain.appendBlock(TxHelpers.transfer(to = TxHelpers.secondAddress, amount = 1.waves + TestValues.fee))

      val tx = TxHelpers.transfer(TxHelpers.secondSigner, TestValues.address, 1.waves)
      validatePost(tx) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
      }
    }

    "valid tx" in {
      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, 1.waves)
      validatePost(tx) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
      }
    }

    "invalid tx" in {
      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, ENOUGH_AMT)
      validatePost(tx) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("Attempt to transfer unavailable funds")
      }
    }

    "NoSuchElementException" in {
      validatePost(TxHelpers.invoke()) ~> route ~> check {
        (responseAs[JsValue] \ "error").as[String] should include("No contract at address")
        response.status shouldBe StatusCodes.OK
      }
    }

    "exchange tx with fail script" in {
      val assetIssuer = TxHelpers.signer(1021)
      val o1sender    = TxHelpers.signer(1022)
      val o2sender    = TxHelpers.signer(1023)

      val issue = TxHelpers.issue(assetIssuer, script = Some(TestCompiler(V5).compileExpression("true")))
      domain.appendBlock(
        TxHelpers.massTransfer(
          richAccount,
          Seq(o1sender.toAddress -> 1.waves, o2sender.toAddress -> 1.waves, assetIssuer.toAddress -> 3.waves),
          fee = 0.003.waves
        ),
        issue,
        TxHelpers.massTransfer(assetIssuer, Seq(o1sender.toAddress -> 50, o2sender.toAddress -> 50), issue.asset, fee = 0.006.waves),
        TxHelpers.setAssetScript(assetIssuer, issue.asset, TestCompiler(V5).compileExpression("if true then throw(\"error\") else false"))
      )

      jsonPost(
        routePath("/validate"),
        TxHelpers.exchangeFromOrders(TxHelpers.orderV3(OrderType.BUY, issue.asset), TxHelpers.orderV3(OrderType.SELL, issue.asset)).json()
      ) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("not allowed by script of the asset")
        (json \ "trace").as[JsArray] shouldBe Json.parse(
          s"[{\"type\":\"asset\",\"context\":\"orderAmount\",\"id\":\"${issue.id()}\",\"result\":\"failure\",\"vars\":[{\"name\":\"throw.@args\",\"type\":\"Array\",\"value\":[{\"type\":\"String\",\"value\":\"error\"}]},{\"name\":\"throw.@complexity\",\"type\":\"Int\",\"value\":1},{\"name\":\"@complexityLimit\",\"type\":\"Int\",\"value\":2147483646}],\"error\":\"error\"}]"
        )
      }
    }

    "invoke tx with asset failing" in {
      val dapp    = TxHelpers.signer(1031)
      val invoker = TxHelpers.signer(1032)

      val failingAssetScript = TestCompiler(V4).compileExpression("""let test = true
                                                                    |if (test) then throw("error") else !test
                                                                    |""".stripMargin)
      val issue = TxHelpers.issue(dapp, script = Some(TestCompiler(V4).compileExpression("true")))
      domain.appendBlock(
        TxHelpers.transfer(richAccount, dapp.toAddress, 10.waves),
        TxHelpers.transfer(richAccount, invoker.toAddress, 10.waves),
        issue,
        TxHelpers.transfer(dapp, invoker.toAddress, 1, issue.asset),
        TxHelpers.setScript(
          dapp,
          TestCompiler(V4)
            .compileContract(s"""@Callable(i)
                                |func default() = []
                                |
                                |@Callable(i)
                                |func dataAndTransfer() = [
                                |     IntegerEntry("key", 1),
                                |     BooleanEntry("key", true),
                                |     StringEntry("key", "str"),
                                |     BinaryEntry("key", base58''),
                                |     DeleteEntry("key"),
                                |     ScriptTransfer(Address(base58'${TxHelpers.secondAddress}'), 1, base58'${issue.asset}')
                                |]
                                |
                                |@Callable(i)
                                |func issue() = {
                                |  let decimals = 4
                                |  [Issue("name", "description", 1000, decimals, true, unit, 0)]
                                |}
                                |
                                |@Callable(i)
                                |func reissue() = [Reissue(base58'${issue.asset}', 1, false)]
                                |
                                |@Callable(i)
                                |func burn() = [Burn(base58'${issue.asset}', 1)]
                                |""".stripMargin)
        ),
        TxHelpers.setScript(invoker, TestCompiler(V4).compileExpression("true")),
        TxHelpers.setAssetScript(dapp, issue.asset, failingAssetScript)
      )

      def testFunction(name: String, result: InvokeScriptTransaction => String) = withClue(s"function $name") {
        val tx = TxHelpers.invoke(dapp.toAddress, func = Some(name), fee = 102500000, invoker = invoker)

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
        val tx = TxHelpers.invoke(dapp.toAddress, fee = 1300000, payments = Seq(Payment(1L, issue.asset)), invoker = invoker)

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
                           |  "id" : "${invoker.toAddress}",
                           |  "result" : "success",
                           |  "error" : null
                           |}, {
                           |  "type" : "dApp",
                           |  "id" : "${dapp.toAddress}",
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
                           |              "value" : "${issue.asset}"
                           |            }
                           |          }
                           |        } ]
                           |      },
                           |      "callerPublicKey" : {
                           |        "type" : "ByteVector",
                           |        "value" : "${invoker.publicKey}"
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
                           |            "value" : "${invoker.toAddress}"
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
                           |    "value" : 51998
                           |  } ]
                           |}, {
                           |  "type" : "asset",
                           |  "context" : "payment",
                           |  "id" : "${issue.id()}",
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
                 |  "id" : "${invoker.toAddress}",
                 |  "result" : "success",
                 |  "error" : null
                 |}, {
                 |  "type" : "dApp",
                 |  "id" : "${dapp.toAddress}",
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
                 |      "asset" : "${issue.asset}",
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
                 |        "value" : "${invoker.publicKey}"
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
                 |            "value" : "${invoker.toAddress}"
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
                 |      "value" : "${issue.asset}"
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
                 |          "value" : "${issue.asset}"
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
                 |            "value" : "${issue.asset}"
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
                 |            "value" : "${issue.asset}"
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
                 |            "value" : "${issue.asset}"
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
                 |            "value" : "${issue.asset}"
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
                 |            "value" : "${issue.asset}"
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
                 |  "id" : "${issue.asset}",
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
                 |  "id" : "${invoker.toAddress}",
                 |  "result" : "success",
                 |  "error" : null
                 |}, {
                 |  "type" : "dApp",
                 |  "id" : "${dapp.toAddress}",
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
                 |        "value" : "${invoker.publicKey}"
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
                 |            "value" : "${invoker.toAddress}"
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
                 |  "id" : "${invoker.toAddress}",
                 |  "result" : "success",
                 |  "error" : null
                 |}, {
                 |  "type" : "dApp",
                 |  "id" : "${dapp.toAddress}",
                 |  "function" : "reissue",
                 |  "args" : [ ],
                 |  "invocations" : [ ],
                 |  "result" : {
                 |    "data" : [ ],
                 |    "transfers" : [ ],
                 |    "issues" : [ ],
                 |    "reissues" : [ {
                 |      "assetId" : "${issue.asset}",
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
                 |        "value" : "${invoker.publicKey}"
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
                 |            "value" : "${invoker.toAddress}"
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
                 |      "value" : "${issue.asset}"
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
                 |          "value" : "${issue.asset}"
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
                 |  "id" : "${issue.asset}",
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
                 |  "id" : "${invoker.toAddress}",
                 |  "result" : "success",
                 |  "error" : null
                 |}, {
                 |  "type" : "dApp",
                 |  "id" : "${dapp.toAddress}",
                 |  "function" : "burn",
                 |  "args" : [ ],
                 |  "invocations" : [ ],
                 |  "result" : {
                 |    "data" : [ ],
                 |    "transfers" : [ ],
                 |    "issues" : [ ],
                 |    "reissues" : [ ],
                 |    "burns" : [ {
                 |      "assetId" : "${issue.asset}",
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
                 |        "value" : "${invoker.publicKey}"
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
                 |            "value" : "${invoker.toAddress}"
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
                 |      "value" : "${issue.asset}"
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
                 |          "value" : "${issue.asset}"
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
                 |  "id" : "${issue.asset}",
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
      val dAppPk      = TxHelpers.signer(1040)
      val dAppAddress = dAppPk.toAddress

      val leaseRecipient = TxHelpers.address(1041)
      val aliasOwner     = TxHelpers.signer(1042)
      val caller         = TxHelpers.signer(1043)

      val invoke          = TxHelpers.invoke(dAppPk.toAddress, invoker = caller)
      val originalLease   = TxHelpers.lease(dAppPk, leaseRecipient)
      val canceledLeaseId = originalLease.id()

      val amount1    = 100
      val recipient1 = Recipient.Address(ByteStr.decodeBase58("3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd").get)
      val nonce1     = 0
      val leaseId1   = Lease.calculateId(Lease(recipient1, amount1, nonce1), invoke.id())

      val amount2    = 20
      val recipient2 = Recipient.Alias("some_alias")
      val nonce2     = 2
      val leaseId2   = Lease.calculateId(Lease(recipient2, amount2, nonce2), invoke.id())

      domain.appendBlock(
        TxHelpers.massTransfer(
          richAccount,
          Seq(dAppPk.toAddress -> 20.waves, aliasOwner.toAddress -> 10.waves, caller.toAddress -> 5.waves),
          fee = 0.003.waves
        ),
        originalLease,
        TxHelpers.createAlias("some_alias", aliasOwner),
        TxHelpers.setScript(
          dAppPk,
          TestCompiler(V6).compileContract(s"""@Callable(i)
                                              |func default() = [
                                              |  LeaseCancel(base58'$canceledLeaseId'),
                                              |  Lease(Address(base58'3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd'), 100, 0),
                                              |  Lease(Alias("some_alias"), 20, 2)
                                              |]
                                              |""".stripMargin)
        ),
        TxHelpers.setScript(caller, TestCompiler(V6).compileExpression("true"))
      )

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
                                                                 |    "height" : ${domain.blockchain.height},
                                                                 |    "status" : "active",
                                                                 |    "cancelHeight" : null,
                                                                 |    "cancelTransactionId" : null
                                                                 |  }, {
                                                                 |    "id" : "$leaseId2",
                                                                 |    "originTransactionId" : "${invoke.id()}",
                                                                 |    "sender" : "$dAppAddress",
                                                                 |    "recipient" : "${aliasOwner.toAddress}",
                                                                 |    "amount" : 20,
                                                                 |    "height" : ${domain.blockchain.height},
                                                                 |    "status" : "active",
                                                                 |    "cancelHeight" : null,
                                                                 |    "cancelTransactionId" : null
                                                                 |  } ],
                                                                 |  "leaseCancels" : [ {
                                                                 |    "id" : "$canceledLeaseId",
                                                                 |    "originTransactionId" : "${originalLease.id()}",
                                                                 |    "sender" : "$dAppAddress",
                                                                 |    "recipient" : "$leaseRecipient",
                                                                 |    "amount" : ${originalLease.amount.value},
                                                                 |    "height" : ${domain.blockchain.height},
                                                                 |    "status" : "canceled",
                                                                 |    "cancelHeight" : ${domain.blockchain.height},
                                                                 |    "cancelTransactionId" : "${invoke.id()}"
                                                                 |  } ],
                                                                 |  "invokes" : [ ]
                                                                 |}""".stripMargin)
        (json \ "trace").as[JsArray] should matchJson(
          s"""[ {
             |  "type" : "verifier",
             |  "id" : "${caller.toAddress}",
             |  "result" : "success",
             |  "error" : null
             |}, {
             |  "type" : "dApp",
             |  "id" : "${dAppAddress}",
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
             |      "sender" : "$dAppAddress",
             |      "recipient" : "${recipient1.bytes}",
             |      "amount" : 100,
             |      "height" : ${domain.blockchain.height},
             |      "status" : "active",
             |      "cancelHeight" : null,
             |      "cancelTransactionId" : null
             |    }, {
             |      "id" : "$leaseId2",
             |      "originTransactionId" : "${invoke.id()}",
             |      "sender" : "$dAppAddress",
             |      "recipient" : "${aliasOwner.toAddress}",
             |      "amount" : 20,
             |      "height" : ${domain.blockchain.height},
             |      "status" : "active",
             |      "cancelHeight" : null,
             |      "cancelTransactionId" : null
             |    } ],
             |    "leaseCancels" : [ {
             |      "id" : "$canceledLeaseId",
             |      "originTransactionId" : "${originalLease.id()}",
             |      "sender" : "$dAppAddress",
             |      "recipient" : "$leaseRecipient",
             |      "amount" : ${originalLease.amount.value},
             |      "height" : ${domain.blockchain.height},
             |      "status" : "canceled",
             |      "cancelHeight" : ${domain.blockchain.height},
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
             |            "value" : "${caller.toAddress}"
             |          }
             |        }
             |      },
             |      "payments" : {
             |        "type" : "Array",
             |        "value" : [ ]
             |      },
             |      "callerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "${caller.publicKey}"
             |      },
             |      "feeAssetId" : {
             |        "type" : "Unit",
             |        "value" : { }
             |      },
             |      "originCallerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "${caller.publicKey}"
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
             |            "value" : "${caller.toAddress}"
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
             |    "name" : "LeaseCancel.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "ByteVector",
             |      "value" : "${originalLease.id()}"
             |    } ]
             |  }, {
             |    "name" : "LeaseCancel.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 51999
             |  }, {
             |    "name" : "Address.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "ByteVector",
             |      "value" : "${recipient1.bytes}"
             |    } ]
             |  }, {
             |    "name" : "Address.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 51998
             |  }, {
             |    "name" : "Lease.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "Address",
             |      "value" : {
             |        "bytes" : {
             |          "type" : "ByteVector",
             |          "value" : "${recipient1.bytes}"
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
             |    "value" : 51997
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
             |    "value" : 51996
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
             |    "value" : 51995
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
             |      "value" : [ ]
             |    } ]
             |  }, {
             |    "name" : "cons.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 51994
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
             |              "value" : "${recipient1.bytes}"
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
             |      } ]
             |    } ]
             |  }, {
             |    "name" : "cons.@complexity",
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
             |      "type" : "LeaseCancel",
             |      "value" : {
             |        "leaseId" : {
             |          "type" : "ByteVector",
             |          "value" : "${originalLease.id()}"
             |        }
             |      }
             |    }, {
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "Lease",
             |        "value" : {
             |          "recipient" : {
             |            "type" : "Address",
             |            "value" : {
             |              "bytes" : {
             |                "type" : "ByteVector",
             |                "value" : "${recipient1.bytes}"
             |              }
             |            }
             |          },
             |          "amount" : {
             |            "type" : "Int",
             |            "value" : 100
             |          },
             |          "nonce" : {
             |            "type" : "Int",
             |            "value" : 0
             |          }
             |        }
             |      }, {
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
             |      } ]
             |    } ]
             |  }, {
             |    "name" : "cons.@complexity",
             |    "type" : "Int",
             |    "value" : 1
             |  }, {
             |    "name" : "@complexityLimit",
             |    "type" : "Int",
             |    "value" : 51992
             |  } ]
             |} ]
             |""".stripMargin
        )
        (json \ "height").as[Int] shouldBe domain.blockchain.height
      }
    }

    "invoke tx returning leases with error" in {
      val dApp1Kp      = signer(911)
      val dApp2Kp      = signer(912)
      val leaseAddress = signer(913).toAddress
      val amount       = 123

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
      domain.appendBlock(
        TxHelpers.massTransfer(
          richAccount,
          Seq(
            dApp1Kp.toAddress -> 20.waves,
            dApp2Kp.toAddress -> 20.waves
          ),
          fee = 0.002.waves
        ),
        leaseTx,
        setScript(dApp1Kp, dApp1),
        setScript(dApp2Kp, dApp2)
      )
      val leaseTxHeight = domain.blockchain.height

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
             |      "id": "3N79VJyPMGWTf1mgfqUZw3Ce6GKoERBoc6Y",
             |      "function": "default",
             |      "args": [],
             |      "invocations": [
             |        {
             |          "type": "dApp",
             |          "id": "3NCMNUhdTDxGv1Q21ZC6Kuk6hGuPNFoMvmh",
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
             |                "height" : $leaseTxHeight,
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
             |                  "value": "FAGx8acozxdhVGLpKzZy2RqNsoPEjZUQu4zvizNzbZyX"
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
             |                      "value": "3N79VJyPMGWTf1mgfqUZw3Ce6GKoERBoc6Y"
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
             |          "value" : "3NCMNUhdTDxGv1Q21ZC6Kuk6hGuPNFoMvmh"
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
             |              "value" : "3NCMNUhdTDxGv1Q21ZC6Kuk6hGuPNFoMvmh"
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
             |  "height": ${domain.blockchain.height},
             |  "error": "Error while executing dApp: Explicit script termination",
             |  "transaction": {
             |    "type": 16,
             |    "id": "${invoke.id()}",
             |    "fee": 500000,
             |    "feeAssetId": null,
             |    "timestamp": ${invoke.timestamp},
             |    "version": 2,
             |    "chainId": 84,
             |    "sender": "${invoke.senderAddress}",
             |    "senderPublicKey": "${invoke.sender}",
             |    "proofs": [ "${(json \ "transaction" \ "proofs" \ 0).as[String]}" ],
             |    "dApp": "3N79VJyPMGWTf1mgfqUZw3Ce6GKoERBoc6Y",
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

    "invoke tx with nested call" in {
      val dAppPk = TxHelpers.signer(1050)
      val caller = TxHelpers.signer(1051)
      val invoke = TxHelpers.invoke(dAppPk.toAddress, func = Some("test1"), invoker = caller)

      val dAppScript = TestCompiler(V5).compileContract(
        s"""@Callable(i)
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
           |""".stripMargin
      )
      domain.appendBlock(
        TxHelpers.transfer(richAccount, dAppPk.toAddress, 10.waves),
        TxHelpers.transfer(richAccount, caller.toAddress, 10.waves),
        TxHelpers.setScript(dAppPk, dAppScript),
        TxHelpers.setScript(caller, TestCompiler(V5).compileExpression("true"))
      )

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
                                                                 |    "dApp" : "${dAppPk.toAddress}",
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
             |  "id" : "${caller.toAddress}",
             |  "result" : "success",
             |  "error" : null
             |}, {
             |  "type" : "dApp",
             |  "id" : "${dAppPk.toAddress}",
             |  "function" : "test1",
             |  "args" : [ ],
             |  "invocations" : [ {
             |    "type" : "dApp",
             |    "id" : "${dAppPk.toAddress}",
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
             |              "value" : "${caller.toAddress}"
             |            }
             |          }
             |        },
             |        "payments" : {
             |          "type" : "Array",
             |          "value" : [ ]
             |        },
             |        "callerPublicKey" : {
             |          "type" : "ByteVector",
             |          "value" : "${dAppPk.publicKey}"
             |        },
             |        "feeAssetId" : {
             |          "type" : "Unit",
             |          "value" : { }
             |        },
             |        "originCallerPublicKey" : {
             |          "type" : "ByteVector",
             |          "value" : "${caller.publicKey}"
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
             |              "value" : "${dAppPk.toAddress}"
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
             |            "value" : "${caller.toAddress}"
             |          }
             |        }
             |      },
             |      "payments" : {
             |        "type" : "Array",
             |        "value" : [ ]
             |      },
             |      "callerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "${caller.publicKey}"
             |      },
             |      "feeAssetId" : {
             |        "type" : "Unit",
             |        "value" : { }
             |      },
             |      "originCallerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "${caller.publicKey}"
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
             |            "value" : "${caller.toAddress}"
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
             |          "value" : "${dAppPk.toAddress}"
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
        (json \ "height").as[Int] shouldBe domain.blockchain.height
      }
    }

    "transfer transaction with asset fail" in {
      val assetOwner = TxHelpers.signer(1060)
      val issue      = TxHelpers.issue(assetOwner, script = Some(TestCompiler(V5).compileAsset("false")))
      domain.appendBlock(
        TxHelpers.transfer(richAccount, assetOwner.toAddress, 5.waves),
        issue
      )

      val tx = TxHelpers.transfer(assetOwner, TxHelpers.defaultAddress, 1, issue.asset)

      jsonPost(routePath("/validate"), tx.json()) ~> route ~> check {
        val json = responseAs[JsObject]
        (json \ "trace").as[JsArray] should matchJson(s"""[ {
                                                         |    "type" : "asset",
                                                         |    "context" : "transfer",
                                                         |    "id" : "${issue.id()}",
                                                         |    "result" : "failure",
                                                         |    "vars" : [ ],
                                                         |    "error" : null
                                                         |  } ]""".stripMargin)

        (json \ "valid").as[Boolean] shouldBe false
        (json \ "transaction").as[JsObject] shouldBe tx.json()
      }
    }

    "txs with empty and small verifier" in {
      val transferFee = 100000

      val acc1 = TxHelpers.signer(1070)
      val acc2 = TxHelpers.signer(1071)
      val acc3 = TxHelpers.signer(1072)

      domain.appendBlock(
        TxHelpers.massTransfer(
          richAccount,
          Seq(
            acc1.toAddress -> 2.waves,
            acc2.toAddress -> 2.waves,
            acc3.toAddress -> 2.waves
          ),
          fee = 0.003.waves
        ),
        TxHelpers.setScript(
          acc2,
          TestCompiler(V5).compileExpression(
            "sigVerify(base58'', base58'', base58'') && sigVerify(base58'', base58'', base58'') && false"
          )
        ),
        TxHelpers.setScript(
          acc3,
          TestCompiler(V5).compileExpression(
            "sigVerify_16Kb(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)"
          )
        )
      )

      val tx = TxHelpers.transfer(acc1, TxHelpers.secondSigner.toAddress, 1.waves, fee = transferFee, version = TxVersion.V2)
      validatePost(tx) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
      }

      val tx2 = TxHelpers.transfer(acc2, TestValues.address, 1.waves, fee = transferFee, version = TxVersion.V2)
      validatePost(tx2) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "error").as[String] should include("Requires 400000 extra fee")
      }

      val tx3 = TxHelpers.transfer(acc3, TestValues.address, 1.waves, fee = transferFee, version = TxVersion.V2)
      validatePost(tx3) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "valid").as[Boolean] shouldBe true
      }
    }

    "InvokeExpression" in {
      val sender = TxHelpers.signer(1080)
      val issue  = TxHelpers.issue(sender)
      val expression = TestCompiler(V6).compileFreeCall(
        s"""
           | let assetId = base58'${issue.asset}'
           | [ Reissue(assetId, 1, true) ]
         """.stripMargin
      )
      domain.appendBlock(
        TxHelpers.transfer(richAccount, sender.toAddress, 2.waves),
        issue
      )
      val invokeExpression = TxHelpers.invokeExpression(expression, sender)
      jsonPost(routePath("/validate"), invokeExpression.json()) ~> route ~> check {
        val json = responseAs[JsValue]
        (json \ "expression").as[String] shouldBe expression.bytes.value().base64
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "trace").as[JsArray] should matchJson(
          s"""
             |[ {
             |  "type" : "dApp",
             |  "id" : "${sender.toAddress}",
             |  "function" : "default",
             |  "args" : [ ],
             |  "invocations" : [ ],
             |  "result" : {
             |    "data" : [ ],
             |    "transfers" : [ ],
             |    "issues" : [ ],
             |    "reissues" : [ {
             |      "assetId" : "${issue.id()}",
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
             |            "value" : "${sender.toAddress}"
             |          }
             |        }
             |      },
             |      "payments" : {
             |        "type" : "Array",
             |        "value" : [ ]
             |      },
             |      "callerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "${sender.publicKey}"
             |      },
             |      "feeAssetId" : {
             |        "type" : "Unit",
             |        "value" : { }
             |      },
             |      "originCallerPublicKey" : {
             |        "type" : "ByteVector",
             |        "value" : "${sender.publicKey}"
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
             |            "value" : "${sender.toAddress}"
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
             |    "value" : "${issue.id()}"
             |  }, {
             |    "name" : "Reissue.@args",
             |    "type" : "Array",
             |    "value" : [ {
             |      "type" : "ByteVector",
             |      "value" : "${issue.id()}"
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
             |          "value" : "${issue.id()}"
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
  }

  routePath("/minerInfo") - {
    "returns info from wallet if miner private keys not specified in config" in {
      val acc = domain.wallet.generateNewAccount()

      acc shouldBe defined
      Get(routePath("/minerInfo")) ~> ApiKeyHeader ~> route ~> check {
        responseAs[Seq[AccountMiningInfo]].map(_.address) shouldBe acc.toSeq.map(_.toAddress.toString)
      }
    }

    "returns info only for miner private keys from config when specified" in {
      val minerAccs   = Seq(TxHelpers.signer(1090), TxHelpers.signer(1091))
      val minerConfig = debugApiRoute.ws.minerSettings.copy(privateKeys = minerAccs.map(_.privateKey))
      val debugRoute  = debugApiRoute.copy(ws = debugApiRoute.ws.copy(minerSettings = minerConfig))

      Get(routePath("/minerInfo")) ~> ApiKeyHeader ~> debugRoute.route ~> check {
        responseAs[Seq[AccountMiningInfo]].map(_.address) shouldBe minerAccs.map(_.toAddress.toString)
      }
    }
  }

  private[this] def jsonPost(path: String, json: JsValue) = {
    Post(path, HttpEntity(ContentTypes.`application/json`, json.toString()))
  }
}
