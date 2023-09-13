package com.wavesplatform.http

import com.wavesplatform.BlockchainStubHelpers
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.{RouteTimeout, TransactionsApiRoute}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.traits.domain.{Lease, Recipient}
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.state.{AccountScriptInfo, Blockchain}
import com.wavesplatform.test.TestTime
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.script.trace.{AccountVerifierTrace, TracedResult}
import com.wavesplatform.transaction.{Asset, Proofs, TxHelpers, TxPositiveAmount, TxVersion}
import com.wavesplatform.utils.{EthEncoding, EthHelpers, SharedSchedulerMixin}
import com.wavesplatform.wallet.Wallet
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Random

class TransactionBroadcastSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with PathMockFactory
    with BlockchainStubHelpers
    with EthHelpers
    with SharedSchedulerMixin {
  private val blockchain           = stub[Blockchain]
  private val transactionPublisher = stub[TransactionPublisher]
  private val testTime             = new TestTime

  private val transactionsApiRoute = new TransactionsApiRoute(
    restAPISettings,
    isLightMode = false,
    stub[CommonTransactionsApi],
    stub[Wallet],
    blockchain,
    stub[() => SnapshotBlockchain],
    mockFunction[Int],
    transactionPublisher,
    testTime,
    new RouteTimeout(60.seconds)(sharedScheduler)
  )

  private val route = seal(transactionsApiRoute.route)

  "exchange" - {
    "accepted with ETH signed orders" in {
      import com.wavesplatform.transaction.assets.exchange.EthOrderSpec.{ethBuyOrder, ethSellOrder}

      val blockchain = createBlockchainStub { blockchain =>
        val sh = StubHelpers(blockchain)
        sh.creditBalance(TxHelpers.matcher.toAddress, *)
        sh.creditBalance(ethBuyOrder.senderAddress, *)
        sh.creditBalance(ethSellOrder.senderAddress, *)
        (blockchain.wavesBalances _)
          .when(*)
          .returns(
            Map(
              TxHelpers.matcher.toAddress -> Long.MaxValue / 3,
              ethBuyOrder.senderAddress   -> Long.MaxValue / 3,
              ethSellOrder.senderAddress  -> Long.MaxValue / 3
            )
          )
        sh.issueAsset(ByteStr(EthStubBytes32))
      }

      val transactionPublisher = blockchain.stub.transactionPublisher(testTime)

      val route = transactionsApiRoute.copy(blockchain = blockchain, transactionPublisher = transactionPublisher).route

      val transaction = TxHelpers.exchange(
        ethBuyOrder,
        ethSellOrder,
        price = 100,
        buyMatcherFee = ethBuyOrder.matcherFee.value,
        sellMatcherFee = ethSellOrder.matcherFee.value,
        version = TxVersion.V3,
        timestamp = 100
      )
      testTime.setTime(100)
      val validResponseJson =
        s"""{
           |  "type" : 7,
           |  "id" : "${transaction.id()}",
           |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
           |  "senderPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
           |  "fee" : 1000000,
           |  "feeAssetId" : null,
           |  "timestamp" : 100,
           |  "proofs" : [ "${transaction.signature}" ],
           |  "version" : 3,
           |  "chainId" : 84,
           |  "order1" : {
           |    "version" : 4,
           |    "id" : "${ethBuyOrder.id()}",
           |    "sender" : "${ethBuyOrder.senderPublicKey.toAddress}",
           |    "senderPublicKey" : "${ethBuyOrder.senderPublicKey}",
           |    "matcherPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
           |    "assetPair" : {
           |      "amountAsset" : "5fQPsn8hoaVddFG26cWQ5QFdqxWtUPNaZ9zH2E6LYzFn",
           |      "priceAsset" : null
           |    },
           |    "orderType" : "buy",
           |    "amount" : 1,
           |    "price" : 100,
           |    "timestamp" : 1,
           |    "expiration" : 123,
           |    "matcherFee" : 100000,
           |    "signature" : "",
           |    "proofs" : [ ],
           |    "matcherFeeAssetId" : null,
           |    "eip712Signature" : "${EthEncoding.toHexString(ethBuyOrder.eip712Signature.get.arr)}",
           |    "priceMode" : null
           |  },
           |  "order2" : {
           |    "version" : 4,
           |    "id" : "${ethSellOrder.id()}",
           |    "sender" : "${ethSellOrder.senderPublicKey.toAddress}",
           |    "senderPublicKey" : "${ethSellOrder.senderPublicKey}",
           |    "matcherPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
           |    "assetPair" : {
           |      "amountAsset" : "5fQPsn8hoaVddFG26cWQ5QFdqxWtUPNaZ9zH2E6LYzFn",
           |      "priceAsset" : null
           |    },
           |    "orderType" : "sell",
           |    "amount" : 1,
           |    "price" : 100,
           |    "timestamp" : 1,
           |    "expiration" : 123,
           |    "matcherFee" : 100000,
           |    "signature" : "",
           |    "proofs" : [ ],
           |    "matcherFeeAssetId" : null,
           |    "eip712Signature" : "${EthEncoding.toHexString(ethSellOrder.eip712Signature.get.arr)}",
           |    "priceMode" : null
           |  },
           |  "amount" : 1,
           |  "price" : 100,
           |  "buyMatcherFee" : 100000,
           |  "sellMatcherFee" : 100000
           |}
           |""".stripMargin

      Post(routePath("/broadcast"), transaction.json()) ~> route ~> check {
        responseAs[JsObject] should matchJson(validResponseJson)
      }

      def removeFields(json: JsObject, fields: String*): JsObject = {
        val order1 = (json \ "order1").as[JsObject]
        val order2 = (json \ "order2").as[JsObject]
        json + ("order1" -> fields.foldLeft(order1)(_ - _)) + ("order2" -> fields.foldLeft(order2)(_ - _))
      }

      Post(routePath("/broadcast"), removeFields(transaction.json(), "senderPublicKey")) ~> route ~> check {
        responseAs[JsObject] should matchJson(validResponseJson)
      }

      Post(routePath("/broadcast"), removeFields(transaction.json(), "senderPublicKey", "eip712Signature")) ~> route ~> check {
        responseAs[JsObject] should matchJson("""{
                                                |  "error" : 199,
                                                |  "message" : "Either senderPublicKey or eip712Signature should be provided"
                                                |}""".stripMargin)
      }
    }
  }

  "invoke script" - {
    def withInvokeScriptTransaction(f: (KeyPair, InvokeScriptTransaction) => Unit): Unit = {
      val seed = new Array[Byte](32)
      Random.nextBytes(seed)
      val sender: KeyPair = KeyPair(seed)
      val ist = InvokeScriptTransaction(
        TxVersion.V1,
        sender.publicKey,
        sender.toAddress,
        None,
        Seq.empty,
        TxPositiveAmount.unsafeFrom(500000L),
        Asset.Waves,
        testTime.getTimestamp(),
        Proofs.empty,
        AddressScheme.current.chainId
      ).signWith(sender.privateKey)
      f(sender, ist)
    }

    "shows trace when trace is enabled" in withInvokeScriptTransaction { (sender, ist) =>
      val accountTrace = AccountVerifierTrace(sender.toAddress, Some(GenericError("Error in account script")))
      (transactionPublisher.validateAndBroadcast _)
        .when(*, None)
        .returning(
          Future.successful(TracedResult(Right(true), List(accountTrace)))
        )
      Post(routePath("/broadcast?trace=true"), ist.json()) ~> route ~> check {
        val result = responseAs[JsObject]
        (result \ "trace").as[JsValue] shouldBe Json.arr(accountTrace.json)
      }
    }

    "does not show trace when trace is disabled" in withInvokeScriptTransaction { (sender, ist) =>
      val accountTrace = AccountVerifierTrace(sender.toAddress, Some(GenericError("Error in account script")))
      (transactionPublisher.validateAndBroadcast _)
        .when(*, None)
        .returning(
          Future.successful(TracedResult(Right(true), List(accountTrace)))
        )
      Post(routePath("/broadcast"), ist.json()) ~> route ~> check {
        (responseAs[JsObject] \ "trace") shouldBe empty
      }
      Post(routePath("/broadcast?trace=false"), ist.json()) ~> route ~> check {
        (responseAs[JsObject] \ "trace") shouldBe empty
      }
    }

    "generates valid trace with vars" in {
      val invoke        = TxHelpers.invoke(TxHelpers.defaultAddress, Some("test"), version = TxVersion.V1)
      val leaseCancelId = ByteStr(bytes32gen.sample.get)

      val amount1    = 100
      val nonce1     = 0
      val recipient1 = Recipient.Address(ByteStr.decodeBase58("3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd").get)
      val leaseId1   = Lease.calculateId(Lease(recipient1, amount1, nonce1), invoke.id())

      val amount2    = 20
      val nonce2     = 2
      val recipient2 = Recipient.Alias("some_alias")
      val leaseId2   = Lease.calculateId(Lease(recipient2, amount2, nonce2), invoke.id())

      val blockchain = createBlockchainStub { blockchain =>
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

        (blockchain.leaseDetails _)
          .when(*)
          .returns(None)
          .anyNumberOfTimes()
        (blockchain.resolveAlias _)
          .when(*)
          .returns(Right(accountGen.sample.get.toAddress))
          .anyNumberOfTimes()
        (blockchain.accountScript _)
          .when(*)
          .returns(
            Some(
              AccountScriptInfo(
                TxHelpers.defaultSigner.publicKey,
                dAppScript,
                0L,
                Map(3 -> Seq("test").map(_ -> 0L).toMap)
              )
            )
          )

        (blockchain.hasAccountScript _).when(*).returns(true)
      }
      val publisher = createTxPublisherStub(blockchain, enableExecutionLog = true)
      val route     = transactionsApiRoute.copy(blockchain = blockchain, transactionPublisher = publisher).route

      Post(routePath("/broadcast?trace=true"), invoke.json()) ~> route ~> check {
        responseAs[JsObject] should matchJson(
          s"""{
             |  "error" : 306,
             |  "message" : "Error while executing dApp: Lease with id=$leaseCancelId not found",
             |  "transaction" : {
             |    "type" : 16,
             |    "id" : "${invoke.id()}",
             |    "fee" : 500000,
             |    "feeAssetId" : null,
             |    "timestamp" : ${invoke.timestamp},
             |    "version" : 1,
             |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |    "senderPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
             |    "proofs" : [ "${invoke.signature}" ],
             |    "dApp" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |    "payment" : [ ],
             |    "call" : {
             |      "function" : "test",
             |      "args" : [ ]
             |    }
             |  },
             |  "trace" : [ {
             |    "type" : "verifier",
             |    "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |    "result" : "success",
             |    "error" : null
             |  }, {
             |    "type" : "dApp",
             |    "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |    "function" : "test",
             |    "args" : [ ],
             |    "invocations" : [ ],
             |    "result" : {
             |      "data" : [ ],
             |      "transfers" : [ ],
             |      "issues" : [ ],
             |      "reissues" : [ ],
             |      "burns" : [ ],
             |      "sponsorFees" : [ ],
             |      "leases" : [ {
             |        "recipient" : "${recipient1.bytes}",
             |        "amount" : $amount1,
             |        "nonce" : $nonce1,
             |        "id" : "$leaseId1"
             |      }, {
             |        "recipient" : "alias:T:${recipient2.name}",
             |        "amount" : $amount2,
             |        "nonce" : $nonce2,
             |        "id" : "$leaseId2"
             |      } ],
             |      "leaseCancels" : [ {
             |        "id" : "$leaseCancelId"
             |      } ],
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
             |      "value" : 25999
             |    }, {
             |      "name" : "Address.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "ByteVector",
             |        "value" : "3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd"
             |      } ]
             |    }, {
             |      "name" : "Address.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25998
             |    }, {
             |      "name" : "Lease.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "Address",
             |        "value" : {
             |          "bytes" : {
             |            "type" : "ByteVector",
             |            "value" : "3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd"
             |          }
             |        }
             |      }, {
             |        "type" : "Int",
             |        "value" : 100
             |      }, {
             |        "type" : "Int",
             |        "value" : 0
             |      } ]
             |    }, {
             |      "name" : "Lease.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25997
             |    }, {
             |      "name" : "Alias.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "String",
             |        "value" : "some_alias"
             |      } ]
             |    }, {
             |      "name" : "Alias.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25996
             |    }, {
             |      "name" : "Lease.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "Alias",
             |        "value" : {
             |          "alias" : {
             |            "type" : "String",
             |            "value" : "some_alias"
             |          }
             |        }
             |      }, {
             |        "type" : "Int",
             |        "value" : 20
             |      }, {
             |        "type" : "Int",
             |        "value" : 2
             |      } ]
             |    }, {
             |      "name" : "Lease.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25995
             |    }, {
             |      "name" : "LeaseCancel.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "ByteVector",
             |        "value" : "$leaseCancelId"
             |      } ]
             |    }, {
             |      "name" : "LeaseCancel.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25994
             |    }, {
             |      "name" : "cons.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "LeaseCancel",
             |        "value" : {
             |          "leaseId" : {
             |            "type" : "ByteVector",
             |            "value" : "$leaseCancelId"
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
             |      "value" : 25993
             |    }, {
             |      "name" : "cons.@args",
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
             |        "type" : "Array",
             |        "value" : [ {
             |          "type" : "LeaseCancel",
             |          "value" : {
             |            "leaseId" : {
             |              "type" : "ByteVector",
             |              "value" : "$leaseCancelId"
             |            }
             |          }
             |        } ]
             |      } ]
             |    }, {
             |      "name" : "cons.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25992
             |    }, {
             |      "name" : "cons.@args",
             |      "type" : "Array",
             |      "value" : [ {
             |        "type" : "Lease",
             |        "value" : {
             |          "recipient" : {
             |            "type" : "Address",
             |            "value" : {
             |              "bytes" : {
             |                "type" : "ByteVector",
             |                "value" : "3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd"
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
             |        "type" : "Array",
             |        "value" : [ {
             |          "type" : "Lease",
             |          "value" : {
             |            "recipient" : {
             |              "type" : "Alias",
             |              "value" : {
             |                "alias" : {
             |                  "type" : "String",
             |                  "value" : "some_alias"
             |                }
             |              }
             |            },
             |            "amount" : {
             |              "type" : "Int",
             |              "value" : 20
             |            },
             |            "nonce" : {
             |              "type" : "Int",
             |              "value" : 2
             |            }
             |          }
             |        }, {
             |          "type" : "LeaseCancel",
             |          "value" : {
             |            "leaseId" : {
             |              "type" : "ByteVector",
             |              "value" : "$leaseCancelId"
             |            }
             |          }
             |        } ]
             |      } ]
             |    }, {
             |      "name" : "cons.@complexity",
             |      "type" : "Int",
             |      "value" : 1
             |    }, {
             |      "name" : "@complexityLimit",
             |      "type" : "Int",
             |      "value" : 25991
             |    } ]
             |  } ]
             |}
             |""".stripMargin
        )
      }
    }
  }
}
