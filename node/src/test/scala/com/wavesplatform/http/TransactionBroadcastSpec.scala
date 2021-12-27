package com.wavesplatform.http

import scala.concurrent.Future
import scala.util.Random

import com.wavesplatform.BlockchainStubHelpers
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.TransactionsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.traits.domain.{Lease, Recipient}
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.state.{AccountScriptInfo, Blockchain}
import com.wavesplatform.test.TestTime
import com.wavesplatform.transaction.{Asset, Proofs, TxHelpers, TxVersion}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.script.trace.{AccountVerifierTrace, TracedResult}
import com.wavesplatform.utils.{EthEncoding, EthHelpers}
import com.wavesplatform.wallet.Wallet
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.{JsObject, Json, JsValue}

class TransactionBroadcastSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with PathMockFactory
    with BlockchainStubHelpers
    with EthHelpers {
  private val blockchain           = stub[Blockchain]
  private val transactionPublisher = stub[TransactionPublisher]
  private val testTime             = new TestTime

  private val transactionsApiRoute = new TransactionsApiRoute(
    restAPISettings,
    stub[CommonTransactionsApi],
    stub[Wallet],
    blockchain,
    mockFunction[Int],
    transactionPublisher,
    testTime
  )

  private val route = seal(transactionsApiRoute.route)

  "exchange" - {
    "accepted with ETH signed orders" in EthChainId.withEChainId {
      val blockchain = createBlockchainStub { blockchain =>
        val sh = StubHelpers(blockchain)
        sh.creditBalance(TxHelpers.matcher.toAddress, *)
        sh.creditBalance(TestEthOrdersPublicKey.toAddress, *)
        sh.issueAsset(ByteStr(EthStubBytes32))
      }

      val transactionPublisher = blockchain.stub.transactionPublisher(testTime)

      val route = transactionsApiRoute.copy(blockchain = blockchain, transactionPublisher = transactionPublisher).route

      import com.wavesplatform.transaction.assets.exchange.EthOrderSpec.{ethBuyOrder, ethSellOrder}
      val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, TxVersion.V3, 100)
      testTime.setTime(100)
      val validResponseJson =
        s"""{
           |  "type" : 7,
           |  "id" : "${transaction.id()}",
           |  "sender" : "3FrCwv8uFRxQazhX6Lno45aZ68Bof6ScaeF",
           |  "senderPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
           |  "fee" : 1000000,
           |  "feeAssetId" : null,
           |  "timestamp" : 100,
           |  "proofs" : [ "${transaction.signature}" ],
           |  "version" : 3,
           |  "chainId" : 69,
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
           |    "priceMode" : "fixedDecimals",
           |    "signature" : "",
           |    "proofs" : [ ],
           |    "matcherFeeAssetId" : null,
           |    "eip712Signature" : "${EthEncoding.toHexString(ethBuyOrder.eip712Signature.get.arr)}"
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
           |    "priceMode" : "fixedDecimals",
           |    "signature" : "",
           |    "proofs" : [ ],
           |    "matcherFeeAssetId" : null,
           |    "eip712Signature" : "${EthEncoding.toHexString(ethSellOrder.eip712Signature.get.arr)}"
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
        500000L,
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
      val invoke        = TxHelpers.invoke(TxHelpers.defaultAddress, "test")
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
      val publisher = createTxPublisherStub(blockchain)
      val route     = transactionsApiRoute.copy(blockchain = blockchain, transactionPublisher = publisher).route

      Post(routePath("/broadcast?trace=true"), invoke.json()) ~> route ~> check {
        responseAs[JsObject] should matchJson(
          s"""{
             |  "type" : 16,
             |  "id" : "${invoke.id()}",
             |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |  "senderPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
             |  "fee" : 1000000,
             |  "feeAssetId" : null,
             |  "timestamp" : ${invoke.timestamp},
             |  "proofs" : [ "${invoke.signature}" ],
             |  "version" : 1,
             |  "dApp" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
             |  "payment" : [ ],
             |  "call" : {
             |    "function" : "test",
             |    "args" : [ ]
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
             |    "invocations": [],
             |    "result" : {
             |      "data" : [ ],
             |      "transfers" : [ ],
             |      "issues" : [ ],
             |      "reissues" : [ ],
             |      "burns" : [ ],
             |      "sponsorFees" : [ ],
             |      "leases" : [
             |         {
             |           "recipient" : "${recipient1.bytes}",
             |           "amount" : $amount1,
             |           "nonce" : $nonce1,
             |           "id" : "$leaseId1"
             |         },
             |         {
             |           "recipient" : "alias:T:${recipient2.name}",
             |           "amount" : $amount2,
             |           "nonce" : $nonce2,
             |           "id" : "$leaseId2"
             |         }
             |      ],
             |      "leaseCancels" : [
             |         {
             |            "id":"$leaseCancelId"
             |         }
             |      ],
             |      "invokes" : [ ]
             |    },
             |    "error" : null,
             |    "vars" : [ {
             |      "name" : "test",
             |      "type" : "Int",
             |      "value" : 1
             |    } ]
             |  } ]
             |}""".stripMargin
        )
      }
    }
  }
}
