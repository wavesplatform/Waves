package com.wavesplatform.http

import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, NotFound}
import akka.http.scaladsl.model.{FormData, HttpEntity}
import com.wavesplatform.BlockGen
import com.wavesplatform.api.http.{RouteTimeout, TransactionsApiRoute}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V8
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.TransactionStateSnapshot
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.utils.{EthHelpers, SharedSchedulerMixin}
import org.scalatest.OptionValues
import play.api.libs.json.*
import play.api.libs.json.Json.JsValueWrapper

import scala.concurrent.Future
import scala.concurrent.duration.*

class TransactionSnapshotsRouteSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with BlockGen
    with OptionValues
    with SharedDomain
    with EthHelpers
    with SharedSchedulerMixin {

  override def settings        = TransactionStateSnapshot
  override def genesisBalances = AddrWithBalance.enoughBalances(defaultSigner, secondSigner)

  private val transactionsApiRoute = new TransactionsApiRoute(
    settings.restAPISettings,
    domain.transactionsApi,
    domain.wallet,
    domain.blockchain,
    () => domain.blockchain,
    () => domain.utxPool.size,
    (tx, _) => Future.successful(domain.utxPool.putIfNew(tx, forceValidate = true)),
    new TestTime,
    new RouteTimeout(60.seconds)(sharedScheduler)
  )
  private val route = seal(transactionsApiRoute.route)

  private def multipleJson(txs: Seq[Transaction]) =
    Post(
      routePath("/snapshot"),
      HttpEntity(`application/json`, Json.obj("ids" -> Json.arr(txs.map(_.id().toString: JsValueWrapper)*)).toString())
    )
  private def multipleFormData(txs: Seq[Transaction]) =
    Post(routePath("/snapshot"), FormData(txs.map("id" -> _.id().toString)*))

  routePath("/snapshot/{id}") - {
    "all snapshot fields" in {
      val script = TestCompiler(V8).compileContract(
        """
          | @Callable(i)
          | func default() = {
          |   let issue = Issue("aaaa", "bbbb", 1000, 4, true, unit, 0)
          |   let lease = Lease(i.caller, 123)
          |   [
          |     lease,
          |     LeaseCancel(lease.calculateLeaseId()),
          |     issue,
          |     SponsorFee(issue.calculateAssetId(), 1000),
          |     IntegerEntry("int", 777),
          |     BinaryEntry("bytes", base64'abc'),
          |     BooleanEntry("bool", true),
          |     StringEntry("str", "text"),
          |     DeleteEntry("delete")
          |   ]
          | }
        """.stripMargin
      )
      val setScriptTx    = setScript(secondSigner, script)
      val invokeTx       = invoke(fee = 100500000)
      val removeScriptTx = removeScript(secondSigner)
      val issueTx        = issue(script = Some(TestCompiler(V8).compileExpression("true")))
      val asset          = IssuedAsset(issueTx.id())
      val aliasTx        = createAlias()
      val order1         = order(BUY, asset, Waves, amount = 123, price = 40_000_000, fee = 777)
      val order2         = order(SELL, asset, Waves, amount = 123, price = 40_000_000, fee = 888)
      val exchangeTx     = exchange(order1, order2, amount = 123, price = 40_000_000, buyMatcherFee = 777, sellMatcherFee = 888)
      val allTxs         = Seq(setScriptTx, invokeTx, issueTx, aliasTx, removeScriptTx, exchangeTx)

      domain.appendBlock(allTxs*)
      val invokeAsset = domain.liquidSnapshot.assetStatics.head._1
      val leaseId     = domain.liquidSnapshot.newLeases.head._1

      val setScriptJson = Json.parse(
        s"""
           | {
           |  "applicationStatus": "succeeded",
           |  "balances": [
           |    {
           |      "address": "$secondAddress",
           |      "asset": null,
           |      "balance": ${ENOUGH_AMT - setScriptTx.fee.value}
           |    },
           |    {
           |      "address": "$defaultAddress",
           |      "asset": null,
           |      "balance": ${ENOUGH_AMT + 200_000_000 + setScriptTx.fee.value * 2 / 5}
           |    }
           |  ],
           |  "leaseBalances": [],
           |  "assetStatics": [],
           |  "assetVolumes": [],
           |  "assetNamesAndDescriptions": [],
           |  "assetScripts": [],
           |  "sponsorships": [],
           |  "newLeases": [],
           |  "cancelledLeases": [],
           |  "aliases": [],
           |  "orderFills": [],
           |  "accountScripts": [
           |    {
           |      "publicKey": "${secondSigner.publicKey}",
           |      "script": "${script.bytes().base64}",
           |      "verifierComplexity": 0
           |    }
           |  ],
           |  "accountData": []
           | }
         """.stripMargin
      )
      Get(routePath(s"/snapshot/${setScriptTx.id()}")) ~> route ~> check(
        responseAs[JsObject] shouldBe setScriptJson
      )

      val invokeJson = Json.parse(
        s"""
           | {
           |   "applicationStatus": "succeeded",
           |   "balances": [
           |     {
           |       "address": "$defaultAddress",
           |       "asset": null,
           |       "balance": ${ENOUGH_AMT + 200_000_000 + setScriptTx.fee.value * 2 / 5 - invokeTx.fee.value * 3 / 5}
           |     },
           |     {
           |       "address": "$secondAddress",
           |       "asset": "$invokeAsset",
           |       "balance": 1000
           |     }
           |   ],
           |   "leaseBalances": [
           |     {
           |       "address": "$secondAddress",
           |       "in": 0,
           |       "out": 0
           |     },
           |     {
           |       "address": "$defaultAddress",
           |       "in": 0,
           |       "out": 0
           |     }
           |   ],
           |   "assetStatics": [
           |     {
           |       "id": "$invokeAsset",
           |       "source": "${invokeTx.id()}",
           |       "issuer": "${secondSigner.publicKey}",
           |       "decimals": 4,
           |       "nft": false
           |     }
           |   ],
           |   "assetVolumes": [
           |     {
           |       "id": "$invokeAsset",
           |       "isReissuable": true,
           |       "volume": 1000
           |     }
           |   ],
           |   "assetNamesAndDescriptions": [
           |     {
           |       "id": "$invokeAsset",
           |       "name": "aaaa",
           |       "description": "bbbb",
           |       "lastUpdatedAt": 2
           |     }
           |   ],
           |   "assetScripts": [],
           |   "sponsorships": [
           |     {
           |       "id": "$invokeAsset",
           |       "minSponsoredAssetFee": 1000
           |     }
           |   ],
           |   "newLeases": [
           |     {
           |       "id": "$leaseId",
           |       "sender": "${secondSigner.publicKey}",
           |       "recipient": "$defaultAddress",
           |       "amount": 123,
           |       "txId": "${invokeTx.id()}",
           |       "height": 2
           |     }
           |   ],
           |   "cancelledLeases": [
           |     {
           |       "id": "$leaseId",
           |       "txId": "${invokeTx.id()}",
           |       "height": 2
           |     }
           |   ],
           |   "aliases": [],
           |   "orderFills": [],
           |   "accountScripts": [],
           |   "accountData": [
           |     {
           |       "address": "$secondAddress",
           |       "data": [
           |         {
           |           "key": "bool",
           |           "type": "boolean",
           |           "value": true
           |         },
           |         {
           |           "key": "str",
           |           "type": "string",
           |           "value": "text"
           |         },
           |         {
           |           "key": "bytes",
           |           "type": "binary",
           |           "value": "base64:abc="
           |         },
           |         {
           |           "key": "int",
           |           "type": "integer",
           |           "value": 777
           |         },
           |         {
           |           "key": "delete",
           |           "value": null
           |         }
           |       ]
           |     }
           |   ]
           | }
         """.stripMargin
      )
      Get(routePath(s"/snapshot/${invokeTx.id()}")) ~> route ~> check(
        responseAs[JsObject] shouldBe invokeJson
      )

      val issueJson = Json.parse(
        s"""
           | {
           |  "applicationStatus": "succeeded",
           |  "balances" : [ {
           |    "address" : "$defaultAddress",
           |    "asset" : "$asset",
           |    "balance" : ${Long.MaxValue / 100}
           |  }, {
           |    "address" : "$defaultAddress",
           |    "asset" : null,
           |    "balance" : ${ENOUGH_AMT + 200_000_000 + setScriptTx.fee.value * 2 / 5 - (invokeTx.fee.value + issueTx.fee.value) * 3 / 5}
           |  } ],
           |  "leaseBalances" : [ ],
           |  "assetStatics" : [ {
           |    "id" : "$asset",
           |    "source" : "${issueTx.id()}",
           |    "issuer" : "${defaultSigner.publicKey}",
           |    "decimals" : 0,
           |    "nft" : false
           |  } ],
           |  "assetVolumes" : [ {
           |    "id": "$asset",
           |    "isReissuable": true,
           |    "volume": 92233720368547758
           |  } ],
           |  "assetNamesAndDescriptions" : [ {
           |    "id": "$asset",
           |    "name" : "test",
           |    "description" : "description",
           |    "lastUpdatedAt" : 2
           |  } ],
           |  "assetScripts" : [ {
           |    "id": "$asset",
           |    "script" : "base64:CAEG32nosg==",
           |    "complexity" : 0
           |  } ],
           |  "sponsorships" : [ ],
           |  "newLeases" : [ ],
           |  "cancelledLeases" : [ ],
           |  "aliases" : [ ],
           |  "orderFills" : [ ],
           |  "accountScripts" : [ ],
           |  "accountData" : [ ]
           | }
         """.stripMargin
      )
      Get(routePath(s"/snapshot/${issueTx.id()}")) ~> route ~> check(
        responseAs[JsObject] shouldBe issueJson
      )

      val aliasJson = Json.parse(
        s"""
           | {
           |  "applicationStatus": "succeeded",
           |  "balances" : [ {
           |    "address" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
           |    "asset" : null,
           |    "balance" : ${ENOUGH_AMT + 200_000_000 + setScriptTx.fee.value * 2 / 5 - (invokeTx.fee.value + issueTx.fee.value + aliasTx.fee.value) * 3 / 5}
           |  } ],
           |  "leaseBalances" : [ ],
           |  "assetStatics" : [ ],
           |  "assetVolumes" : [ ],
           |  "assetNamesAndDescriptions" : [ ],
           |  "assetScripts" : [ ],
           |  "sponsorships" : [ ],
           |  "newLeases" : [ ],
           |  "cancelledLeases" : [ ],
           |  "aliases" : [ { "address": "$defaultAddress", "alias": "alias" } ],
           |  "orderFills" : [ ],
           |  "accountScripts" : [ ],
           |  "accountData" : [ ]
           | }
         """.stripMargin
      )
      Get(routePath(s"/snapshot/${aliasTx.id()}")) ~> route ~> check(responseAs[JsObject]) shouldBe aliasJson

      val removeScriptJson = Json.parse(
        s"""
           | {
           |  "applicationStatus": "succeeded",
           |  "balances": [
           |    {
           |      "address": "$secondAddress",
           |      "asset": null,
           |      "balance": ${ENOUGH_AMT - setScriptTx.fee.value - removeScriptTx.fee.value}
           |    },
           |    {
           |      "address": "$defaultAddress",
           |      "asset": null,
           |       "balance": ${ENOUGH_AMT + 200_000_000 + (setScriptTx.fee.value + removeScriptTx.fee.value) * 2 / 5 - (invokeTx.fee.value + issueTx.fee.value + aliasTx.fee.value) * 3 / 5}
           |    }
           |  ],
           |  "leaseBalances": [],
           |  "assetStatics": [],
           |  "assetVolumes": [],
           |  "assetNamesAndDescriptions": [],
           |  "assetScripts": [],
           |  "sponsorships": [],
           |  "newLeases": [],
           |  "cancelledLeases": [],
           |  "aliases": [],
           |  "orderFills": [],
           |  "accountScripts": [
           |    {
           |      "publicKey": "${secondSigner.publicKey}",
           |      "script": null,
           |      "verifierComplexity": 0
           |    }
           |  ],
           |  "accountData": []
           | }
         """.stripMargin
      )
      Get(routePath(s"/snapshot/${removeScriptTx.id()}")) ~> route ~> check(
        responseAs[JsObject] shouldBe removeScriptJson
      )

      val exchangeJson = Json.parse(
        s"""
           | {
           |  "applicationStatus": "succeeded",
           |  "balances": [
           |    {
           |      "address": "$defaultAddress",
           |      "asset": null,
           |      "balance": ${ENOUGH_AMT + 200_000_000 + (setScriptTx.fee.value + removeScriptTx.fee.value) * 2 / 5 - (invokeTx.fee.value + issueTx.fee.value + aliasTx.fee.value + exchangeTx.fee.value) * 3 / 5}
           |    }
           |  ],
           |  "leaseBalances": [],
           |  "assetStatics": [],
           |  "assetVolumes": [],
           |  "assetNamesAndDescriptions": [],
           |  "assetScripts": [],
           |  "sponsorships": [],
           |  "newLeases": [],
           |  "cancelledLeases": [],
           |  "aliases": [],
           |  "orderFills": [
           |    {
           |      "id": "${exchangeTx.order1.id()}",
           |      "volume": 123,
           |      "fee": 777
           |    },
           |    {
           |      "id": "${exchangeTx.order2.id()}",
           |      "volume": 123,
           |      "fee": 888
           |    }
           |  ],
           |  "accountScripts": [],
           |  "accountData": []
           | }
         """.stripMargin
      )
      Get(routePath(s"/snapshot/${exchangeTx.id()}")) ~> route ~> check(
        responseAs[JsObject] shouldBe exchangeJson
      )

      val allSnapshotsJson = JsArray(Seq(setScriptJson, invokeJson, issueJson, aliasJson, removeScriptJson, exchangeJson))
      multipleJson(allTxs) ~> route ~> check(responseAs[JsArray] shouldBe allSnapshotsJson)
      multipleFormData(allTxs) ~> route ~> check(responseAs[JsArray] shouldBe allSnapshotsJson)
    }

    "multiple snapshots limit" in {
      val transfers = (1 to 101).map(_ => transfer())
      domain.appendBlock(transfers*)
      multipleJson(transfers.drop(1)) ~> route ~> check(responseAs[JsArray].value.size shouldBe 100)
      multipleFormData(transfers.drop(1)) ~> route ~> check(responseAs[JsArray].value.size shouldBe 100)

      multipleJson(transfers) ~> route ~> check {
        status shouldEqual BadRequest
        (responseAs[JsObject] \ "message").as[String] shouldBe "Too big sequence requested: max limit is 100 entries"
      }
      multipleFormData(transfers) ~> route ~> check {
        status shouldEqual BadRequest
        (responseAs[JsObject] \ "message").as[String] shouldBe "Too big sequence requested: max limit is 100 entries"
      }
    }

    "unexisting id" in {
      val tx = transfer()
      Get(routePath(s"/snapshot/${tx.id()}")) ~> route ~> check {
        status shouldEqual NotFound
        (responseAs[JsObject] \ "message").as[String] shouldBe "transactions does not exist"
      }
      multipleJson(Seq(tx)) ~> route ~> check {
        status shouldEqual NotFound
        (responseAs[JsObject] \ "message").as[String] shouldBe "transactions does not exist"
      }
      multipleFormData(Seq(tx)) ~> route ~> check {
        status shouldEqual NotFound
        (responseAs[JsObject] \ "message").as[String] shouldBe "transactions does not exist"
      }
    }
  }
}
