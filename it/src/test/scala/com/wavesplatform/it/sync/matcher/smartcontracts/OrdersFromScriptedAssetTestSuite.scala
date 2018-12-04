package com.wavesplatform.it.sync.matcher.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.{AddressScheme, PrivateKeyAccount}
import com.wavesplatform.it.api.SyncHttpApi.NodeExtSync
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync.createSignedIssueRequest
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.transaction.assets.{IssueTransactionV1, IssueTransactionV2}
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.transaction.smart.script.{Script, ScriptCompiler}

import scala.concurrent.duration._
import scala.util.Random

/**
  * Rules:
  * 1. If the script fails during placing, the matcher must reject an order
  * 2. If the script fails during execution, the matcher must cancel both orders (submitted and counter)
  */
class OrdersFromScriptedAssetTestSuite extends MatcherSuiteBase {

  import OrdersFromScriptedAssetTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  "can place if the script returns TRUE" in {
    val pair = AssetPair.createAssetPair(UnscriptedAssetId, AllowAssetId).get
    val counterId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee).message.id
    matcherNode.waitOrderStatus(pair, counterId, "Accepted")
    matcherNode.cancelOrder(matcherPk, pair, counterId)
  }

  "can't place if the script returns FALSE" in {
    val pair = AssetPair.createAssetPair(UnscriptedAssetId, DenyAssetId).get
    val order = Order.buy(
      matcherPk,
      matcherPk,
      pair,
      100000,
      2 * Order.PriceConstant,
      timestamp = System.currentTimeMillis(),
      expiration = System.currentTimeMillis() + 60 * 60 * 30,
      matcherFee = smartTradeFee,
      version = 2
    )
    matcherNode.expectIncorrectOrderPlacement(order, 400, "OrderRejected")
  }

  "if counter order has expired then cancel its only, not a submitted order" in {
    val pair = AssetPair.createAssetPair("WAVES", AllowAssetId).get

    // place expiring counter order
    val counterId =
      matcherNode
        .placeOrder(matcherPk, pair, OrderType.SELL, 2.waves, 10, version = 2, fee = smartTradeFee, timeToLive = 61.seconds)
        .message
        .id
    matcherNode.waitOrderStatus(pair, counterId, "Accepted")

    // wait counter order's lifetime
    Thread.sleep(65.seconds.toMillis)
    matcherNode.orderBook(pair).asks.size shouldBe 1

    // place a submitted order
    val submittedId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.BUY, 2.waves, 10, version = 2, fee = smartTradeFee).message.id

    matcherNode.waitOrderStatus(pair, counterId, "Cancelled")
    matcherNode.orderStatus(submittedId, pair).status shouldBe "Accepted"
  }

  "can execute against unscripted, if the script returns TRUE" in {
    info("place counter")
    val pair = AssetPair.createAssetPair(UnscriptedAssetId, AllowAssetId).get
    val counterId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee).message.id
    matcherNode.waitOrderStatus(pair, counterId, "Accepted")

    info("place a submitted order")
    val submittedId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee).message.id
    matcherNode.waitOrderStatus(pair, submittedId, "Filled")
  }

  "can execute against scripted, if both scripts returns TRUE" in {
    val allowAsset2Id = issueAsset()

    info("place a counter order")
    val pair = AssetPair.createAssetPair(allowAsset2Id, AllowAssetId).get
    val counterId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 2, fee = twoSmartTradeFee).message.id
    matcherNode.waitOrderStatus(pair, counterId, "Accepted")

    info("place a submitted order")
    val submittedId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, fee = twoSmartTradeFee).message.id

    info("both orders are cancelled")
    matcherNode.waitOrderStatus(pair, submittedId, "Filled")
    matcherNode.waitOrderStatus(pair, counterId, "Filled")
  }

  "can't execute against unscripted, if the script returns FALSE" in {
    info("place a counter order")
    val pair = AssetPair.createAssetPair(AllowAsset2Id, UnscriptedAssetId).get
    val counterId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.SELL, 100001, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee).message.id
    matcherNode.waitOrderStatus(pair, counterId, "Accepted")

    info("update a script")
    val setAssetScriptId = matcherNode.setAssetScript(AllowAsset2Id, matcherPk.address, 1.waves, Some(DenyBigAmountScript.bytes().base64)).id
    matcherNode.waitForTransaction(setAssetScriptId)

    info("a counter order wasn't rejected")
    matcherNode.orderStatus(counterId, pair).status shouldBe "Accepted"

    info("place a submitted order")
    val submittedId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee).message.id

    info("both orders are cancelled")
    matcherNode.waitOrderStatus(pair, submittedId, "Cancelled")
    matcherNode.waitOrderStatus(pair, counterId, "Cancelled")
  }

  "can't execute against scripted, if one script returns FALSE" in {
    info("place a counter order")
    val pair = AssetPair.createAssetPair(AllowAsset3Id, AllowAssetId).get
    val counterId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.SELL, 100001, 2 * Order.PriceConstant, version = 2, fee = twoSmartTradeFee).message.id
    matcherNode.waitOrderStatus(pair, counterId, "Accepted")

    info("update a script")
    val setAssetScriptId = matcherNode.setAssetScript(AllowAsset3Id, matcherPk.address, 1.waves, Some(DenyBigAmountScript.bytes().base64)).id
    matcherNode.waitForTransaction(setAssetScriptId)

    info("a counter order wasn't rejected")
    matcherNode.orderStatus(counterId, pair).status shouldBe "Accepted"

    info("place a submitted order")
    val submittedId =
      matcherNode.placeOrder(matcherPk, pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, fee = twoSmartTradeFee).message.id

    info("both orders are cancelled")
    matcherNode.waitOrderStatus(pair, submittedId, "Cancelled")
    matcherNode.waitOrderStatus(pair, counterId, "Cancelled")
  }

  private def issueAsset(): String = {
    info("issue an asset")
    val allowAsset2   = mkAllowAsset()
    val allowAsset2Id = allowAsset2.id().base58
    matcherNode.signedIssue(createSignedIssueRequest(allowAsset2))
    matcherNode.waitForTransaction(allowAsset2Id)
    allowAsset2Id
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Seq(UnscriptedAsset).map(createSignedIssueRequest).map(matcherNode.signedIssue).foreach { tx =>
      matcherNode.waitForTransaction(tx.id)
    }
    Seq(AllowAsset, AllowAsset2, AllowAsset3, DenyAsset).map(createSignedIssueRequest).map(matcherNode.signedIssue).foreach { tx =>
      matcherNode.waitForTransaction(tx.id)
    }
  }
}

object OrdersFromScriptedAssetTestSuite {

  private val matcherPk = PrivateKeyAccount.fromSeed(MatcherDefaultConfig.Configs.head.getString("account-seed")).right.get

  private val UnscriptedAsset = IssueTransactionV1
    .selfSigned(
      sender = matcherPk,
      name = "UnscriptedAsset".getBytes(),
      description = "unscripted".getBytes(),
      quantity = Int.MaxValue / 3,
      decimals = 0,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  private val UnscriptedAssetId = UnscriptedAsset.id().base58

  private def mkAllowAsset(id: Int = Random.nextInt(1000) + 1): IssueTransactionV2 = {
    IssueTransactionV2
      .selfSigned(
        2,
        AddressScheme.current.chainId,
        sender = matcherPk,
        name = s"AllowAsset-$id".getBytes(),
        description = s"AllowAsset-$id".getBytes(),
        quantity = Int.MaxValue / 3,
        decimals = 0,
        reissuable = false,
        script = Some(ScriptV1(Terms.TRUE).explicitGet()),
        fee = 1.waves,
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()
  }

  private val AllowAsset    = mkAllowAsset(0)
  private val AllowAssetId  = AllowAsset.id().base58
  private val AllowAsset2   = mkAllowAsset(1)
  private val AllowAsset2Id = AllowAsset2.id().base58
  private val AllowAsset3   = mkAllowAsset(1)
  private val AllowAsset3Id = AllowAsset3.id().base58

  private val DenyAsset = IssueTransactionV2
    .selfSigned(
      2,
      AddressScheme.current.chainId,
      sender = matcherPk,
      name = "DenyAsset".getBytes(),
      description = "DenyAsset".getBytes(),
      quantity = Int.MaxValue / 3,
      decimals = 0,
      reissuable = false,
      script = Some(ScriptV1(Terms.FALSE).explicitGet()),
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  private val DenyAssetId = DenyAsset.id().base58

  private val DenyBigAmountScript: Script = {
    val scriptText = s"""
                        |{-# LANGUAGE_VERSION 2 #-}
                        |match tx {
                        | case tx: ExchangeTransaction => tx.sellOrder.amount <= 100000
                        | case other => true
                        |}
                        |""".stripMargin
    ScriptCompiler(scriptText, isAssetScript = true).explicitGet()._1
  }

  private val commonConfig = ConfigFactory.parseString(s"""
                                                           |waves {
                                                           |  blockchain.custom.functionality.pre-activated-features = { 9 = 0 }
                                                           |  matcher.price-assets = ["$AllowAssetId", "$DenyAssetId", "$UnscriptedAssetId"]
                                                           |}
                                                           |waves.matcher {
                                                           |  order-cleanup-interval = 5m
                                                           |}""".stripMargin)

  private val Configs = MatcherDefaultConfig.Configs.map(commonConfig.withFallback(_))

}
