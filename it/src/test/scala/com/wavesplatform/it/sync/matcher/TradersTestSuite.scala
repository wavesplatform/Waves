package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scala.util.Random
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._

class TradersTestSuite extends MatcherSuiteBase {
  private val exTxFee                             = 300000
  private def orderVersion                        = (Random.nextInt(2) + 1).toByte
  override protected def nodeConfigs: Seq[Config] = Configs

  "Verifications of tricky ordering cases" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, issueFee, 2).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    // val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    aliceNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)
    bobNode.assertAssetBalance(bobAcc.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset      = bobNode.issue(bobAcc.address, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, false, issueFee, 2).id
    nodes.waitForHeightAriseAndTxPresent(bobNewAsset)
    val bobAssetId   = ByteStr.decodeBase58(bobNewAsset).get
    val aliceAssetId = ByteStr.decodeBase58(aliceAsset).get

    val bobWavesPair = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = None
    )

    val twoAssetsPair =
      if (MatcherActor.compare(Some(bobAssetId.arr), Some(aliceAssetId.arr)) < 0)
        AssetPair(
          amountAsset = Some(aliceAssetId),
          priceAsset = Some(bobAssetId)
        )
      else
        AssetPair(
          amountAsset = Some(bobAssetId),
          priceAsset = Some(aliceAssetId)
        )

    nodes.waitForHeightArise()
    bobNode.assertAssetBalance(bobAcc.address, bobNewAsset, bobAssetQuantity)

    "matcher should respond with Public key" in {
      matcherNode.matcherGet("/matcher").getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
    }

    "owner moves assets/waves to another account and order become an invalid" ignore {
      // todo: reactivate after balance watcher is reimplemented
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(8000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          val transferId = bobNode.transfer(bobAcc.address, aliceAcc.address, 3050, exTxFee, Some(bobNewAsset), None, 2).id
          nodes.waitForHeightAriseAndTxPresent(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, Some(newestOrderId)).status should be("OrderCanceled")

          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, 3050, exTxFee, Some(bobNewAsset), None, 2).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }

        "leased waves, insufficient fee" in {
          val bobBalance    = bobNode.accountBalances(bobAcc.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - exTxFee - matcherFee
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, Some(newestOrderId)).status should be("OrderCanceled")
          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "moved waves, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobAcc.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - exTxFee - matcherFee
          val transferId     = bobNode.transfer(bobAcc.address, aliceAcc.address, transferAmount, exTxFee, None, None, 2).id
          nodes.waitForHeightAriseAndTxPresent(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, Some(newestOrderId)).status should be("OrderCanceled")
          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, transferAmount, exTxFee, None, None, 2).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }
      }

      "order with waves" - {
        "leased waves, insufficient fee for one ExchangeTransaction" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobAcc.address)._1

          val oldestOrderId = bobPlacesWaveOrder(bobWavesPair, 10.waves * Order.PriceConstant, 1)
          val newestOrderId = bobPlacesWaveOrder(bobWavesPair, 10.waves * Order.PriceConstant, 1)

          //      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - exTxFee - 10.waves - matcherFee
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The newest order '$oldestOrderId' is Cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobAcc, bobWavesPair, Some(newestOrderId)).status should be("OrderCanceled")
          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "leased waves, insufficient waves" in {
          val bobBalance = bobNode.accountBalances(bobAcc.address)._1
          val price      = 1.waves
          val order2     = bobPlacesWaveOrder(bobWavesPair, price * Order.PriceConstant, 1)

          val leaseAmount = bobBalance - exTxFee - price / 2
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The order '$order2' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, order2, "Cancelled")
          }

          // Cleanup
          nodes.waitForHeightArise()
          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "moved waves, insufficient fee" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobAcc.address)._1
          val price      = exTxFee / 2
          val order3     = bobPlacesWaveOrder(bobWavesPair, price * Order.PriceConstant, 1)

          val transferAmount = bobBalance - exTxFee - price
          val txId           = bobNode.transfer(bobAcc.address, aliceAcc.address, transferAmount, exTxFee, None, None, 2).id
          nodes.waitForHeightAriseAndTxPresent(txId)

          withClue(s"The order '$order3' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, order3, "Cancelled")
          }

          // Cleanup
          nodes.waitForHeightArise()
          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, transferAmount, exTxFee, None, None, 2).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }

      }
    }
  }

  def bobPlacesWaveOrder(assetPair: AssetPair, price: Price, amount: Long): String = {
    val bobOrder = matcherNode.prepareOrder(bobAcc, assetPair, OrderType.BUY, price, amount, orderVersion)
    val order    = matcherNode.placeOrder(bobOrder).message.id
    matcherNode.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = ByteStr.decodeBase58(assetId).get
    val bobOrder = if (twoAssetsPair.amountAsset.contains(decodedAsset)) {
      matcherNode.prepareOrder(bobAcc, twoAssetsPair, OrderType.SELL, 1 * Order.PriceConstant, bobCoinAmount, orderVersion)
    } else {
      matcherNode.prepareOrder(bobAcc, twoAssetsPair, OrderType.BUY, bobCoinAmount * Order.PriceConstant, 1, orderVersion)
    }
    val order = matcherNode.placeOrder(bobOrder)
    matcherNode.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}
