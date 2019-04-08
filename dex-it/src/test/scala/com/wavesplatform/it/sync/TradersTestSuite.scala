package com.wavesplatform.it.sync

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.util.Random

class TradersTestSuite extends MatcherSuiteBase {

  private def orderVersion = (Random.nextInt(2) + 1).toByte // TODO OrderV3 support

  "Verifications of tricky ordering cases" - {
    // Alice issues new asset
    val aliceAsset = node
      .broadcastIssue(alice,
                      "AliceCoin",
                      "AliceCoin for matcher's tests",
                      someAssetAmount,
                      0,
                      reissuable = false,
                      smartIssueFee,
                      None,
                      waitForTx = true)
      .id

    // Wait for balance on Alice's account
    node.assertAssetBalance(alice.address, aliceAsset, someAssetAmount)
    node.assertAssetBalance(matcher.address, aliceAsset, 0)
    node.assertAssetBalance(bob.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset =
      node.broadcastIssue(bob, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, reissuable = false, smartIssueFee, None, waitForTx = true).id

    val bobAssetId   = IssuedAsset(ByteStr.decodeBase58(bobNewAsset).get)
    val aliceAssetId = IssuedAsset(ByteStr.decodeBase58(aliceAsset).get)

    val bobWavesPair = AssetPair(
      amountAsset = bobAssetId,
      priceAsset = Waves
    )

    val twoAssetsPair =
      if (MatcherActor.compare(Some(bobAssetId.id.arr), Some(aliceAssetId.id.arr)) < 0)
        AssetPair(
          amountAsset = aliceAssetId,
          priceAsset = bobAssetId
        )
      else
        AssetPair(
          amountAsset = bobAssetId,
          priceAsset = aliceAssetId
        )

    node.assertAssetBalance(bob.address, bobNewAsset, bobAssetQuantity)

    "owner moves assets/waves to another account and order become an invalid" - {
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)

          // 5000 waves are rest
          node.broadcastTransfer(bob, alice.address, 5000, matcherFee, Some(bobNewAsset), None, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            node.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobWavesPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, twoAssetsPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")
          node.broadcastTransfer(alice, bob.address, 5000, matcherFee, Some(bobNewAsset), None, waitForTx = true).id
        }

        "leased waves, insufficient fee" in {
          val bobBalance    = node.accountBalances(bob.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - matcherFee - matcherFee
          val leaseId     = node.broadcastLease(bob, alice.address, leaseAmount, matcherFee, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            node.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobWavesPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, twoAssetsPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          node.broadcastCancelLease(bob, leaseId, matcherFee, waitForTx = true).id
        }

        "moved waves, insufficient fee" in {
          val bobBalance    = node.accountBalances(bob.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - matcherFee - matcherFee
          node.broadcastTransfer(bob, alice.address, transferAmount, matcherFee, None, None, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            node.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobWavesPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, twoAssetsPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")
          node.broadcastTransfer(alice, bob.address, transferAmount, matcherFee, None, None, waitForTx = true).id
        }
      }

      "order with waves" - {
        "leased waves, insufficient fee for one ExchangeTransaction" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = node.accountBalances(bob.address)._1

          val oldestOrderId = bobPlacesWaveOrder(bobWavesPair, 1, 10.waves * Order.PriceConstant)
          val newestOrderId = bobPlacesWaveOrder(bobWavesPair, 1, 10.waves * Order.PriceConstant)

          //      waitForOrderStatus(node, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - matcherFee - 10.waves - matcherFee
          val leaseId     = node.broadcastLease(bob, alice.address, leaseAmount, matcherFee, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' is Cancelled") {
            node.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobWavesPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, bobWavesPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          node.broadcastCancelLease(bob, leaseId, matcherFee, waitForTx = true)
        }

        "leased waves, insufficient waves" in {
          val bobBalance = node.accountBalances(bob.address)._1
          val price      = 1.waves
          val order2     = bobPlacesWaveOrder(bobWavesPair, 1, price * Order.PriceConstant)

          val leaseAmount = bobBalance - matcherFee - price / 2
          val leaseId     = node.broadcastLease(bob, alice.address, leaseAmount, matcherFee, waitForTx = true).id

          withClue(s"The order '$order2' was cancelled") {
            node.waitOrderStatus(bobWavesPair, order2, "Cancelled")
          }

          // Cleanup
          node.broadcastCancelLease(bob, leaseId, matcherFee, waitForTx = true)
        }

        "moved waves, insufficient fee" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = node.accountBalances(bob.address)._1
          val price      = matcherFee / 2
          val order3     = bobPlacesWaveOrder(bobWavesPair, 1, price * Order.PriceConstant)

          val transferAmount = bobBalance - matcherFee - price
          node.broadcastTransfer(bob, alice.address, transferAmount, matcherFee, None, None, waitForTx = true).id

          withClue(s"The order '$order3' was cancelled") {
            node.waitOrderStatus(bobWavesPair, order3, "Cancelled")
          }

          // Cleanup
          node.broadcastTransfer(alice, bob.address, transferAmount, matcherFee, None, None, waitForTx = true).id
        }
      }
    }
  }

  def bobPlacesWaveOrder(assetPair: AssetPair, amount: Long, price: Price): String = {
    val bobOrder = node.prepareOrder(bob, assetPair, OrderType.BUY, amount, price)
    val order    = node.placeOrder(bobOrder).message.id
    node.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = IssuedAsset(ByteStr.decodeBase58(assetId).get)
    val bobOrder = if (twoAssetsPair.amountAsset == decodedAsset) {
      node.prepareOrder(bob, twoAssetsPair, OrderType.SELL, bobCoinAmount, 1 * Order.PriceConstant, matcherFee, orderVersion)
    } else {
      node.prepareOrder(bob, twoAssetsPair, OrderType.BUY, 1, bobCoinAmount * Order.PriceConstant, matcherFee, orderVersion)
    }
    val order = node.placeOrder(bobOrder)
    node.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}
