package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

class TradersTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode    = nodes.head
  private def aliceNode      = nodes(1)
  private def bobNode        = nodes(2)
  private val TransactionFee = 300000

  "Verifications of tricky ordering cases" ignore {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, issueFee).id
    matcherNode.waitForTransaction(aliceAsset)

    // val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    matcherNode.assertAssetBalance(aliceNode.address, aliceAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)
    matcherNode.assertAssetBalance(bobNode.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset      = bobNode.issue(bobNode.address, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, false, issueFee).id
    matcherNode.waitForTransaction(bobNewAsset)
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

    matcherNode.assertAssetBalance(bobNode.address, bobNewAsset, bobAssetQuantity)

    "owner moves assets/waves to another account and order become an invalid" ignore {
      // todo: reactivate after balance watcher is reimplemented
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(8000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          val transferId = bobNode.transfer(bobNode.address, aliceNode.address, 3050, TransactionFee, Some(bobNewAsset), None).id
          matcherNode.waitForTransaction(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobNode, twoAssetsPair, newestOrderId).status should be("OrderCanceled")

          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, 3050, TransactionFee, Some(bobNewAsset), None).id
          matcherNode.waitForTransaction(transferBackId)
        }

        "leased waves, insufficient fee" in {
          val bobBalance    = bobNode.accountBalances(bobNode.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - TransactionFee - matcherFee
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Accepted")
          }

          // Cleanup
          matcherNode.cancelOrder(bobNode, twoAssetsPair, newestOrderId)
          matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "moved waves, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobNode.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - TransactionFee - matcherFee
          val transferId     = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
          matcherNode.waitForTransaction(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Accepted")
          }

          // Cleanup
          matcherNode.cancelOrder(bobNode, twoAssetsPair, newestOrderId)
          matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")

          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
          matcherNode.waitForTransaction(transferBackId)
        }
      }

      "order with waves" - {
        "leased waves, insufficient fee for one ExchangeTransaction" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobNode.address)._1

          val oldestOrderId = bobPlacesWaveOrder(bobWavesPair, 1, 10.waves * Order.PriceConstant)
          val newestOrderId = bobPlacesWaveOrder(bobWavesPair, 1, 10.waves * Order.PriceConstant)

          //      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - TransactionFee - 10.waves - matcherFee
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The newest order '$oldestOrderId' is Cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Accepted")
          }

          // Cleanup
          matcherNode.cancelOrder(bobNode, bobWavesPair, newestOrderId)
          matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")

          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "leased waves, insufficient waves" in {
          val bobBalance = bobNode.accountBalances(bobNode.address)._1
          val price      = 1.waves
          val order2     = bobPlacesWaveOrder(bobWavesPair, 1, price * Order.PriceConstant)

          val leaseAmount = bobBalance - TransactionFee - price / 2
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The order '$order2' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, order2, "Cancelled")
          }

          // Cleanup
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "moved waves, insufficient fee" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobNode.address)._1
          val price      = TransactionFee / 2
          val order3     = bobPlacesWaveOrder(bobWavesPair, 1, price * Order.PriceConstant)

          val transferAmount = bobBalance - TransactionFee - price
          val txId           = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
          matcherNode.waitForTransaction(txId)

          withClue(s"The order '$order3' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, order3, "Cancelled")
          }

          // Cleanup
          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
          matcherNode.waitForTransaction(transferBackId)
        }

      }
    }
  }

  def bobPlacesWaveOrder(assetPair: AssetPair, amount: Long, price: Price): String = {
    val bobOrder = matcherNode.prepareOrder(bobNode, assetPair, OrderType.BUY, amount, price)
    val order    = matcherNode.placeOrder(bobOrder).message.id
    matcherNode.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = ByteStr.decodeBase58(assetId).get
    val bobOrder = if (twoAssetsPair.amountAsset.contains(decodedAsset)) {
      matcherNode.prepareOrder(bobNode, twoAssetsPair, OrderType.SELL, bobCoinAmount, 1 * Order.PriceConstant)
    } else {
      matcherNode.prepareOrder(bobNode, twoAssetsPair, OrderType.BUY, 1, bobCoinAmount * Order.PriceConstant)
    }
    val order = matcherNode.placeOrder(bobOrder)
    matcherNode.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}
