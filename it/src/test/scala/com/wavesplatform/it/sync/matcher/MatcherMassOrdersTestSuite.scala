package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class MatcherMassOrdersTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs

  private def orderVersion = (Random.nextInt(2) + 1).toByte

  "Create orders with statuses FILL, PARTIAL, CANCELLED, ACTIVE" - {
    // Alice issues new assets
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2).id

    val aliceSecondAsset = aliceNode
      .issue(aliceAcc.address, "AliceSecondCoin", "AliceSecondCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2)
      .id
    Seq(aliceAsset, aliceSecondAsset).foreach(matcherNode.waitForTransaction(_))

    val aliceWavesPair       = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Waves)
    val aliceSecondWavesPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceSecondAsset).get), Waves)

    // Check balances on Alice's account
    aliceNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
    aliceNode.assertAssetBalance(aliceAcc.address, aliceSecondAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)

    val transfer1ToBobId = aliceNode.transfer(aliceAcc.address, bobAcc.address, someAssetAmount / 2, minFee, Some(aliceAsset), None, 2).id
    matcherNode.waitForTransaction(transfer1ToBobId)

    val transfer2ToBobId = aliceNode.transfer(aliceAcc.address, bobAcc.address, someAssetAmount / 2, minFee, Some(aliceSecondAsset), None, 2).id
    matcherNode.waitForTransaction(transfer2ToBobId)

    matcherNode.assertAssetBalance(bobAcc.address, aliceAsset, someAssetAmount / 2)
    matcherNode.assertAssetBalance(bobAcc.address, aliceSecondAsset, someAssetAmount / 2)

    // Alice places sell orders
    val aliceOrderIdFill = matcherNode
      .placeOrder(aliceAcc, aliceSecondWavesPair, OrderType.SELL, 3, Order.PriceConstant, matcherFee, orderVersion, 10.minutes)
      .message
      .id

    val alicePartialOrderId = matcherNode
      .placeOrder(aliceAcc, aliceSecondWavesPair, OrderType.SELL, 3, Order.PriceConstant, matcherFee, orderVersion, 10.minutes)
      .message
      .id

    val aliceOrderToCancelId =
      matcherNode
        .placeOrder(aliceAcc, aliceSecondWavesPair, OrderType.SELL, 3, Order.PriceConstant, matcherFee, orderVersion, 70.seconds)
        .message
        .id

    val aliceActiveOrderId = matcherNode
      .placeOrder(aliceAcc, aliceSecondWavesPair, OrderType.SELL, 3, Order.PriceConstant + 100000000, matcherFee, orderVersion, 10.minutes)
      .message
      .id

    matcherNode.cancelOrder(aliceAcc, aliceSecondWavesPair, aliceOrderToCancelId) // TODO: remove this line in DEX-160
    matcherNode.waitOrderStatus(aliceSecondWavesPair, aliceOrderToCancelId, "Cancelled", 2.minutes)

    //Bob orders should partially fill one Alice order and fill another
    ordersRequestsGen(2, bobAcc, aliceSecondWavesPair, OrderType.BUY, 2)

    //check orders after filling
    matcherNode.waitOrderStatus(aliceSecondWavesPair, alicePartialOrderId, "PartiallyFilled")

    orderStatus(aliceAcc, aliceSecondWavesPair, aliceOrderIdFill, "Filled")
    orderStatus(aliceAcc, aliceSecondWavesPair, alicePartialOrderId, "PartiallyFilled")

    "Mass orders creation with random lifetime. Active orders still in list" in {
      matcherNode.ordersByAddress(aliceAcc, activeOnly = false).length shouldBe 4
      matcherNode.ordersByAddress(aliceAcc, activeOnly = true).length shouldBe 2

      matcherNode.ordersByAddress(bobAcc, activeOnly = false).length shouldBe 2
      matcherNode.ordersByAddress(bobAcc, activeOnly = true).length shouldBe 0

      val orderIds = matcherNode.fullOrderHistory(aliceAcc).map(_.id)

      orderIds should contain(aliceActiveOrderId)

      ordersRequestsGen(orderLimit + 1, aliceAcc, aliceWavesPair, OrderType.SELL, 3)

      //wait for some orders cancelled
      Thread.sleep(5000)
      val bobsOrderIds = ordersRequestsGen(orderLimit + 1, bobAcc, aliceWavesPair, OrderType.BUY, 2)
      Thread.sleep(5000)

      // Alice check that order Active order is still in list
      val orderIdsAfterMatching = matcherNode.fullOrderHistory(aliceAcc).map(_.id)

      orderIdsAfterMatching should contain(aliceActiveOrderId)
      orderIdsAfterMatching should contain(alicePartialOrderId)

      matcherNode.waitOrderStatus(aliceSecondWavesPair, aliceActiveOrderId, "Accepted")
      matcherNode.waitOrderStatus(aliceSecondWavesPair, alicePartialOrderId, "PartiallyFilled")

      matcherNode.fullOrderHistory(bobAcc).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
      matcherNode.orderHistoryByPair(bobAcc, aliceWavesPair).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
    }

    "Filled and Cancelled orders should be after Partial And Accepted" in {
      val lastIdxOfActiveOrder =
        matcherNode.fullOrderHistory(aliceAcc).lastIndexWhere(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled"))
      val firstIdxOfClosedOrder = matcherNode.fullOrderHistory(aliceAcc).indexWhere(o => o.status.equals("Filled") || o.status.equals("Cancelled"))
      lastIdxOfActiveOrder should be < firstIdxOfClosedOrder
    }

    "Accepted and PartiallyFilled orders should be sorted by timestamp." in {
      val activeAndPartialOrders =
        matcherNode.fullOrderHistory(aliceAcc).filter(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled")).map(_.timestamp)
      activeAndPartialOrders.reverse shouldBe sorted
    }

    "Filled and Cancelled orders should be sorted by timestamp." in {
      val filledAndCancelledOrders =
        matcherNode.fullOrderHistory(aliceAcc).filter(o => o.status.equals("Filled") || o.status.equals("Cancelled")).map(_.timestamp)
      filledAndCancelledOrders.reverse shouldBe sorted
    }

    "check order history orders count after fill" in {
      val aliceOrderHistory = matcherNode.fullOrderHistory(aliceAcc)
      aliceOrderHistory.size shouldBe orderLimit
      val aliceOrderHistoryByPair = matcherNode.orderHistoryByPair(aliceAcc, aliceWavesPair)
      aliceOrderHistoryByPair.size shouldBe orderLimit
    }

  }

  private def ordersRequestsGen(n: Int, sender: PrivateKeyAccount, assetPair: AssetPair, orderType: OrderType, amount: Long): Seq[String] = {
    val orderIds = 1 to n map (_ => {
      matcherNode
        .placeOrder(sender, assetPair, orderType, amount, Order.PriceConstant, matcherFee, orderVersion, (120 + Random.nextInt(70)).seconds)
        .message
        .id
    })
    orderIds
  }

  private def orderStatus(sender: PrivateKeyAccount, assetPair: AssetPair, orderId: String, expectedStatus: String) =
    matcherNode.waitOrderStatus(assetPair, orderId, expectedStatus)
}
