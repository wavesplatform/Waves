package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.state.ByteStr
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.it.util._

import scala.util.Random

class TradersTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  import TradersTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head
  private def aliceNode   = nodes(1)
  private def bobNode     = nodes(2)

  "Verifications of tricky ordering cases" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", AssetQuantity, 0, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    // val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, AssetQuantity)
    matcherNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)
    bobNode.assertAssetBalance(bobNode.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset      = bobNode.issue(bobNode.address, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, false, 100000000L).id
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
    bobNode.assertAssetBalance(bobNode.address, bobNewAsset, bobAssetQuantity)

    "matcher should respond with Public key" in {
      matcherNode.matcherGet("/matcher").getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
    }

    "owner moves assets/waves to another account and order become an invalid" - {
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(8000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          val transferId = bobNode.transfer(bobNode.address, aliceNode.address, 3050, TransactionFee, Some(bobNewAsset), None).id
          nodes.waitForHeightAriseAndTxPresent(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobNewAsset, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.getOrderStatus(bobNewAsset, newestOrderId).status shouldBe "Accepted"
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherCancelOrder(bobNode, matcherNode, twoAssetsPair, newestOrderId).status should be("OrderCanceled")

          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, 3050, TransactionFee, Some(bobNewAsset), None).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }

        "leased waves, insufficient fee" in {
          val bobBalance    = bobNode.accountBalances(bobNode.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, MatcherFee for one order
          val leaseAmount = bobBalance - TransactionFee - MatcherFee
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobNewAsset, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.getOrderStatus(bobNewAsset, newestOrderId).status shouldBe "Accepted"
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherCancelOrder(bobNode, matcherNode, twoAssetsPair, newestOrderId).status should be("OrderCanceled")
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "moved waves, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobNode.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, MatcherFee for one order
          val transferAmount = bobBalance - TransactionFee - MatcherFee
          val transferId     = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobNewAsset, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.getOrderStatus(bobNewAsset, newestOrderId).status shouldBe "Accepted"
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherCancelOrder(bobNode, matcherNode, twoAssetsPair, newestOrderId).status should be("OrderCanceled")
          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }
      }

      "order with waves" - {
        "leased waves, insufficient fee" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobNode.address)._1

          val price    = TransactionFee / 2
          val bobOrder = prepareOrder(bobNode, matcherNode, bobWavesPair, OrderType.BUY, price * Order.PriceConstant, 1)
          val order1   = matcherNode.placeOrder(bobOrder).message.id
          matcherNode.waitOrderStatus(bobNewAsset, order1, "Accepted")

          //      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")

          val leaseAmount = bobBalance - TransactionFee - price
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The order '$order1' was cancelled") {
            matcherNode.waitOrderStatus(bobNewAsset, order1, "Cancelled")
          }

          // Cleanup
          nodes.waitForHeightArise()
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "leased waves, insufficient waves" in {
          val bobBalance = bobNode.accountBalances(bobNode.address)._1

          val price    = 1.waves
          val bobOrder = prepareOrder(bobNode, matcherNode, bobWavesPair, OrderType.BUY, price * Order.PriceConstant, 1)
          val order2   = matcherNode.placeOrder(bobOrder).message.id
          matcherNode.waitOrderStatus(bobNewAsset, order2, "Accepted")

          val leaseAmount = bobBalance - TransactionFee - price / 2
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The order '$order2' was cancelled") {
            matcherNode.waitOrderStatus(bobNewAsset, order2, "Cancelled")
          }

          // Cleanup
          nodes.waitForHeightArise()
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, fee).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "moved waves, insufficient fee" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobNode.address)._1

          val price    = TransactionFee / 2
          val bobOrder = prepareOrder(bobNode, matcherNode, bobWavesPair, OrderType.BUY, price * Order.PriceConstant, 1)
          val order3   = matcherNode.placeOrder(bobOrder).message.id
          matcherNode.waitOrderStatus(bobNewAsset, order3, "Accepted")

          val transferAmount = bobBalance - TransactionFee - price
          val txId           = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(txId)

          withClue(s"The order '$order3' was cancelled") {
            matcherNode.waitOrderStatus(bobNewAsset, order3, "Cancelled")
          }

          // Cleanup
          nodes.waitForHeightArise()
          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }

      }
    }
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val bobOrder = if (twoAssetsPair.amountAsset.contains(assetId)) {
      prepareOrder(bobNode, matcherNode, twoAssetsPair, OrderType.SELL, 1 * Order.PriceConstant, bobCoinAmount)
    } else {
      prepareOrder(bobNode, matcherNode, twoAssetsPair, OrderType.BUY, bobCoinAmount * Order.PriceConstant, 1)
    }
    val order = matcherNode.placeOrder(bobOrder)
    matcherNode.waitOrderStatus(assetId, order.message.id, "Accepted")
    order.message.id
  }

}

object TradersTestSuite {

  import ConfigFactory._
  import com.wavesplatform.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  private val AssetQuantity    = 1000
  private val MatcherFee       = 300000
  private val TransactionFee   = 300000

  private val minerDisabled = parseString("waves.miner.enable = no")
  private val matcherConfig = parseString(s"""
                                             |waves.matcher {
                                             |  enable = yes
                                             |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                             |  bind-address = "0.0.0.0"
                                             |  order-match-tx-fee = 300000
                                             |  blacklisted-assets = ["$ForbiddenAssetId"]
                                             |  balance-watching.enable = yes
                                             |}""".stripMargin)

  private val Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }
}
