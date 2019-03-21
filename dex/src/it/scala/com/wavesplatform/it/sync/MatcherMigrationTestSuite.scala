package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

import scala.concurrent.duration._

class MatcherMigrationTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs

  "issue asset and run migration tool inside container" - {
    // Alice issues new asset
    val aliceAsset =
      node
        .broadcastIssue(alice, "DisconnectCoin", "Alice's coin for disconnect tests", 1000 * someAssetAmount, 8, reissuable = false, issueFee, None)
        .id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceBalance = node.accountBalances(alice.address)._2
    val t1           = node.broadcastTransfer(alice, matcher.address, aliceBalance - minFee - 250000, minFee, None, None).id
    nodes.waitForHeightAriseAndTxPresent(t1)

    val aliceWavesPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Waves)

    "place order and run migration tool" in {
      // Alice places sell order
      val aliceOrder = node
        .placeOrder(alice, aliceWavesPair, OrderType.SELL, 3000000, 3000000, matcherFee)
      aliceOrder.status shouldBe "OrderAccepted"
      val firstOrder = aliceOrder.message.id

      // check order status
      node.waitOrderStatus(aliceWavesPair, firstOrder, "Accepted")

      // sell order should be in the node orderbook
      node.fullOrderHistory(alice).head.status shouldBe "Accepted"

      val tbBefore = node.tradableBalance(alice, aliceWavesPair)
      val rbBefore = node.reservedBalance(alice)

      // stop node, run migration tool and start node again
      docker.runMigrationToolInsideContainer(dockerNodes().head)
      Thread.sleep(60.seconds.toMillis)

      val height = nodes.map(_.height).max

      node.waitForHeight(height + 1, 40.seconds)
      node.orderStatus(firstOrder, aliceWavesPair).status shouldBe "Accepted"
      node.fullOrderHistory(alice).head.status shouldBe "Accepted"

      val tbAfter = node.tradableBalance(alice, aliceWavesPair)
      val rbAfter = node.reservedBalance(alice)

      assert(tbBefore == tbAfter)
      assert(rbBefore == rbAfter)

    }
  }
}
