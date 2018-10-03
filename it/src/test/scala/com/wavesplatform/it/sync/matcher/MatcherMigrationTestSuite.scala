package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{TransferSending, _}
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.state.ByteStr
import org.scalatest.concurrent.Eventually
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class MatcherMigrationTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with TransferSending
    with Eventually {

  override protected def nodeConfigs: Seq[Config] = Configs
  private def matcherNode                         = nodes.head
  private def aliceNode                           = nodes(1)

  "issue asset and run migration tool inside container" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode
        .issue(aliceNode.address, "DisconnectCoin", "Alice's coin for disconnect tests", 1000 * someAssetAmount, 8, reissuable = false, issueFee)
        .id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceBalance = aliceNode.accountBalances(aliceNode.address)._2
    val t1           = aliceNode.transfer(aliceNode.address, matcherNode.address, aliceBalance - minFee - 250000, minFee, None, None).id
    nodes.waitForHeightAriseAndTxPresent(t1)

    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    "place order and run migration tool" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceNode, aliceWavesPair, OrderType.SELL, 3000000, 3000000)
      aliceOrder.status shouldBe "OrderAccepted"
      val firstOrder = aliceOrder.message.id

      // check order status
      matcherNode.orderStatus(firstOrder, aliceWavesPair).status shouldBe "Accepted"

      // sell order should be in the aliceNode orderbook
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      val tbBefore = matcherNode.tradableBalance(aliceNode, aliceWavesPair)
      val rbBefore = matcherNode.reservedBalance(aliceNode)

      // stop node, run migration tool and start node again
      docker.runMigrationToolInsideContainer(dockerNodes().head, Configs.head)
      Thread.sleep(60.seconds.toMillis)

      val height = nodes.map(_.height).max

      matcherNode.waitForHeight(height + 1, 40.seconds)
      matcherNode.orderStatus(firstOrder, aliceWavesPair).status shouldBe "Accepted"
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      val tbAfter = matcherNode.tradableBalance(aliceNode, aliceWavesPair)
      val rbAfter = matcherNode.reservedBalance(aliceNode)

      assert(tbBefore == tbAfter)
      assert(rbBefore == rbAfter)

    }
  }
}
