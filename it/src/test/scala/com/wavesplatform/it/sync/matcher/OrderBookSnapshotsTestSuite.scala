package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.matcher.model.OrderStatus
import com.wavesplatform.matcher.queue.QueueEventWithMeta
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import org.scalacheck.Gen

import scala.concurrent.duration.DurationInt

class OrderBookSnapshotsTestSuite extends MatcherSuiteBase {
  private def interval        = 50L
  private def configOverrides = ConfigFactory.parseString(s"""waves.matcher {
      |  price-assets = ["WAVES"]
      |  snapshots-interval = $interval
      |}""".stripMargin)

  override protected def nodeConfigs: Seq[Config] = Configs.map(configOverrides.withFallback)

  private val (issue1, issue2, assetPair1) = issueAssetPair(aliceAcc, 8, 8)
  private val assetPair2                   = AssetPair(assetPair1.amountAsset, None)

  private val ordersPackSize = 11
  private val ordersPack = Gen
    .containerOfN[Vector, Order](ordersPackSize - 1, orderGen(matcherNode.publicKey, aliceAcc, List(assetPair1)))
    .sample
    .get :+ orderGen(matcherNode.publicKey, aliceAcc, List(assetPair2)).sample.get

  "Order books are created with right offsets" in {
    ordersPack.foreach { order =>
      matcherNode.placeOrder(order)
    }

    matcherNode.waitFor[QueueEventWithMeta.Offset]("all events are consumed")(_.getCurrentOffset, _ == ordersPackSize - 1, 300.millis)
    val allSnapshotOffsets = matcherNode.getAllSnapshotOffsets

    allSnapshotOffsets(assetPair1.key) should be < interval

    // [0;{N}] orders to the assetPair, {N+1} order to assetPair2
    // the offset of assetPair2's order books should be {N} to be able to process {N+1} after restart
    // see OrderBookActor.executeCommands
    allSnapshotOffsets(assetPair2.key) shouldBe (ordersPackSize - 2) // -2 because snapshot offsets are the indexes, those start from 0
  }

  "All events are processed after restart" in {
    docker.killAndStartContainer(dockerNodes().head)
    matcherNode.waitFor[QueueEventWithMeta.Offset]("all events are consumed")(_.getCurrentOffset, _ == ordersPackSize - 1, 300.millis)
    ordersPack.foreach { order =>
      matcherNode.orderStatus(order.idStr(), order.assetPair) should not be OrderStatus.NotFound.name
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val ids = Seq(issue1, issue2).map(matcherNode.signedIssue).map(_.id)
    ids.foreach(nodes.waitForTransaction)
  }
}
