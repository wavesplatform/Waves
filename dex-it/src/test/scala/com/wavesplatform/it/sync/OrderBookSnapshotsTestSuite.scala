package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.{MatcherSuiteBase, orderGen}
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.matcher.model.OrderStatus
import com.wavesplatform.matcher.queue.QueueEventWithMeta
import com.wavesplatform.transaction.Asset.Waves
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

  private val (issue1, issue2, assetPair1) = issueAssetPair(alice, 8, 8)
  private val assetPair2                   = AssetPair(assetPair1.amountAsset, Waves)

  private val ordersPack1Size = 11
  private val ordersPack1 = Gen
    .containerOfN[Vector, Order](ordersPack1Size - 1, orderGen(matcher, alice, List(assetPair1)))
    .sample
    .get :+ orderGen(matcher, alice, List(assetPair2)).sample.get

  private val ordersPack2Size = interval.toInt
  private val ordersPack2 = Gen
    .containerOfN[Vector, Order](ordersPack2Size, orderGen(matcher, alice, List(assetPair2)))
    .sample
    .get

  "Order books are created with right offsets" in {
    ordersPack1.foreach(node.placeOrder)

    node.waitFor[QueueEventWithMeta.Offset]("ordersPack1Size - all events are consumed")(
      _.getCurrentOffset,
      _ == ordersPack1Size - 1,
      300.millis
    )
    val allSnapshotOffsets1 = node.getAllSnapshotOffsets

    withClue("We doesn't show pairs, those have snapshot's offset equal to -1") {
      if (allSnapshotOffsets1.contains(assetPair1.key)) allSnapshotOffsets1(assetPair1.key) should be < interval
      if (allSnapshotOffsets1.contains(assetPair2.key)) allSnapshotOffsets1(assetPair2.key) should be < interval
    }

    ordersPack2.foreach { order =>
      node.placeOrder(order)
    }

    node.waitFor[QueueEventWithMeta.Offset]("ordersPack2Size - all events are consumed")(
      _.getCurrentOffset,
      _ == ordersPack1Size + ordersPack2Size - 1,
      300.millis
    )
    val allSnapshotOffsets2 = node.getAllSnapshotOffsets
    withClue("Asset pairs has right offsets") {
      allSnapshotOffsets2.foreach {
        case (pair, offset) =>
          withClue(pair) {
            offset should be < (interval * 2)
          }
      }
    }
  }

  "All events are processed after restart" in {
    docker.killAndStartContainer(dockerNodes().head)
    node.waitFor[QueueEventWithMeta.Offset]("all events are consumed")(
      _.getCurrentOffset,
      _ == ordersPack1Size + ordersPack2Size - 1,
      300.millis
    )
    ordersPack1.foreach { order =>
      node.orderStatus(order.idStr(), order.assetPair) should not be OrderStatus.NotFound.name
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val ids = Seq(issue1, issue2).map(x => node.broadcastRequest(x.json())).map(_.id)
    ids.foreach(nodes.waitForTransaction)
    node.waitForHeight(node.height + 1)
  }
}
