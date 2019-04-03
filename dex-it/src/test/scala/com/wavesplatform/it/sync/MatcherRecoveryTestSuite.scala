package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.api.{MatcherCommand, MatcherState}
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.matcher.queue.QueueEventWithMeta
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import org.scalacheck.Gen

import scala.concurrent.duration.DurationInt
import scala.util.Random

class MatcherRecoveryTestSuite extends MatcherSuiteBase {
  private def configOverrides = ConfigFactory.parseString("""waves.matcher {
      |  price-assets = ["WAVES"]
      |  snapshots-interval = 51
      |}""".stripMargin)

  override protected def nodeConfigs: Seq[Config] = Configs.map(configOverrides.withFallback)

  private val placesNumber  = 200
  private val cancelsNumber = placesNumber / 10

  private val (issue1, issue2, assetPair1) = issueAssetPair(alice, 8, 8)
  private val assetPair2                   = AssetPair(assetPair1.amountAsset, Waves)
  private val assetPair3                   = AssetPair(assetPair1.priceAsset, Waves)
  private val assetPairs                   = Seq(assetPair1, assetPair2, assetPair3)

  {
    val xs = Seq(issue1, issue2).map(_.json()).map(node.broadcastRequest(_))
    xs.foreach(x => node.waitForTransaction(x.id))
  }

  private val orders    = Gen.containerOfN[Vector, Order](placesNumber, orderGen(matcher, alice, assetPairs)).sample.get
  private val lastOrder = orderGen(matcher, alice, assetPairs).sample.get

  "Place, fill and cancel a lot of orders" in {
    val cancels  = (1 to cancelsNumber).map(_ => choose(orders))
    val commands = Random.shuffle(orders.map(MatcherCommand.Place(node, _))) ++ cancels.map(MatcherCommand.Cancel(node, alice, _))
    executeCommands(commands)
    executeCommands(List(MatcherCommand.Place(node, lastOrder)))
  }

  "Wait until all requests are processed - 1" in node.waitForStableOffset(10, 100, 200.millis)

  private var stateBefore: MatcherState = _

  "Store the current state" in {
    stateBefore = state
    withClue("common offset") {
      stateBefore.offset should be > 0L
    }
    stateBefore.snapshots.foreach {
      case (assetPair, snapshotOffset) =>
        withClue(assetPair) {
          snapshotOffset should be > 0L
        }
    }
  }

  "Restart the matcher" in docker.restartContainer(node)

  "Wait until all requests are processed - 2" in {
    node.waitFor[QueueEventWithMeta.Offset]("all events are consumed")(_.getCurrentOffset, _ == stateBefore.offset, 300.millis)
    withClue("Last command processed") {
      node.waitOrderProcessed(lastOrder.assetPair, lastOrder.idStr())
    }
  }

  "Verify the state" in {
    val stateAfter = state
    stateBefore shouldBe stateAfter
  }

  private def state = node.matcherState(assetPairs, orders, Seq(alice))
}
