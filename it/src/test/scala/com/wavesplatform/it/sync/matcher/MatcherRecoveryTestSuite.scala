package com.wavesplatform.it.sync.matcher

import java.util.concurrent.TimeUnit

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.Docker.DockerNode
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.api.{MatcherStatusResponse, OrderBookResponse, OrderbookHistory}
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.MatcherRecoveryTestSuite._
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.it.util.GlobalTimer
import com.wavesplatform.matcher.queue.QueueEventWithMeta
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalacheck.Gen

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future, Promise}
import scala.util.Random
import scala.util.control.NonFatal

class MatcherRecoveryTestSuite extends MatcherSuiteBase {
  private def configOverrides = ConfigFactory.parseString("""waves.matcher {
      |  price-assets = ["WAVES"]
      |  snapshots-interval = 51
      |}""".stripMargin)

  override protected def nodeConfigs: Seq[Config] = Configs.map(configOverrides.withFallback)

  private val placesNumber  = 200
  private val cancelsNumber = placesNumber / 10

  private val (issue1, issue2, assetPair1) = issueAssetPair(aliceAcc, 8, 8)
  private val assetPair2                   = AssetPair(assetPair1.amountAsset, None)
  private val assetPair3                   = AssetPair(assetPair1.priceAsset, None)
  private val assetPairs                   = Seq(assetPair1, assetPair2, assetPair3)

  Seq(issue1, issue2).map(matcherNode.signedIssue).map(x => nodes.waitForTransaction(x.id))

  private val orders = Gen.containerOfN[Vector, Order](placesNumber, orderGen(assetPairs)).sample.get

  "Place, fill and cancel a lot of orders" in {
    val cancels  = (1 to cancelsNumber).map(_ => random(orders))
    val requests = Random.shuffle(orders.map(Command.Place)) ++ cancels.map(Command.Cancel)
    requests.foreach {
      case Command.Place(order) => matcherNode.placeOrder(order)
      case Command.Cancel(order) =>
        try matcherNode.cancelOrder(aliceAcc, order.assetPair, order.idStr())
        catch {
          case NonFatal(_) => // not interesting
        }
    }

    waitAllRequestsProcessed(10, 100, 200.millis)
  }

  private var stateBefore: State = _

  "Store the current state" in {
    stateBefore = state
    stateBefore.snapshots.foreach {
      case (assetPair, snapshotOffset) =>
        withClue(assetPair) {
          snapshotOffset should be > 0L
        }
    }
  }

  "Restart the matcher" in {
    docker.restartContainer(matcherNode.asInstanceOf[DockerNode])
  }

  "Wait all requests are processed" in {
    matcherNode.waitFor[QueueEventWithMeta.Offset]("all requests are processed")(_.getCurrentOffset, _ == stateBefore.offset, 300.millis)
  }

  "Verify the state" in {
    val stateAfter = state
    stateAfter shouldBe stateBefore
  }

  private def orderGen(assetPairs: Seq[AssetPair]): Gen[Order] =
    for {
      assetPair      <- Gen.oneOf(assetPairs)
      tpe            <- Gen.oneOf(OrderType.BUY, OrderType.SELL)
      amount         <- Gen.choose(10, 100)
      price          <- Gen.choose(10, 100)
      orderVersion   <- Gen.oneOf(1: Byte, 2: Byte)
      expirationDiff <- Gen.choose(600000, 6000000)
    } yield {
      val ts = System.currentTimeMillis()
      if (tpe == OrderType.BUY)
        Order.buy(
          aliceAcc,
          matcherNode.publicKey,
          assetPair,
          amount,
          price * Order.PriceConstant,
          System.currentTimeMillis(),
          ts + expirationDiff,
          matcherFee,
          orderVersion
        )
      else
        Order.sell(
          aliceAcc,
          matcherNode.publicKey,
          assetPair,
          amount,
          price * Order.PriceConstant,
          System.currentTimeMillis(),
          ts + expirationDiff,
          matcherFee,
          orderVersion
        )
    }

  private def random(orders: IndexedSeq[Order]) = orders(Random.nextInt(orders.size))

  private def state: State = clean {
    State(
      offset = matcherNode.getCurrentOffset,
      snapshots = matcherNode.getAllSnapshotOffsets,
      orderBooks = assetPairs.map(x => x        -> matcherNode.orderBook(x)).toMap,
      orderStatuses = orders.map(x => x.idStr() -> matcherNode.orderStatus(x.idStr(), x.assetPair)).toMap,
      reservedBalances = matcherNode.reservedBalance(aliceAcc),
      orderHistory = assetPairs.map(x => x -> matcherNode.orderHistoryByPair(aliceAcc, x)).toMap
    )
  }

  private def waitAllRequestsProcessed(confirmations: Int, maxTries: Int, interval: FiniteDuration): Boolean =
    Await.result(waitAllRequestsProcessedAsync(confirmations, maxTries, interval), (maxTries + 1) * interval)

  private def waitAllRequestsProcessedAsync(confirmations: Int, maxTries: Int, interval: FiniteDuration): Future[Boolean] = {
    val p = Promise[Boolean]

    def loop(n: Int, currRow: Int, currOffset: QueueEventWithMeta.Offset): Unit =
      if (currRow >= confirmations) p.success(true)
      else if (n > maxTries) p.success(false)
      else
        GlobalTimer.instance.newTimeout(
          _ => {
            val freshOffset = matcherNode.getCurrentOffset
            if (freshOffset == currOffset) loop(n + 1, currRow + 1, freshOffset)
            else loop(n + 1, 0, freshOffset)
          },
          interval.toMillis,
          TimeUnit.MILLISECONDS
        )

    loop(0, 0, matcherNode.getCurrentOffset)
    p.future
  }

  private def clean(x: State): State = x.copy(
    orderBooks = x.orderBooks.map { case (k, v) => k -> v.copy(timestamp = 0L) }
  )
}

object MatcherRecoveryTestSuite {
  private sealed trait Command extends Product with Serializable
  private object Command {
    case class Place(order: Order)  extends Command
    case class Cancel(order: Order) extends Command
  }
  private case class State(offset: QueueEventWithMeta.Offset,
                           snapshots: Map[String, QueueEventWithMeta.Offset],
                           orderBooks: Map[AssetPair, OrderBookResponse],
                           orderStatuses: Map[String, MatcherStatusResponse],
                           reservedBalances: Map[String, Long],
                           orderHistory: Map[AssetPair, Seq[OrderbookHistory]])
}
