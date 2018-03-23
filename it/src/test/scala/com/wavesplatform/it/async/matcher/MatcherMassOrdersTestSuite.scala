package com.wavesplatform.it.async.matcher

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.crypto
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api.OrderbookHistory
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.state2.ByteStr
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class MatcherMassOrdersTestSuite
    extends FreeSpec
    with NodesFromDocker
    with MatcherUtils
    with ReportingTestName
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure {

  import MatcherMassOrdersTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  private var aliceAsset: String              = ""
  private var aliceSecondAsset: String        = ""
  private var aliceWavesPair: AssetPair       = AssetPair(None, None)
  private var aliceSecondWavesPair: AssetPair = AssetPair(None, None)

  private def ordersRequestsGen(n: Int, node: Node, assetPair: AssetPair, orderType: OrderType, amount: Long): Future[Unit] = {
    val xs = 1 to n

    def execute(requests: Seq[Int], result: Future[Unit]): Future[Unit] = requests match {
      case Seq() => result
      case head +: tail =>
        val r = result.flatMap { _ =>
          matcherNode
            .placeOrder(prepareOrder(node, matcherNode, assetPair, orderType, Order.PriceConstant, amount, (120 + Random.nextInt(70)).seconds))
            .map(_ => ())
        }
        execute(tail, r)
    }

    execute(xs, Future.successful(()))
  }

  private def aliceOrderHistory(): Seq[OrderbookHistory] = {
    val ts         = System.currentTimeMillis()
    val privateKey = aliceNode.privateKey
    val signature  = ByteStr(crypto.sign(privateKey, aliceNode.publicKey.publicKey ++ Longs.toByteArray(ts)))
    Await.result(matcherNode.getOrderbookByPublicKey(aliceNode.publicKeyStr, ts, signature), 1.minute)
  }

  private def orderStatus(node: Node, orderId: String) = {
    aliceOrderHistory().filter(_.id == orderId).seq.head.status
  }

  "create known orders with statuses FILL, PARTIAL, CANCELLED, ACTIVE" - {

    // Alice issues new asset
    aliceAsset = issueAsset(aliceNode, "AliceCoin", AssetQuantity)
    aliceSecondAsset = issueAsset(aliceNode, "AliceSecondCoin", AssetQuantity)
    aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    aliceSecondWavesPair = AssetPair(ByteStr.decodeBase58(aliceSecondAsset).toOption, None)

    // Wait for balance on Alice's account
    waitForAssetBalance(aliceNode, aliceAsset, AssetQuantity)
    waitForAssetBalance(aliceNode, aliceSecondAsset, AssetQuantity)
    waitForAssetBalance(matcherNode, aliceAsset, 0)
    Await.result(aliceNode.transfer(aliceNode.address, bobNode.address, AssetQuantity / 2, 100000, Some(aliceAsset)), 1.minute)
    Await.result(aliceNode.transfer(aliceNode.address, bobNode.address, AssetQuantity / 2, 100000, Some(aliceSecondAsset)), 1.minute)
    waitForAssetBalance(bobNode, aliceAsset, AssetQuantity / 2)

    // Alice places sell order
    val (aliceOrderIdFill, _) =
      matcherPlaceOrder(matcherNode, prepareOrder(aliceNode, matcherNode, aliceSecondWavesPair, OrderType.SELL, Order.PriceConstant, 3, 10.minutes))

    val (alicePartialOrderId, _) =
      matcherPlaceOrder(matcherNode, prepareOrder(aliceNode, matcherNode, aliceSecondWavesPair, OrderType.SELL, Order.PriceConstant, 3, 10.minutes))

    val (aliceOrderToCancelId, _) =
      matcherPlaceOrder(matcherNode, prepareOrder(aliceNode, matcherNode, aliceSecondWavesPair, OrderType.SELL, Order.PriceConstant, 3, 2.minutes))

    val (aliceActiveOrderId, _) =
      matcherPlaceOrder(matcherNode,
                        prepareOrder(aliceNode, matcherNode, aliceSecondWavesPair, OrderType.SELL, Order.PriceConstant + 1, 3, 10.minutes))

    waitForOrderStatus(matcherNode, aliceSecondAsset, aliceOrderToCancelId, "Cancelled", 3.minutes)
    //Bob orders should partailly fill one Alice order and fill another
    Await.result(ordersRequestsGen(2, bobNode, aliceSecondWavesPair, OrderType.BUY, 2), 2.minute)

    //check orders after filling
    waitForOrderStatus(matcherNode, aliceSecondAsset, alicePartialOrderId, "PartiallyFilled", 1.minutes)
    orderStatus(aliceNode, aliceOrderIdFill) shouldBe "Filled"
    orderStatus(aliceNode, alicePartialOrderId) shouldBe "PartiallyFilled"

    "mass create orders with random lifetime. Active orders still in list" in {

      val orderIds = aliceOrderHistory().map(_.id)

      orderIds should contain(aliceActiveOrderId)

      Await.result(ordersRequestsGen(orderLimit, aliceNode, aliceWavesPair, OrderType.SELL, 3), 2.minute)
      //wait for some orders cancelled
      Thread.sleep(100000)
      Await.result(ordersRequestsGen(orderLimit, bobNode, aliceWavesPair, OrderType.BUY, 2), 2.minute)

      // Alice check that order Active order is still in list
      val orderIdsAfterMatching = aliceOrderHistory().map(_.id)

      orderIdsAfterMatching should contain(aliceActiveOrderId)
      orderIdsAfterMatching should contain(alicePartialOrderId)

      matcherCheckOrderStatus(matcherNode, aliceSecondAsset, aliceActiveOrderId) shouldBe "Accepted"
      matcherCheckOrderStatus(matcherNode, aliceSecondAsset, alicePartialOrderId) shouldBe "PartiallyFilled"
    }

    "Filled and Cancelled orders should be after Partial And Accepted" in {
      val lastIdxOfActiveOrder  = aliceOrderHistory().lastIndexWhere(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled"))
      val firstIdxOfClosedOrder = aliceOrderHistory().indexWhere(o => o.status.equals("Filled") || o.status.equals("Cancelled"))
      lastIdxOfActiveOrder should be < firstIdxOfClosedOrder
    }

    "Accepted and PartiallyFilled orders should be sorted by timestamp." in {
      val activeAndPartialOrders = aliceOrderHistory().filter(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled")).map(_.timestamp)
      activeAndPartialOrders.reverse shouldBe sorted
    }

    "Filled and Cancelled orders should be sorted by timestamp." in {
      val filledAndCancelledOrders = aliceOrderHistory().filter(o => o.status.equals("Filled") || o.status.equals("Cancelled")).map(_.timestamp)
      filledAndCancelledOrders.reverse shouldBe sorted
    }
  }
}

object MatcherMassOrdersTestSuite {
  val ForbiddenAssetId = "FdbnAsset"
  val orderLimit       = 20

  import NodeConfigs.Default

  private val matcherConfig = ConfigFactory.parseString(s"""
       |waves.matcher {
       |  enable=yes
       |  account="3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC"
       |  bind-address="0.0.0.0"
       |  order-match-tx-fee = 300000
       |  blacklisted-assets = [$ForbiddenAssetId]
       |  order-cleanup-interval = 20s
       |  rest-order-limit=$orderLimit
       |}
       |waves.rest-api {
       |    enable = yes
       |    api-key-hash = 7L6GpLHhA5KyJTAVc8WFHwEcyTY8fC8rRbyMCiFnM4i
       |}
       |waves.miner.enable=no
      """.stripMargin)

  private val nonGeneratingPeersConfig = ConfigFactory.parseString(
    """
      |waves.matcher {
      | order-cleanup-interval = 30s
      |}
      |waves.miner.enable=no
    """.stripMargin
  )

  val AssetQuantity: Long = 1000000000

  val MatcherFee: Long     = 300000
  val TransactionFee: Long = 300000

  val Waves: Long = 100000000L

  val Configs: Seq[Config] = Seq(matcherConfig.withFallback(Default.head)) ++
    Random.shuffle(Default.tail.init).take(2).map(nonGeneratingPeersConfig.withFallback(_)) ++
    Seq(Default.last)

}
