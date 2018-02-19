package com.wavesplatform.it.matcher

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.state2.ByteStr
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class MatcherMassOrdersTestSuite extends FreeSpec  with NodesFromDocker with MatcherUtils
  with ReportingTestName  with Matchers with BeforeAndAfterAll with CancelAfterFailure{

  import MatcherMassOrdersTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  private var aliceSellOrderId = ""

  private var aliceAsset: String = ""
  private var aliceSecondAsset: String = ""
  private var aliceWavesPair: AssetPair = AssetPair(None, None)
  private var aliceSecondWavesPair: AssetPair = AssetPair(None, None)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    // Alice issues new asset
    aliceAsset = issueAsset(aliceNode, "AliceCoin", AssetQuantity)
    aliceSecondAsset = issueAsset(aliceNode, "AliceSecondCoin", AssetQuantity)
    aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    aliceSecondWavesPair = AssetPair(ByteStr.decodeBase58(aliceSecondAsset).toOption, None)

    // Wait for balance on Alice's account
    waitForAssetBalance(aliceNode, aliceAsset, AssetQuantity)
    waitForAssetBalance(aliceNode, aliceSecondAsset, AssetQuantity)
    waitForAssetBalance(matcherNode, aliceAsset, 0)
    Await.result(aliceNode.transfer(aliceNode.address, bobNode.address, AssetQuantity / 2, 100000, Some(aliceAsset)), 1.minute);
    waitForAssetBalance(bobNode, aliceAsset, AssetQuantity / 2)
  }

  private def ordersRequestsGen(n: Int, node: Node, assetPair: AssetPair, orderType: OrderType): Future[Unit] = {
    val xs = 1 to n

    def execute(requests: Seq[Int], result: Future[Unit]): Future[Unit] = requests match {
      case Seq() => result
      case head +: tail =>
        val r = result.flatMap { _ =>
          matcherNode.placeOrder(prepareOrder(node, matcherNode, assetPair, orderType,
            Order.PriceConstant, 1, 70.seconds)).map(_ => ())
        }
        execute(tail, r)
    }

    execute(xs, Future.successful(()))
  }

  "mass orders could be placed and active in list" in {
    // Alice places sell order
    val (id, status) = matcherPlaceOrder(matcherNode,
      prepareOrder(aliceNode, matcherNode, aliceSecondWavesPair, OrderType.SELL, Order.PriceConstant, 1, 10.minutes))
    status shouldBe "OrderAccepted"
    aliceSellOrderId = id
    // Alice checks that the order in order book
    matcherCheckOrderStatus(matcherNode, aliceAsset, aliceSellOrderId) shouldBe "Accepted"


    val ts = System.currentTimeMillis()
    val privateKey = aliceNode.privateKey
    val signature = ByteStr(EllipticCurveImpl.sign(privateKey, aliceNode.publicKey.publicKey ++ Longs.toByteArray(ts)))
    val orderIds = Await.result(matcherNode.getOrderbookByPublicKey(aliceNode.publicKeyStr, ts, signature), 1.minute)
      .map(_.id)

    orderIds should contain(aliceSellOrderId)

    Await.result(ordersRequestsGen(orderLimit, aliceNode, aliceWavesPair, OrderType.SELL), 2.minute)
    Await.result(ordersRequestsGen(orderLimit, bobNode, aliceWavesPair, OrderType.BUY), 2.minute)

    // Alice check that order Active order is still in list
    val ts1 = System.currentTimeMillis()
    val signature1 = ByteStr(EllipticCurveImpl.sign(privateKey, aliceNode.publicKey.publicKey ++ Longs.toByteArray(ts1)))
    val orderIdsAfterMatching = Await.result(matcherNode.getOrderbookByPublicKey(aliceNode.publicKeyStr, ts1, signature1), 1.minute)
      .map(_.id)

    orderIdsAfterMatching should contain(aliceSellOrderId)
    matcherCheckOrderStatus(matcherNode, aliceSecondAsset, aliceSellOrderId) shouldBe "Accepted"

  }

}

object MatcherMassOrdersTestSuite {
  val ForbiddenAssetId = "FdbnAsset"
  val orderLimit = 20
  import NodeConfigs.Default

  private val matcherConfig = ConfigFactory.parseString(
    s"""
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

  val AssetQuantity: Long = 100000000

  val MatcherFee: Long = 300000
  val TransactionFee: Long = 300000

  val Waves: Long = 100000000L

  val Configs: Seq[Config] = Seq(matcherConfig.withFallback(Default.head)) ++
    Random.shuffle(Default.tail.init).take(2).map(nonGeneratingPeersConfig.withFallback(_)) ++
    Seq(Default.last)


}
