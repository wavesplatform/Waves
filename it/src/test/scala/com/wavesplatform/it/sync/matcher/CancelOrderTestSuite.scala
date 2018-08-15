package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.api.http.assets.SignedIssueV1Request
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.utils.Base58
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import scala.util.{Random, Try}

class CancelOrderTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  import CancelOrderTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx))
  nodes.waitForHeightArise()

  "cancel order using api-key" in {
    val orderId = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 800, 100.waves).message.id
    matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

    matcherNode.cancelOrderWithApiKey(orderId)
    matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)

    matcherNode.fullOrderHistory(bobNode).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderHistoryByPair(bobNode, wavesUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderBook(wavesUsdPair).bids shouldBe empty
    matcherNode.orderBook(wavesUsdPair).asks shouldBe empty

  }

  "Alice and Bob trade WAVES-USD" - {
    "place usd-waves order" in {
      // Alice wants to sell USD for Waves
      val orderId1      = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 800, 100.waves).message.id
      val orderId2      = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 700, 100.waves).message.id
      val bobSellOrder3 = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 600, 100.waves).message.id

      matcherNode.fullOrderHistory(aliceNode)
      matcherNode.fullOrderHistory(bobNode)

      matcherNode.waitOrderStatus(wavesUsdPair, bobSellOrder3, "Accepted", 1.minute)

      val aliceOrder = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, 800, 0.00125.waves)
      matcherNode.placeOrder(aliceOrder).message.id

      Thread.sleep(2000)
      matcherNode.fullOrderHistory(aliceNode)
      val orders = matcherNode.fullOrderHistory(bobNode)
      for (orderId <- Seq(orderId1, orderId2)) {
        orders.filter(_.id == orderId).head.status shouldBe "Accepted"
      }
    }

  }

  def correctAmount(a: Long, price: Long): Long = {
    val min = (BigDecimal(Order.PriceConstant) / price).setScale(0, RoundingMode.CEILING)
    if (min > 0)
      Try(((BigDecimal(a) / min).toBigInt() * min.toBigInt()).bigInteger.longValueExact()).getOrElse(Long.MaxValue)
    else
      a
  }

  def receiveAmount(ot: OrderType, matchPrice: Long, matchAmount: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}

object CancelOrderTestSuite {

  import ConfigFactory._
  import com.wavesplatform.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  private val Decimals: Byte   = 2

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

  private val _Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }

  private val aliceSeed = _Configs(1).getString("account-seed")
  private val bobSeed   = _Configs(2).getString("account-seed")
  private val alicePk   = PrivateKeyAccount.fromSeed(aliceSeed).right.get
  private val bobPk     = PrivateKeyAccount.fromSeed(bobSeed).right.get

  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = "USD-X".getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueWctTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bobPk,
      name = "WCT-X".getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val UsdId: AssetId = IssueUsdTx.id()
  val WctId          = IssueWctTx.id()

  val wctUsdPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = Some(UsdId)
  )

  val wctWavesPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = None
  )

  val wavesUsdPair = AssetPair(
    amountAsset = None,
    priceAsset = Some(UsdId)
  )

  private val updatedMatcherConfig = parseString(s"""
       |waves.matcher {
       |  price-assets = [ "$UsdId", "WAVES"]
       |}
     """.stripMargin)

  private val Configs = _Configs.map(updatedMatcherConfig.withFallback(_))

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }
}
