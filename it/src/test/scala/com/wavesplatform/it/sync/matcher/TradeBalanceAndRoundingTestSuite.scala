package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.utils.Base58
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.api.http.assets.SignedIssueV1Request
import scorex.transaction.assets.IssueTransactionV1
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import scala.util.{Random, Try}

class TradeBalanceAndRoundingTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  import TradeBalanceAndRoundingTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))

  private val aliceSellAmount = 500

  "Alice and Bob trade USD-WAVES" in {
    nodes.waitForHeightArise()
    val aliceWavesBalanceBefore = matcherNode.accountBalances(aliceNode.address)._1
    val bobWavesBalanceBefore   = matcherNode.accountBalances(bobNode.address)._1

    val price           = 280
    val buyOrderAmount  = 700000
    val sellOrderAmount = 300000000

    // Alice wants to sell USD for Waves
    val aliceOrder   = matcherNode.prepareOrder(aliceNode, usdWavesPair, OrderType.BUY, price, buyOrderAmount)
    val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id
    matcherNode.waitOrderStatus(usdWavesPair, aliceOrderId, "Accepted", 1.minute)

    // Bob wants to buy some USD
    val bobOrder1   = matcherNode.prepareOrder(bobNode, usdWavesPair, OrderType.SELL, price, sellOrderAmount)
    val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id
    matcherNode.waitOrderStatus(usdWavesPair, bobOrder1Id, "PartiallyFilled", 1.minute)

    // Each side get fair amount of assets
    val exchangeTx = matcherNode.transactionsByOrder(aliceOrder.id().base58).headOption.getOrElse(fail("Expected an exchange transaction"))
    nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)

    val aliceWavesBalanceAfter = matcherNode.accountBalances(aliceNode.address)._1
    val aliceUsdBalance        = matcherNode.assetBalance(aliceNode.address, UsdId.base58).balance

    val bobWavesBalanceAfter = matcherNode.accountBalances(bobNode.address)._1
    val bobUsdBalance        = matcherNode.assetBalance(bobNode.address, UsdId.base58).balance
    val adjustedAmount       = receiveAmount(OrderType.BUY, price, buyOrderAmount)
    val adjustedTotal        = receiveAmount(OrderType.SELL, price, buyOrderAmount)

    (aliceWavesBalanceAfter - aliceWavesBalanceBefore) should be(
      adjustedAmount - (BigInt(MatcherFee) * adjustedAmount / buyOrderAmount).bigInteger.longValue())

    aliceUsdBalance - defaultAssetQuantity should be(-adjustedTotal)
    bobWavesBalanceAfter - bobWavesBalanceBefore should be(
      -adjustedAmount - (BigInt(MatcherFee) * adjustedAmount / sellOrderAmount).bigInteger.longValue())
    bobUsdBalance should be(adjustedTotal)
  }

  def correctAmount(a: Long, price: Long): Long = {
    val min = (BigDecimal(Order.PriceConstant) / price).setScale(0, RoundingMode.HALF_UP)
    if (min > 0)
      Try(((BigDecimal(a) / min).toBigInt() * min.toBigInt()).bigInteger.longValueExact()).getOrElse(Long.MaxValue)
    else
      a
  }
  def receiveAmount(ot: OrderType, matchPrice: Long, matchAmount: Long): Long =
    if (ot == OrderType.BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}

object TradeBalanceAndRoundingTestSuite {

  import ConfigFactory._
  import com.wavesplatform.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  private val AssetQuantity    = 1000
  private val MatcherFee       = 300000
  private val TransactionFee   = 300000
  private val Waves            = 100000000L

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
  private val pk        = PrivateKeyAccount.fromSeed(aliceSeed).right.get
  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = pk,
      name = "USD-X".getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 2,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val UsdId = IssueUsdTx.id()
  val usdWavesPair = AssetPair(
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
