package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import com.wavesplatform.it.util._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.OrderbookHistory
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.state.ByteStr
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.transaction.assets.IssueTransactionV1
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.it.sync._
import com.wavesplatform.utils.Base58
import scorex.api.http.assets.SignedIssueV1Request

import scala.concurrent.duration._
import scala.util.Random

class MatcherTradingBalance
    extends FreeSpec
    with NodesFromDocker
    with ReportingTestName
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure {

  import MatcherTradingBalance._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head
  private def aliceNode   = nodes(1)
  private def bobNode     = nodes(2)

  val req        = createSignedIssueRequest(assetTx)
  val aliceAsset = aliceNode.signedIssue(req).id
  nodes.waitForHeightAriseAndTxPresent(aliceAsset)

  // Alice issues new assets
  val alicePair = AssetPair(None, ByteStr.decodeBase58(aliceAsset).toOption)

  "one sell and many buy" - {

    "check alice balance after issue" in {
      // Check balances on Alice's account
      aliceNode.assertAssetBalance(aliceAddress, aliceAsset, AssetQuantity)
      matcherNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)
    }

    "transfer some asset to bob" in {
      val bobBalance              = bobNode.accountBalances(bobNode.address)
      val transferBobWavesToALice = bobNode.transfer(bobNode.address, aliceAddress, bobBalance._1 - 10.waves - minFee, minFee, None, None).id
      nodes.waitForHeightAriseAndTxPresent(transferBobWavesToALice)
      // bobNode.assertAssetBalance(bobNode.address, aliceAsset, AssetQuantity / 2)
    }

    "alice set order" in {
      val bobOrderId = matcherNode
        .placeOrder(prepareOrder(bobNode, matcherNode, alicePair, OrderType.SELL, 600, 2.waves, 10.minutes))
        .message
        .id
      matcherNode.waitOrderStatus(aliceAsset, bobOrderId, "Accepted")

      ordersRequestsGen(2, aliceNode, alicePair, OrderType.BUY, 0.00676.waves, 600)
      //Thread.sleep(60000)
      nodes.waitForHeightArise()

      val orderStatusesAfterMatching = aliceOrderHistory().map(_.status)
      orderStatusesAfterMatching should not contain ("Accepted")
      orderStatusesAfterMatching should not contain ("PartiallyFilled")

    }
  }

  private def ordersRequestsGen(n: Int, node: Node, assetPair: AssetPair, orderType: OrderType, amount: Long, price: Long): Unit = {
    1 to n map (_ => {
      matcherNode
        .placeOrder(prepareOrder(node, matcherNode, assetPair, orderType, price, amount, 10.minutes))
    })
  }

  private def aliceOrderHistory(): Seq[OrderbookHistory] = {
    getOrderBook(aliceNode, matcherNode)
  }

  private def orderStatus(node: Node, orderId: String) = {
    aliceOrderHistory().filter(_.id == orderId).seq.head.status
  }
}

object MatcherTradingBalance {
  private val ForbiddenAssetId    = "FdbnAsset"
  private val orderLimit          = 20
  private val AssetQuantity: Long = 1000000000

  import ConfigFactory._
  import NodeConfigs.Default

//  val matcherNode = Default.head
  val aliceNode = Default(1)
//  val bobNode = Default(2)
//  val miner = Default(3)
  val aliceAddress = aliceNode.getString("address")

  private val seed = Default(1).getString("account-seed")
  private val pk   = PrivateKeyAccount.fromSeed(seed).right.get
  val assetTx = IssueTransactionV1
    .selfSigned(
      sender = pk,
      name = "asset".getBytes(),
      description = "asset description".getBytes(),
      quantity = AssetQuantity,
      decimals = 2,
      reissuable = false,
      fee = issueFee,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val assetId = assetTx.id()

  private val matcherConfig = ConfigFactory.parseString(s"""
                                                           |waves.matcher {
                                                           |  enable = yes
                                                           |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                                           |  bind-address = "0.0.0.0"
                                                           |  order-match-tx-fee = 300000
                                                           |  blacklisted-assets = [$ForbiddenAssetId]
                                                           |  price-assets: [$assetId]
                                                           |  order-cleanup-interval = 20s
                                                           |  rest-order-limit=$orderLimit
                                                           |}""".stripMargin)

  private val minerDisabled = parseString("waves.miner.enable = no")
  private val minerConfig   = parseString("waves.miner.enable = yes")

  val Configs: Seq[Config] = Seq(
    matcherConfig.withFallback(Default.head),
    minerDisabled.withFallback(Default(1)),
    minerDisabled.withFallback(Default(2)),
    minerConfig.withFallback(Default(3))
  )

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
//  private val Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
//    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
//    .map { case (n, o) => o.withFallback(n) }
}
