package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.BlockedAssetsTestSuite._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransactionV2, Order, OrderV2}
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransactionV2}
import org.scalatest._

import scala.concurrent.duration.DurationInt
class BlockedAssetsTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {
  override protected def nodeConfigs: Seq[Config] = Configs.map(ConfigOverrides.withFallback)

  private def matcher = dockerNodes().head
  private def alice   = dockerNodes()(1)

  val (amount, price) = (1000L, Order.PriceConstant)

  private val asset = {
    val txId   = matcher.signedBroadcast(IssueUsdTx.json()).id
    val height = matcher.waitForTransaction(txId).height
    nodes.waitForHeight(height + 1)
    IssueUsdTx
  }

  private def order      = matcher.prepareOrder(matcher.privateKey, wavesUsdPair, BUY, amount, price, matcherFee)
  private val toTransfer = LimitOrder(order).requiredBalance(Some(asset.id())) * 2
  private def reverseTransferTx =
    TransferTransactionV2
      .selfSigned(
        assetId = Some(asset.id()),
        sender = matcher.privateKey,
        recipient = alice.publicKey.toAddress,
        amount = toTransfer,
        timestamp = System.currentTimeMillis(),
        feeAssetId = None,
        feeAmount = 300000L,
        attachment = Array.emptyByteArray
      )
      .explicitGet()

  "TransferTransaction" in {
    val transferTx = TransferTransactionV2
      .selfSigned(
        assetId = Some(asset.id()),
        sender = alice.privateKey,
        recipient = matcher.publicKey.toAddress,
        amount = toTransfer,
        timestamp = System.currentTimeMillis(),
        feeAssetId = None,
        feeAmount = 300000L,
        attachment = Array.emptyByteArray
      )
      .explicitGet()
    matcher.signedBroadcast(transferTx.json(), waitForTx = true)
    nodes.waitForHeight(matcher.transactionInfo(transferTx.id().base58).height + 1)

    val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
    tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0

    val reservedBalance = matcher.reservedBalance(matcher.privateKey)
    reservedBalance.getOrElse(asset.id().base58, 0) shouldBe toTransfer

    matcher.expectIncorrectOrderPlacement(order, 400, "OrderRejected")

//    matcher.signedBroadcast(reverseTransferTx.json(), waitForTx = true)
  }

  "MassTransferTransaction" in {
    val transferTx = MassTransferTransaction
      .selfSigned(
        assetId = Some(asset.id()),
        sender = alice.privateKey,
        transfers = List(MassTransferTransaction.ParsedTransfer(matcher.publicKey.toAddress, toTransfer)),
        timestamp = System.currentTimeMillis(),
        feeAmount = 300000L,
        attachment = Array.emptyByteArray
      )
      .explicitGet()
    matcher.signedBroadcast(transferTx.json(), waitForTx = true)
    nodes.waitForHeight(matcher.transactionInfo(transferTx.id().base58).height + 1)

    val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
    tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0

    val reservedBalance = matcher.reservedBalance(matcher.privateKey)
    reservedBalance.getOrElse(asset.id().base58, 0) shouldBe toTransfer * 2

    matcher.expectIncorrectOrderPlacement(order, 400, "OrderRejected")

//    matcher.signedBroadcast(reverseTransferTx.json(), waitForTx = true)
  }

  "ExchangeTransaction from different matcher" in {
    val now   = System.currentTimeMillis()
    val price = Order.PriceConstant
    val exchangeTx = ExchangeTransactionV2
      .create(
        matcher = alice.privateKey,
        buyOrder = OrderV2.buy(
          sender = alice.privateKey,
          matcher = alice.publicKey,
          pair = wavesUsdPair,
          amount = toTransfer,
          price = price,
          timestamp = now,
          expiration = now + 10.days.toMillis,
          matcherFee = 3000000
        ),
        sellOrder = OrderV2.sell(
          sender = matcher.privateKey,
          matcher = alice.publicKey,
          pair = wavesUsdPair,
          amount = toTransfer,
          price = price,
          timestamp = now,
          expiration = now + 10.days.toMillis,
          matcherFee = 3000000
        ),
        amount = toTransfer,
        price = price,
        buyMatcherFee = 3000000,
        sellMatcherFee = 3000000,
        fee = 3000000,
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()
    matcher.signedBroadcast(exchangeTx.json(), waitForTx = true)
    nodes.waitForHeight(matcher.transactionInfo(exchangeTx.id().base58).height + 1)

    val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
    tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0

    val reservedBalance = matcher.reservedBalance(matcher.privateKey)
    reservedBalance.getOrElse(asset.id().base58, 0) shouldBe toTransfer * 3

    matcher.expectIncorrectOrderPlacement(order, 400, "OrderRejected")
  }
}

object BlockedAssetsTestSuite {
  private def ConfigOverrides = ConfigFactory.parseString(s"""waves.blockchain.custom.functionality.tracked-assets {
                                                             |  allowed-sources = [${Configs.head.getString("waves.matcher.account")}]
                                                             |  assets = ["${IssueUsdTx.id().base58}"]
                                                             |}""".stripMargin)
}
