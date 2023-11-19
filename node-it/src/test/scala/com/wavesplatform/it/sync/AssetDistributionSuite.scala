package com.wavesplatform.it.sync

import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.AssetDistributionPage
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import org.scalatest.CancelAfterFailure

import scala.concurrent.duration.*

class AssetDistributionSuite extends BaseTransactionSuite with CancelAfterFailure {

  lazy val node: Node = nodes.head

  private lazy val issuer = node.keyPair

  test("'Asset distribution at height' method works properly") {
    val transferAmount = 1000000L
    val issueAmount    = 1000000000L

    val addresses     = nodes.map(_.keyPair.toAddress).filter(_ != issuer.toAddress).toList
    val initialHeight = node.height

    nodes.waitForHeightArise()

    val issueTx = node.issue(issuer, "TestCoin", "no description", issueAmount, 8, reissuable = false, issueFee, waitForTx = true).id

    node.massTransfer(
      issuer,
      addresses.map(addr => MassTransferTransaction.Transfer(addr.toString, transferAmount)),
      minFee + (minFee * addresses.size),
      assetId = Some(issueTx),
      waitForTx = true
    )

    nodes.waitForHeightArise()

    val distributionHeight = node.height

    nodes.waitForHeightArise()

    node.assetDistributionAtHeight(issueTx, initialHeight, 100).items shouldBe Map.empty

    val assetDis = node
      .assetDistributionAtHeight(issueTx, distributionHeight, 100)
      .items

    val issuerAssetDis = assetDis.view.filterKeys(_ == issuer.toAddress).values

    assetDis should be equals node.assetDistribution(issueTx)

    issuerAssetDis.size shouldBe 1
    issuerAssetDis.head shouldBe (issueAmount - addresses.length * transferAmount)

    val othersAssetDis = assetDis.view.filterKeys(_ != issuer.toAddress)

    assert(othersAssetDis.values.forall(_ == transferAmount))

    val assetDisFull =
      distributionPages(issueTx, distributionHeight, 100)
        .flatMap(_.items.toList)
        .filterNot(_._1 == issuer.toAddress)

    assert(assetDisFull.forall(_._2 == transferAmount))

    assertBadRequestAndMessage(
      node.assetDistributionAtHeight(issueTx, node.height, 10),
      "Using 'assetDistributionAtHeight' on current height can lead to inconsistent result",
      400
    )
  }

  test("'Asset distribution' works properly") {
    val receivers = for (i <- 0 until 10) yield KeyPair(s"receiver#$i".getBytes("UTF-8"))

    val issueTx = node.issue(issuer, "TestCoin#2", "no description", issueAmount, 8, reissuable = false, issueFee, waitForTx = true).id

    node
      .massTransfer(
        issuer,
        receivers.map(rc => MassTransferTransaction.Transfer(rc.toAddress.toString, 10)).toList,
        minFee + minFee * receivers.length,
        assetId = Some(issueTx),
        waitForTx = true
      )

    nodes.waitForHeightArise()

    val distribution = node.assetDistribution(issueTx)

    distribution.size shouldBe (receivers.size + 1)
    distribution(issuer.toAddress) shouldBe (issueAmount - 10 * receivers.length)

    assert(receivers.forall(rc => distribution(rc.toAddress) == 10), "Distribution correct")
  }

  test("Correct last page and entry count") {
    val receivers = for (i <- 0 until 50) yield KeyPair(s"receiver#$i".getBytes("UTF-8"))

    val issueTx = node.issue(issuer, "TestCoin#2", "no description", issueAmount, 8, reissuable = false, issueFee, waitForTx = true).id

    node
      .massTransfer(
        issuer,
        receivers.map(rc => MassTransferTransaction.Transfer(rc.toAddress.toString, 10)).toList,
        minFee + minFee * receivers.length,
        assetId = Some(issueTx),
        waitForTx = true
      )

    nodes.waitForHeightArise()

    val height = node.height

    nodes.waitForHeightArise()

    val pages = distributionPages(issueTx, height, 10)

    assert(!pages.last.hasNext)
    assert(pages.last.lastItem.nonEmpty)
    assert(pages.length == 6)
    assert(pages.map(_.items.size).sum == 51)
  }

  test("Unlimited list") {
    val assetId = node.issue(issuer, "TestCoin#2", "no description", issueAmount, 8, reissuable = false, issueFee, waitForTx = true).id

    val receivers = for (i <- 0 until 2000) yield KeyPair(s"receiver#$i".getBytes("UTF-8"))

    val transfers = receivers.map { r => MassTransferTransaction.Transfer(r.toAddress.toString, 10L) }.toList

    transfers.grouped(100).foreach { t =>
      node.massTransfer(issuer, t, minFee + t.length * minFee, assetId = Some(assetId))
    }

    node.waitFor("empty utx")(_.utxSize, (_: Int) == 0, 1 second)
    nodes.waitForHeightArise()

    val list = node.assetDistribution(assetId)
    list should have size 2001
  }

  def distributionPages(asset: String, height: Int, limit: Int): List[AssetDistributionPage] = {
    def _load(acc: List[AssetDistributionPage], maybeAfter: Option[String]): List[AssetDistributionPage] = {
      val page = node.assetDistributionAtHeight(asset, height, limit, maybeAfter)
      if (page.hasNext) _load(page :: acc, page.lastItem.map(_.toString))
      else page :: acc
    }

    _load(Nil, None).reverse
  }
}
