package com.wavesplatform.it.sync

import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.AssetDistributionPage
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import org.scalatest.CancelAfterFailure

class AssetDistributionSuite extends BaseTransactionSuite with CancelAfterFailure {

  val node: Node = nodes.head

  val issuer = node.privateKey

  test("'Asset distribution at height' method works properly") {
    val transferAmount = 1000000L
    val issueAmount    = 1000000000L

    val addresses     = nodes.map(_.privateKey.toAddress).filter(_ != issuer.toAddress).toList
    val initialHeight = node.height

    nodes.waitForHeightArise()

    val issueTx = node.issue(issuer.address, "TestCoin", "no description", issueAmount, 8, false, issueFee, waitForTx = true).id

    node.massTransfer(
      issuer.address,
      addresses.map(addr => MassTransferTransaction.Transfer(addr.address, transferAmount)),
      minFee + (minFee * addresses.size),
      Some(issueTx),
      waitForTx = true
    )

    nodes.waitForHeightArise()

    val distributionHeight = node.height

    nodes.waitForHeightArise()

    node.assetDistributionAtHeight(issueTx, initialHeight, 100).items shouldBe Map.empty

    val assetDis = node
      .assetDistributionAtHeight(issueTx, distributionHeight, 100)
      .items

    assetDis should be equals node.assetDistribution(issueTx)

    val issuerAssetDis = assetDis.filterKeys(_ == issuer.toAddress).values

    issuerAssetDis.size shouldBe 1
    issuerAssetDis.head shouldBe (issueAmount - addresses.length * transferAmount)

    val othersAssetDis = assetDis.filterKeys(_ != issuer.toAddress)

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

    val issueTx = node.issue(issuer.address, "TestCoin#2", "no description", issueAmount, 8, false, issueFee, waitForTx = true).id

    node
      .massTransfer(
        issuer.address,
        receivers.map(rc => MassTransferTransaction.Transfer(rc.address, 10)).toList,
        minFee + minFee * receivers.length,
        Some(issueTx),
        waitForTx = true
      )

    nodes.waitForHeightArise()

    val distribution = node.assetDistribution(issueTx)

    distribution.size shouldBe (receivers.size + 1)
    distribution(issuer) shouldBe (issueAmount - 10 * receivers.length)

    assert(receivers.forall(rc => distribution(rc) == 10), "Distribution correct")
  }

  test("Correct last page and entry count") {
    val receivers = for (i <- 0 until 50) yield KeyPair(s"receiver#$i".getBytes("UTF-8"))

    val issueTx = node.issue(issuer.address, "TestCoin#2", "no description", issueAmount, 8, false, issueFee, waitForTx = true).id

    node
      .massTransfer(
        issuer.address,
        receivers.map(rc => MassTransferTransaction.Transfer(rc.address, 10)).toList,
        minFee + minFee * receivers.length,
        Some(issueTx),
        waitForTx = true
      )

    nodes.waitForHeightArise()

    val height = node.height

    nodes.waitForHeightArise()

    val pages = distributionPages(issueTx, height, 10)

    assert(pages.last.hasNext == false)
    assert(pages.last.lastItem.nonEmpty)
    assert(pages.length == 6)
    assert(pages.map(_.items.size).sum == 51)
  }

  def distributionPages(asset: String, height: Int, limit: Int): List[AssetDistributionPage] = {
    def _load(acc: List[AssetDistributionPage], maybeAfter: Option[String]): List[AssetDistributionPage] = {
      val page = node.assetDistributionAtHeight(asset, height, limit, maybeAfter)
      if (page.hasNext) _load(page :: acc, page.lastItem.map(_.stringRepr))
      else page :: acc
    }

    _load(Nil, None).reverse
  }
}
