package com.wavesplatform.it.sync

import com.wavesplatform.it.Node
import com.wavesplatform.it.transactions.BaseTransactionSuite
import org.scalatest.CancelAfterFailure
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.it.api.SyncHttpApi._

class AssetDistributionSuite extends BaseTransactionSuite with CancelAfterFailure {

  val node: Node = nodes.head

  val issuer = node.privateKey

  test("'Asset distribution at height' method works properly") {
    val transferAmount = 1000000
    val issueAmount    = 1000000000

    val addresses     = nodes.map(_.privateKey.toAddress).filter(_ != issuer.toAddress).toList
    val initialHeight = node.height

    val issueTx = node.issue(issuer.address, "TestCoin", "no description", issueAmount, 8, false, issueFee, waitForTx = true).id

    node.massTransfer(
      issuer.address,
      addresses.map(addr => MassTransferTransaction.Transfer(addr.address, transferAmount)),
      minFee + (minFee * addresses.size),
      Some(issueTx),
      waitForTx = true
    )

    node.assetDistribution(issueTx, Some(initialHeight), Some(100)) shouldBe Map.empty

    val assetDis = node
      .assetDistribution(issueTx, Some(node.height), Some(100))

    assetDis should be equals node.assetDistribution(issueTx)

    val issuerAssetDis = assetDis.filterKeys(_ == issuer.address).values

    issuerAssetDis.size shouldBe 1
    issuerAssetDis.head shouldBe (issueAmount - addresses.length * transferAmount)

    val othersAssetDis = assetDis.filterKeys(_ != issuer.address)

    othersAssetDis.values.forall(_ == transferAmount)

    val assetDisFull = node
      .assetDistribution(issueTx, Some(node.height), Some(100), Some(issuer.address))

    assetDisFull.values.forall(_ == transferAmount)
    !assetDisFull.keySet.contains(issuer.address)
  }

}
