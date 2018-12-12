package com.wavesplatform.it.sync

import com.wavesplatform.it.Node
import com.wavesplatform.it.transactions.BaseTransactionSuite
import org.scalatest.CancelAfterFailure
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.it.api.SyncHttpApi._
import play.api.libs.json._
import com.wavesplatform.it.util._

class AssetDistributionSuite extends BaseTransactionSuite with CancelAfterFailure {

  val node: Node = nodes.head

  val issuer = node.privateKey

  test("'Asset distribution at height' method works properly") {
    val transferAmount = 0.01.waves

    val addresses     = nodes.map(_.privateKey.toAddress).filter(_ != issuer.toAddress).toList
    val initialHeight = node.height

    val issueTx = node.issue(issuer.address, "TestCoin", "no description", issueAmount, 8, false, issueFee)

    nodes.waitForHeightAriseAndTxPresent(issueTx.id)

    val massTransferTx = node.massTransfer(
      issuer.address,
      addresses.map(addr => MassTransferTransaction.Transfer(addr.address, transferAmount)),
      minFee + (minFee * addresses.size),
      Some(issueTx.id)
    )

    nodes.waitForHeightAriseAndTxPresent(massTransferTx.id)

    val r1 = node
      .get(s"/assets/${issueTx.id}/distribution/$initialHeight/limit/100")
      .getResponseBody()

    Json.parse(r1).as[JsObject].value.toList shouldBe List.empty

    val r2 = node
      .get(s"/assets/${issueTx.id}/distribution/${node.height}/limit/100")
      .getResponseBody

    val jsonResponse = Json.parse(r2)

    (jsonResponse \ issuer.address).as[Long] shouldBe (issueAmount - addresses.length * transferAmount)

    addresses.forall { addr =>
      (jsonResponse \ addr.address).as[Long] == transferAmount
    } shouldBe true
  }

}
