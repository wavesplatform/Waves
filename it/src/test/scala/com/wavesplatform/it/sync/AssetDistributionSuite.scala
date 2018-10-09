package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.it.Node
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.transactions.BaseTransactionSuite
import org.scalatest.CancelAfterFailure
import com.wavesplatform.it.util._
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.utils.Base58
import play.api.libs.json._

class AssetDistributionSuite extends BaseTransactionSuite with CancelAfterFailure {

  import AssetDistributionSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  val node: Node = nodes.head

  val issuer = node.privateKey

  test("'Asset distribution at height' method works properly") {

    val addresses = nodes.map(_.privateKey.toAddress).filter(_ != issuer.toAddress).toList
    val initialHeight = node.height

    val issueTx = node.issue(issuer.address, "TestCoin", "no description", issueAmount, 8, false, issueFee)

    nodes.waitForHeightAriseAndTxPresent(issueTx.id)

    val massTransferTx = node.massTransfer(
      issuer.address,
      addresses.map(addr => MassTransferTransaction.Transfer(addr.address, transferAmount)),
      transferFee + (transferFee * addresses.size),
      Some(issueTx.id)
    )

    nodes.waitForHeightAriseAndTxPresent(massTransferTx.id)

    val r1 = node
      .get(s"/assets/${issueTx.id}/distribution/$initialHeight")
      .getResponseBody()

    Json.parse(r1).as[JsObject].value.toList shouldBe List.empty

    val r2 = node
      .get(s"/assets/${issueTx.id}/distribution/${node.height}")
      .getResponseBody

    val jsonResponse = Json.parse(r2)

    (jsonResponse \ issuer.address).as[Long] shouldBe (issueAmount - addresses.length * transferAmount)

    addresses.forall { addr =>
      (jsonResponse \ addr.address).as[Long] == transferAmount
    } shouldBe true
  }

}

object AssetDistributionSuite {
  case class Balance(address: String, balance: Long)

  val Configs: Seq[Config] = Default.take(4)

  val transferAmount: Long = 100000
  val transferFee: Long    = 100000
  val issueAmount: Long    = 100000000
  val issueFee             = 1.waves
}
