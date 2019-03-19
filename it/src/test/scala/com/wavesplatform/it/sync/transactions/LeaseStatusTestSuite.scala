package com.wavesplatform.it.sync.transactions

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import org.scalatest.CancelAfterFailure
import play.api.libs.json.Json
import com.wavesplatform.it.sync._
import com.wavesplatform.transaction.lease.LeaseTransaction.Status.{Active, Canceled}

class LeaseStatusTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  import LeaseStatusTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  test("verification of leasing status") {
    val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee = minFee).id
    nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)
    val status = getStatus(createdLeaseTxId)
    status shouldBe Active

    val cancelLeaseTxId = sender.cancelLease(firstAddress, createdLeaseTxId, fee = minFee).id
    miner.waitForTransaction(cancelLeaseTxId)
    nodes.waitForHeightArise()
    val status1 = getStatus(createdLeaseTxId)
    status1 shouldBe Canceled
    val sizeActiveLeases = sender.activeLeases(firstAddress).size
    sizeActiveLeases shouldBe 0
  }

  private def getStatus(txId: String): String = {
    val r = sender.get(s"/transactions/info/$txId")
    (Json.parse(r.getResponseBody) \ "status").as[String]

  }
}

object LeaseStatusTestSuite {
  private val blockGenerationOffset = "10000ms"
  import com.wavesplatform.it.NodeConfigs.Default

  private val minerConfig = ConfigFactory.parseString(s"""waves {
       |   miner{
       |      enable = yes
       |      minimal-block-generation-offset = $blockGenerationOffset
       |      quorum = 0
       |      micro-block-interval = 3s
       |      max-transactions-in-key-block = 0
       |   }
       |}
     """.stripMargin)

  private val notMinerConfig = ConfigFactory.parseString(s"""waves {
       |   miner.enable = no
       |   miner.minimal-block-generation-offset = $blockGenerationOffset
       |}
     """.stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1))
  )

}
