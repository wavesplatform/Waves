package com.wavesplatform.it.sync.transactions


import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import play.api.libs.json.{Json}
import scorex.transaction.lease.LeaseTransaction.Status.{Active, Canceled}

class LeaseStatusTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  import LeaseStatusTestSuiteConfig._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferFee = 0.001.waves
  private val leasingAmount = 10.waves


  test("verification of leasing status") {
    val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee = transferFee).id
    nodes.waitForHeightAraiseAndTxPresent(createdLeaseTxId)
    val status = getStatus(createdLeaseTxId)
    status shouldBe Active

    val cancelLesingTxId = sender.cancelLease(firstAddress, createdLeaseTxId, fee = transferFee).id
    notMiner.waitForTransaction(cancelLesingTxId)
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

object LeaseStatusTestSuiteConfig {
  private val blockGenerationOffest = "10000ms"
  import com.wavesplatform.it.NodeConfigs.Default

  private val minerConfig = ConfigFactory.parseString(
    s"""waves {
       |   miner{
       |      enable = yes
       |      minimal-block-generation-offset = $blockGenerationOffest
       |      quorum = 0
       |      micro-block-interval = 3s
       |      max-transactions-in-key-block = 0
       |   }
       |}
     """.stripMargin)


  private val notMinerConfig = ConfigFactory.parseString(
    s"""waves {
       |   miner.enable = no
       |   miner.minimal-block-generation-offset = $blockGenerationOffest
       |}
     """.stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1))
  )

}
