package com.wavesplatform.it.sync.transactions


import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.transactions.BaseTransactionSuite
import play.api.libs.json.{Json}

class LeaseStatusTestSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val transferFee = 0.001.waves
  private val leasingAmount = 10.waves

  private val blockGenerationOffest = "25000ms"

  test("verification of leasing status") {
    nodes.waitForHeightAraise()
    val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee = transferFee).id
    nodes.waitForHeightAraiseAndTxPresent(createdLeaseTxId)
    val status = getStatus(createdLeaseTxId)

    status shouldBe Active
  }

  private def getStatus(txId: String): String = {
    val r = sender.get(s"/transactions/info/$txId")
    (Json.parse(r.getResponseBody) \ "status").as[String]

  }

  private val minerConfig = ConfigFactory.parseString(
    s"""waves {
       |   miner{
       |      enable = yes
       |      minimal-block-generation-offset = $blockGenerationOffest
       |   }
       |}
     """.stripMargin)


  private val notMinerConfig = ConfigFactory.parseString(
    s"""waves {
       |   miner.enable = no
       |   miner.minimal-block-generation-offset = $blockGenerationOffest
       |}
     """.stripMargin)

  override protected def nodeConfigs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1))
  )
}
