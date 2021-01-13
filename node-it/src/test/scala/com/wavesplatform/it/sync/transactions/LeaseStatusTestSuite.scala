package com.wavesplatform.it.sync.transactions

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.api.http.TransactionsApiRoute.LeaseStatus
import com.wavesplatform.it.RandomKeyPair
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import play.api.libs.json.Json

class LeaseStatusTestSuite extends BaseTransactionSuite {
  import LeaseStatusTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  test("verification of leasing status") {
    val secondAddress    = RandomKeyPair().toAddress.toString
    val createdLeaseTxId = miner.lease(firstKeyPair, secondAddress, leasingAmount, leasingFee = minFee).id
    nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)
    val status = getStatus(createdLeaseTxId)
    status shouldBe LeaseStatus.Active

    val cancelLeaseTxId = miner.cancelLease(firstKeyPair, createdLeaseTxId, fee = minFee).id
    miner.waitForTransaction(cancelLeaseTxId)
    nodes.waitForHeightArise()
    val status1 = getStatus(createdLeaseTxId)
    status1 shouldBe LeaseStatus.Canceled
    val sizeActiveLeases = miner.activeLeasesOld(firstAddress).size
    sizeActiveLeases shouldBe 0
  }

  private def getStatus(txId: String): String = {
    val r = miner.get(s"/transactions/info/$txId")
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
