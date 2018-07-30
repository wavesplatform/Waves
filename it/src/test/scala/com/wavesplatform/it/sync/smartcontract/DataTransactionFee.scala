package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{ReportingTestName, WaitForHeight2}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

class DataTransactionFee extends FreeSpec with Matchers with WaitForHeight2 with CancelAfterFailure with ReportingTestName with NodesFromDocker {
  import DataTransactionFee._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def node = nodes.head

  "calc data fee before feature 4, and compare it with fee after feature activation" in {
    val entry1 = IntegerDataEntry("int", 923275292849183L)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Array.tabulate(445)(_.toByte)))
    val entry4 = StringDataEntry("str", "AAA-AAA")

    val tx = DataTransaction
      .create(
        1,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        List(entry1, entry2, entry3, entry4),
        100000,
        1526911531530L,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94" * 1000).get))
      )
      .right
      .get

    node.activationStatus
    val feeBeforeFeature = node.calculateFee(tx.json())

    nodes.waitForHeight(11)

    node.activationStatus
    val feeAfterFeature = node.calculateFee(tx.json())

    assert(feeBeforeFeature.feeAmount != feeAfterFeature.feeAmount)

  }
}

object DataTransactionFee {
  import com.wavesplatform.it.NodeConfigs._
  private val featureConfig = ConfigFactory.parseString(s"""
                                                               |waves {
                                                               |  synchronization.synchronization-timeout = 10s
                                                               |  blockchain.custom.functionality {
                                                               |    pre-activated-features = {
                                                               |    1 = 0
                                                               |    4 = 10
                                                               |    5 = 0
                                                               |    }
                                                               |  }
                                                               |  miner.quorum = 0
                                                               |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    featureConfig.withFallback(Default.head)
  )
}
