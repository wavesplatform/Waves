package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.{BaseFreeSpec, WaitForHeight2}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.test._
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class UTXAllowance extends BaseFreeSpec with WaitForHeight2 {
  import UTXAllowance._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def nodeA = nodes.head
  private def nodeB = nodes.last

  "create two nodes with scripted accounts and check UTX" in {
    val accounts = List(nodeA, nodeB).map(i => {

      val acc = i.createKeyPair()

      i.transfer(i.keyPair, acc.toAddress.toString, 10.waves, 0.005.waves, None, waitForTx = true)

      val scriptText = s"""true""".stripMargin
      val script     = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
      i.setScript(acc, Some(script), setScriptFee, waitForTx = true)

      acc
    })

    assertBadRequestAndMessage(
      nodeA
        .transfer(
          accounts.head,
          recipient = accounts.head.toAddress.toString,
          assetId = None,
          amount = 1.waves,
          fee = minFee + 0.004.waves,
          version = 2
        ),
      "transactions from scripted accounts are denied from UTX pool"
    )

    val txBId =
      nodeB
        .transfer(
          accounts(1),
          recipient = accounts(1).toAddress.toString,
          assetId = None,
          amount = 1.01.waves,
          fee = minFee + 0.004.waves,
          version = 2
        )
        .id

    nodes.waitForHeightArise()
    nodeA.findTransactionInfo(txBId) shouldBe None
  }

}

object UTXAllowance {
  import com.wavesplatform.it.NodeConfigs._
  private val FirstNode = ConfigFactory.parseString(s"""
                                                       |waves {
                                                       |  utx.allow-transactions-from-smart-accounts = false
                                                       |  miner {
                                                       |      quorum = 0
                                                       |      enable = yes
                                                       |  }
                                                       |}""".stripMargin)

  private val SecondNode = ConfigFactory.parseString(s"""
                                                        |waves {
                                                        |  utx.allow-transactions-from-smart-accounts = true
                                                        |  miner {
                                                        |      enable = no
                                                        |  }
                                                        |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    FirstNode.withFallback(Default.head),
    SecondNode.withFallback(Default(1))
  )

}
