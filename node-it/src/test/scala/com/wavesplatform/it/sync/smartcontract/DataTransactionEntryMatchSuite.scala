package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync.{setScriptFee, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class DataTransactionEntryMatchSuite extends BaseTransactionSuite {
  private val activationHeight = 4

  private def compile(scriptText: String) =
    ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.ContinuationTransaction.id, activationHeight)))
      .withDefault(1)
      .buildNonConflicting()

  private val script =
    compile(
      s"""
         | {-# STDLIB_VERSION 4 #-}
         | {-# CONTENT_TYPE DAPP #-}
         | {-# SCRIPT_TYPE ACCOUNT #-}
         |
         | @Verifier(tx)
         | func verify() = match tx {
         |     case d:DataTransaction =>
         |         d.data.size() == 1 &&
         |         match d.data[0] {
         |           case entry: StringEntry =>
         |             entry.key == "key" &&
         |             entry.value == "value"
         |           case _: IntegerEntry =>
         |             throw("unexpected IntegerEntry")
         |           case _: BinaryEntry =>
         |             throw("unexpected BinaryEntry")
         |           case _: BooleanEntry =>
         |             throw("unexpected BooleanEntry")
         |         }
         |     case _ =>
         |       sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
         | }
       """.stripMargin
    )

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val setScriptId = sender.setScript(firstKeyPair, Some(script), setScriptFee, waitForTx = true).id
    sender.transactionInfo[TransactionInfo](setScriptId).script.get.startsWith("base64:") shouldBe true

    val scriptInfo = sender.addressScriptInfo(firstKeyPair.toAddress.toString)
    scriptInfo.script.isEmpty shouldBe false
    scriptInfo.scriptText.isEmpty shouldBe false
    scriptInfo.script.get.startsWith("base64:") shouldBe true
  }

  test("data transaction validation error due to incorrect RIDE mapping") {
    assertBadRequestAndMessage(
      sendDataTransaction(),
      "Error while executing account-script: Match error"
    )
  }

  test("successful validation of data transaction") {
    sender.waitForHeight(activationHeight)
    sendDataTransaction()
  }

  private def sendDataTransaction() = {
    val data = List(StringDataEntry("key", "value"))
    val fee  = calcDataFee(data, TxVersion.V1) + smartFee
    sender.putData(firstKeyPair, data, version = TxVersion.V1, fee = fee, waitForTx = true)
  }
}
