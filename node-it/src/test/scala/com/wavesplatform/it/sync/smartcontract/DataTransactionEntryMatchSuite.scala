package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
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
  private val activationHeight = 5

  private def compile(scriptText: String) =
    ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.SynchronousCalls.id, activationHeight)))
      .withDefault(1)
      .buildNonConflicting()

  private def script(dApp: Boolean) =
    compile(
      s"""
         | {-# STDLIB_VERSION 4       #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         | {-# CONTENT_TYPE ${if (dApp) "DAPP" else "EXPRESSION" }  #-}
         |
         | ${if (dApp) "@Verifier(tx) \n func verify() = " else ""}
         | match tx {
         |   case d:DataTransaction =>
         |       d.data.size() == 1 &&
         |       match d.data[0] {
         |         case entry: StringEntry =>
         |           entry.key == "key" &&
         |           entry.value == "value"
         |         case _: IntegerEntry =>
         |           throw("unexpected IntegerEntry")
         |         case _: BinaryEntry =>
         |           throw("unexpected BinaryEntry")
         |         case _: BooleanEntry =>
         |           throw("unexpected BooleanEntry")
         |         case _: DeleteEntry =>
         |           throw("unexpected DeleteEntry")
         |       }
         |   case _ =>
         |     sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
         | }
       """.stripMargin
    )

  private lazy val dAppVerifier: KeyPair      = firstKeyPair
  private lazy val accountExpression: KeyPair = secondKeyPair

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val setDApp = sender.setScript(dAppVerifier, Some(script(dApp = true)), setScriptFee, waitForTx = true).id
    sender.transactionInfo[TransactionInfo](setDApp).script.get.startsWith("base64:") shouldBe true

    val dAppInfo = sender.addressScriptInfo(dAppVerifier.toAddress.toString)
    dAppInfo.script.isEmpty shouldBe false
    dAppInfo.scriptText.isEmpty shouldBe false
    dAppInfo.script.get.startsWith("base64:") shouldBe true

    val setAccountExpression = sender.setScript(accountExpression, Some(script(dApp = false)), setScriptFee, waitForTx = true).id
    sender.transactionInfo[TransactionInfo](setAccountExpression).script.get.startsWith("base64:") shouldBe true

    val accountExpressionInfo = sender.addressScriptInfo(accountExpression.toAddress.toString)
    accountExpressionInfo.script.isEmpty shouldBe false
    accountExpressionInfo.scriptText.isEmpty shouldBe false
    accountExpressionInfo.script.get.startsWith("base64:") shouldBe true
  }

  test("successful validation of data transaction for account expression") {
    sendDataTransaction(accountExpression)
  }

  test("data transaction validation error due to incorrect RIDE mapping for DApp verifier") {
    assertBadRequestAndMessage(
      sendDataTransaction(dAppVerifier),
      "Error while executing account-script: Match error"
    )
  }

  test("successful validation of data transaction for DApp verifier after activation of fix") {
    sender.waitForHeight(activationHeight)
    sendDataTransaction(dAppVerifier)
  }

  private def sendDataTransaction(address: KeyPair) = {
    val data = List(StringDataEntry("key", "value"))
    val fee  = calcDataFee(data, TxVersion.V1) + smartFee
    sender.putData(address, data, version = TxVersion.V1, fee = fee, waitForTx = true)
  }
}
