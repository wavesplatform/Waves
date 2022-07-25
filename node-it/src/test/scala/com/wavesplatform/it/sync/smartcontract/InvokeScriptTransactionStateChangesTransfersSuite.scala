package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptTransactionStateChangesTransfersSuite extends BaseTransactionSuite with CancelAfterFailure {
  private def dApp               = firstKeyPair
  private def callerAndRecipient = secondKeyPair

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    val script = ScriptCompiler
      .compile(
        """
          |{-# STDLIB_VERSION 4 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable(inv)
          |func sendToCaller(amount: Int) = [ScriptTransfer(inv.caller, amount, unit)]
          |
      """.stripMargin,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1
      .bytes()
      .base64
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true)
  }

  test("payment value higher than transfer") {
    val pamentAmount   = 2
    val transferAmount = 1

    val invokeScriptTx = sender.invokeScript(
      callerAndRecipient,
      dApp.toAddress.toString,
      func = Some("sendToCaller"),
      args = List(CONST_LONG(transferAmount)),
      payment = Seq(Payment(pamentAmount, Waves)),
      fee = 1.waves,
      waitForTx = true
    )
    nodes.waitForHeightAriseAndTxPresent(invokeScriptTx._1.id)
    val txStateChanges = sender.stateChanges(invokeScriptTx._1.id)

    val transferCountOpt            = txStateChanges.stateChanges.map(_.transfers.size)
    val firstTransferAddrOpt        = txStateChanges.stateChanges.map(_.transfers.head.address)
    val firstTransferAssetAmountOpt = txStateChanges.stateChanges.map(_.transfers.head.amount)

    transferCountOpt shouldBe Some(1)
    firstTransferAddrOpt shouldBe Some(callerAndRecipient.toAddress.toString)
    firstTransferAssetAmountOpt shouldBe Some(transferAmount)
  }

  test("payment equal to transfer") {
    val pamentAmount   = 3
    val transferAmount = 3

    val invokeScriptTx = sender.invokeScript(
      callerAndRecipient,
      dApp.toAddress.toString,
      func = Some("sendToCaller"),
      args = List(CONST_LONG(transferAmount)),
      payment = Seq(Payment(pamentAmount, Waves)),
      fee = 1.waves,
      waitForTx = true
    )
    nodes.waitForHeightAriseAndTxPresent(invokeScriptTx._1.id)
    val txStateChanges = sender.stateChanges(invokeScriptTx._1.id)

    val transferCountOpt            = txStateChanges.stateChanges.map(_.transfers.size)
    val firstTransferAddrOpt        = txStateChanges.stateChanges.map(_.transfers.head.address)
    val firstTransferAssetAmountOpt = txStateChanges.stateChanges.map(_.transfers.head.amount)

    transferCountOpt shouldBe Some(1)
    firstTransferAddrOpt shouldBe Some(callerAndRecipient.toAddress.toString)
    firstTransferAssetAmountOpt shouldBe Some(transferAmount)
  }

  test("payment value lower than transfer") {
    val paymentAmount  = 1
    val transferAmount = 4

    val invokeScriptTx = sender.invokeScript(
      callerAndRecipient,
      dApp.toAddress.toString,
      func = Some("sendToCaller"),
      args = List(CONST_LONG(transferAmount)),
      payment = Seq(Payment(paymentAmount, Waves)),
      fee = 1.waves,
      waitForTx = true
    )
    nodes.waitForHeightAriseAndTxPresent(invokeScriptTx._1.id)
    val txStateChanges = sender.stateChanges(invokeScriptTx._1.id)

    val transferCountOpt            = txStateChanges.stateChanges.map(_.transfers.size)
    val firstTransferAddrOpt        = txStateChanges.stateChanges.map(_.transfers.head.address)
    val firstTransferAssetAmountOpt = txStateChanges.stateChanges.map(_.transfers.head.amount)

    transferCountOpt shouldBe Some(1)
    firstTransferAddrOpt shouldBe Some(callerAndRecipient.toAddress.toString)
    firstTransferAssetAmountOpt shouldBe Some(transferAmount)
  }

  test("zero transfer amount") {
    val paymentAmount  = 1
    val transferAmount = 0

    val invokeScriptTx = sender.invokeScript(
      callerAndRecipient,
      dApp.toAddress.toString,
      func = Some("sendToCaller"),
      args = List(CONST_LONG(transferAmount)),
      payment = Seq(Payment(paymentAmount, Waves)),
      fee = 1.waves,
      waitForTx = true
    )
    nodes.waitForHeightAriseAndTxPresent(invokeScriptTx._1.id)
    val txStateChanges = sender.stateChanges(invokeScriptTx._1.id)

    val transferCountOpt            = txStateChanges.stateChanges.map(_.transfers.size)
    val firstTransferAddrOpt        = txStateChanges.stateChanges.map(_.transfers.head.address)
    val firstTransferAssetAmountOpt = txStateChanges.stateChanges.map(_.transfers.head.amount)

    transferCountOpt shouldBe Some(1)
    firstTransferAddrOpt shouldBe Some(callerAndRecipient.toAddress.toString)
    firstTransferAssetAmountOpt shouldBe Some(transferAmount)
  }
}
