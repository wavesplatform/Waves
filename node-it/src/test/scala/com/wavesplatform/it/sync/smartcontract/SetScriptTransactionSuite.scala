package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync.{minFee, setScriptFee, transferAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer._
import org.scalatest.CancelAfterFailure

class SetScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val fourthAddress: String = sender.createAddress()
  private val fifthAddress: String  = sender.createAddress()

  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)
  private val acc3 = pkByAddress(fourthAddress)
  private val acc4 = pkByAddress(fifthAddress)

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    sender.transfer(acc0.toAddress.toString, acc4.toAddress.toString, 10.waves, waitForTx = true)
  }

  test("set acc0 as 2of2 multisig") {
    for (v <- setScrTxSupportedVersions) {
      val contract = if (v < 2) acc0 else acc4
      val scriptText =
        s"""
        match tx {
          case t: Transaction => {
            let A = base58'${acc1.publicKey}'
            let B = base58'${acc2.publicKey}'
            let AC = sigVerify(tx.bodyBytes,tx.proofs[0],A)
            let BC = sigVerify(tx.bodyBytes,tx.proofs[1],B)
            AC && BC
          }
          case _ => false
        }
      """.stripMargin

      val (contractBalance, contractEffBalance) = sender.accountBalances(contract.toAddress.toString)
      val script      = ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1.bytes().base64
      val setScriptId = sender.setScript(contract.toAddress.toString, Some(script), setScriptFee, version = v).id

      nodes.waitForHeightAriseAndTxPresent(setScriptId)

      val acc0ScriptInfo = sender.addressScriptInfo(contract.toAddress.toString)

      acc0ScriptInfo.script.isEmpty shouldBe false
      acc0ScriptInfo.scriptText.isEmpty shouldBe false

      acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true

      sender.transactionInfo[TransactionInfo](setScriptId).script.get.startsWith("base64:") shouldBe true
      sender.assertBalances(contract.toAddress.toString, contractBalance - setScriptFee, contractEffBalance - setScriptFee)
    }
  }

  test("can't send from contract using old pk") {
    for (v <- setScrTxSupportedVersions) {
      val contract = if (v < 2) acc0 else acc4
      val (contractBalance, contractEffBalance) = sender.accountBalances(contract.toAddress.toString)
      val (acc3Balance, acc3EffBalance)         = sender.accountBalances(acc3.toAddress.toString)
      assertApiErrorRaised(
        sender.transfer(
          contract.toAddress.toString,
          recipient = acc3.toAddress.toString,
          assetId = None,
          amount = transferAmount,
          fee = minFee + 0.00001.waves + 0.00002.waves
        )
      )
      sender.assertBalances(contract.toAddress.toString, contractBalance, contractEffBalance)
      sender.assertBalances(acc3.toAddress.toString, acc3Balance, acc3EffBalance)
    }
  }

  test("can send from acc0 using multisig of acc1 and acc2") {
    for (v <- setScrTxSupportedVersions) {
      val contract                              = if (v < 2) acc0 else acc4
      val (contractBalance, contractEffBalance) = sender.accountBalances(contract.toAddress.toString)
      val (acc3Balance, acc3EffBalance)         = sender.accountBalances(acc3.toAddress.toString)
      val unsigned =
        TransferTransaction(
          version = 2.toByte,
          sender = contract.publicKey,
          recipient = acc3.toAddress,
          assetId = Waves,
          amount = 1000,
          feeAssetId = Waves,
          fee = minFee + 0.004.waves,
          attachment = None,
          timestamp = System.currentTimeMillis(),
          proofs = Proofs.empty,
          acc3.toAddress.chainId
        )
      val sig1   = crypto.sign(acc1.privateKey, unsigned.bodyBytes())
      val sig2   = crypto.sign(acc2.privateKey, unsigned.bodyBytes())
      val signed = unsigned.copy(proofs = Proofs(sig1, sig2))
      val transferTxId = sender.signedBroadcast(signed.json()).id

      nodes.waitForHeightAriseAndTxPresent(transferTxId)

      sender.assertBalances(contract.toAddress.toString, contractBalance - 1000 - minFee - 0.004.waves, contractEffBalance - 1000 - minFee - 0.004.waves)
      sender.assertBalances(acc3.toAddress.toString, acc3Balance + 1000, acc3EffBalance + 1000)
    }
  }

  test("can clear script at contract") {
    for (v <- setScrTxSupportedVersions) {
      val contract = if (v < 2) acc0 else acc4
      val (contractBalance, contractEffBalance) = sender.accountBalances(contract.toAddress.toString)
      val unsigned = SetScriptTransaction
        .create(
          version = v,
          sender = contract.publicKey,
          script = None,
          fee = setScriptFee + 0.004.waves,
          timestamp = System.currentTimeMillis(),
          proofs = Proofs.empty
        )
        .explicitGet()
      val sig1 = crypto.sign(acc1.privateKey, unsigned.bodyBytes())
      val sig2 = crypto.sign(acc2.privateKey, unsigned.bodyBytes())

      val signed = unsigned.copy(version = v, proofs = Proofs(Seq(sig1, sig2)))

      val removeScriptId = sender
        .signedBroadcast(signed.json())
        .id

      nodes.waitForHeightAriseAndTxPresent(removeScriptId)

      sender.transactionInfo[TransactionInfo](removeScriptId).script shouldBe None
      sender.addressScriptInfo(contract.toAddress.toString).script shouldBe None
      sender.addressScriptInfo(contract.toAddress.toString).scriptText shouldBe None
      sender.assertBalances(contract.toAddress.toString, contractBalance - setScriptFee - 0.004.waves, contractEffBalance - setScriptFee - 0.004.waves)
    }
  }

  test("can send using old pk of contract") {
    for (v <- setScrTxSupportedVersions) {
      val contract                              = if (v < 2) acc0 else acc4
      val (contractBalance, contractEffBalance) = sender.accountBalances(contract.toAddress.toString)
      val (acc3Balance, acc3EffBalance)         = sender.accountBalances(acc3.toAddress.toString)
      val transferTxId = sender
        .transfer(
          contract.toAddress.toString,
          recipient = acc3.toAddress.toString,
          assetId = None,
          amount = 1000,
          fee = minFee,
          version = 2
        )
        .id

      nodes.waitForHeightAriseAndTxPresent(transferTxId)
      sender.assertBalances(contract.toAddress.toString, contractBalance - 1000 - minFee, contractEffBalance - 1000 - minFee)
      sender.assertBalances(acc3.toAddress.toString, acc3Balance + 1000, acc3EffBalance + 1000)
    }
  }
}
