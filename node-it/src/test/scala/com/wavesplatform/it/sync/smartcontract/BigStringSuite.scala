package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, setScriptFee, transferAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.test._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class BigStringSuite extends BaseTransactionSuite with CancelAfterFailure {
  private def acc0 = firstKeyPair
  private def acc1 = secondKeyPair
  private def acc2 = thirdKeyPair

  test("set contract, make leasing and cancel leasing") {
    val (balance1, eff1) = miner.accountBalances(acc0.toAddress.toString)
    val (balance2, eff2) = miner.accountBalances(thirdAddress)

    val txId = sender.transfer(sender.keyPair, acc0.toAddress.toString, 10 * transferAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    miner.assertBalances(firstAddress, balance1 + 10 * transferAmount, eff1 + 10 * transferAmount)

    val scriptText = s"""
        let pkA = base58'${acc0.publicKey}'
        let pkB = base58'${acc1.publicKey}'
        let pkC = base58'${acc2.publicKey}'

        let a0 = "йцукенгшщзхъфывапролдячсмитьбюйцукпврарвараравртавтрвапваппвпавп"
        ${(for (b <- 1 to 20) yield { "let a" + b + "=a" + (b - 1) + "+a" + (b - 1) }).mkString("\n")}
        
        a20 == a0 || match tx {
          case ltx: LeaseTransaction => sigVerify(ltx.bodyBytes,ltx.proofs[0],pkA) && sigVerify(ltx.bodyBytes,ltx.proofs[2],pkC)
          case lctx : LeaseCancelTransaction => sigVerify(lctx.bodyBytes,lctx.proofs[1],pkA) && sigVerify(lctx.bodyBytes,lctx.proofs[2],pkB)
          case _ => false
        }
        """.stripMargin

    val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(1.toByte, acc0, Some(script), setScriptFee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json())
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val unsignedLeasing =
      LeaseTransaction
        .create(
          2.toByte,
          acc0.publicKey,
          acc2.toAddress,
          transferAmount,
          minFee + 0.2.waves,
          System.currentTimeMillis(),
          Proofs.empty
        )
        .explicitGet()

    val sigLeasingA = crypto.sign(acc0.privateKey, unsignedLeasing.bodyBytes())
    val sigLeasingC = crypto.sign(acc2.privateKey, unsignedLeasing.bodyBytes())

    val signedLeasing =
      unsignedLeasing.copy(proofs = Proofs(Seq(sigLeasingA, ByteStr.empty, sigLeasingC)))

    assertBadRequestAndMessage(sender.signedBroadcast(signedLeasing.json()).id, "String size = 32768 exceeds 32767 bytes")

    val leasingId = Base58.encode(unsignedLeasing.id().arr)

    nodes.waitForHeightArise()
    nodes(0).findTransactionInfo(leasingId) shouldBe None

    miner.assertBalances(firstAddress, balance1 + 10 * transferAmount - setScriptFee, eff1 + 10 * transferAmount - setScriptFee)
    miner.assertBalances(thirdAddress, balance2, eff2)

  }
}
