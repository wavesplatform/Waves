package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.it.api.BalanceDetails
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, setScriptFee, transferAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class LeaseSmartContractsTestSuite extends BaseTransactionSuite {
  private def acc0 = firstKeyPair

  private def acc1 = secondKeyPair

  private def acc2 = thirdKeyPair

  test("set contract, make leasing and cancel leasing") {
    val BalanceDetails(_, balance1, _, _, eff1) = miner.balanceDetails(acc0.toAddress.toString)
    val BalanceDetails(_, balance2, _, _, eff2) = miner.balanceDetails(thirdKeyPair.toAddress.toString)

    miner.transfer(miner.keyPair, acc0.toAddress.toString, 10 * transferAmount, minFee, waitForTx = true).id

    miner.assertBalances(firstAddress, balance1 + 10 * transferAmount, eff1 + 10 * transferAmount)

    val scriptText = s"""
        let pkA = base58'${acc0.publicKey}'
        let pkB = base58'${acc1.publicKey}'
        let pkC = base58'${acc2.publicKey}'

        match tx {
          case ltx: LeaseTransaction => sigVerify(ltx.bodyBytes,ltx.proofs[0],pkA) && sigVerify(ltx.bodyBytes,ltx.proofs[2],pkC)
          case lctx : LeaseCancelTransaction => sigVerify(lctx.bodyBytes,lctx.proofs[1],pkA) && sigVerify(lctx.bodyBytes,lctx.proofs[2],pkB)
          case _ => false
        }
        """.stripMargin

    val script = ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    miner.setScript(acc0, Some(script), setScriptFee, waitForTx = true).id

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

    val leasingId =
      miner.signedBroadcast(signedLeasing.json(), waitForTx = true).id

    miner.assertBalances(
      firstAddress,
      balance1 + 10 * transferAmount - (minFee + setScriptFee + 0.2.waves),
      eff1 + 9 * transferAmount - (minFee + setScriptFee + 0.2.waves)
    )
    miner.assertBalances(thirdAddress, balance2, eff2 + transferAmount)

    val unsignedCancelLeasing =
      LeaseCancelTransaction
        .create(
          version = 2.toByte,
          sender = acc0.publicKey,
          leaseId = ByteStr.decodeBase58(leasingId).get,
          fee = minFee + 0.2.waves,
          timestamp = System.currentTimeMillis(),
          proofs = Proofs.empty
        )
        .explicitGet()

    val sigLeasingCancelA = crypto.sign(acc0.privateKey, unsignedCancelLeasing.bodyBytes())
    val sigLeasingCancelB = crypto.sign(acc1.privateKey, unsignedCancelLeasing.bodyBytes())

    val signedLeasingCancel =
      unsignedCancelLeasing.copy(proofs = Proofs(Seq(ByteStr.empty, sigLeasingCancelA, sigLeasingCancelB)))

    miner.signedBroadcast(signedLeasingCancel.json(), waitForTx = true).id

    miner.assertBalances(
      firstAddress,
      balance1 + 10 * transferAmount - (2 * minFee + setScriptFee + 2 * 0.2.waves),
      eff1 + 10 * transferAmount - (2 * minFee + setScriptFee + 2 * 0.2.waves)
    )
    miner.assertBalances(thirdAddress, balance2, eff2)

  }
}
