package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, setScriptFee, transferAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV2, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class LeaseSmartContractsTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  test("set contract, make leasing and cancel leasing") {
    val (balance1, eff1) = miner.accountBalances(acc0.stringRepr)
    val (balance2, eff2) = miner.accountBalances(thirdAddress)

    sender.transfer(sender.address, acc0.stringRepr, 10 * transferAmount, minFee, waitForTx = true).id

    miner.assertBalances(firstAddress, balance1 + 10 * transferAmount, eff1 + 10 * transferAmount)

    val scriptText = s"""
        let pkA = base58'${ByteStr(acc0.publicKey)}'
        let pkB = base58'${ByteStr(acc1.publicKey)}'
        let pkC = base58'${ByteStr(acc2.publicKey)}'

        match tx {
          case ltx: LeaseTransaction => sigVerify(ltx.bodyBytes,ltx.proofs[0],pkA) && sigVerify(ltx.bodyBytes,ltx.proofs[2],pkC)
          case lctx : LeaseCancelTransaction => sigVerify(lctx.bodyBytes,lctx.proofs[1],pkA) && sigVerify(lctx.bodyBytes,lctx.proofs[2],pkB)
          case other => false
        }
        """.stripMargin

    val script = ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(acc0.stringRepr, Some(script), setScriptFee, waitForTx = true).id

    val unsignedLeasing =
      LeaseTransactionV2
        .create(
          acc0,
          transferAmount,
          minFee + 0.2.waves,
          System.currentTimeMillis(),
          acc2,
          Proofs.empty
        )
        .explicitGet()

    val sigLeasingA = ByteStr(crypto.sign(acc0, unsignedLeasing.bodyBytes()))
    val sigLeasingC = ByteStr(crypto.sign(acc2, unsignedLeasing.bodyBytes()))

    val signedLeasing =
      unsignedLeasing.copy(proofs = Proofs(Seq(sigLeasingA, ByteStr.empty, sigLeasingC)))

    val leasingId =
      sender.signedBroadcast(signedLeasing.json(), waitForTx = true).id

    miner.assertBalances(firstAddress,
                         balance1 + 10 * transferAmount - (minFee + setScriptFee + 0.2.waves),
                         eff1 + 9 * transferAmount - (minFee + setScriptFee + 0.2.waves))
    miner.assertBalances(thirdAddress, balance2, eff2 + transferAmount)

    val unsignedCancelLeasing =
      LeaseCancelTransactionV2
        .create(
          chainId = AddressScheme.current.chainId,
          sender = acc0,
          leaseId = ByteStr.decodeBase58(leasingId).get,
          fee = minFee + 0.2.waves,
          timestamp = System.currentTimeMillis(),
          proofs = Proofs.empty
        )
        .explicitGet()

    val sigLeasingCancelA = ByteStr(crypto.sign(acc0, unsignedCancelLeasing.bodyBytes()))
    val sigLeasingCancelB = ByteStr(crypto.sign(acc1, unsignedCancelLeasing.bodyBytes()))

    val signedLeasingCancel =
      unsignedCancelLeasing.copy(proofs = Proofs(Seq(ByteStr.empty, sigLeasingCancelA, sigLeasingCancelB)))

    sender.signedBroadcast(signedLeasingCancel.json(), waitForTx = true).id

    miner.assertBalances(firstAddress,
                         balance1 + 10 * transferAmount - (2 * minFee + setScriptFee + 2 * 0.2.waves),
                         eff1 + 10 * transferAmount - (2 * minFee + setScriptFee + 2 * 0.2.waves))
    miner.assertBalances(thirdAddress, balance2, eff2)

  }
}
