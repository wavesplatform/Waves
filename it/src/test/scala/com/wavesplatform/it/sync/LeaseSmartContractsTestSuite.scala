package com.wavesplatform.it.sync

import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import org.scalatest.CancelAfterFailure
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.utils.dummyTypeCheckerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber
import scorex.account.{AddressScheme, PrivateKeyAccount}
import scorex.transaction.Proofs
import scorex.transaction.lease.{LeaseCancelTransactionV2, LeaseTransactionV2}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1

class LeaseSmartContractsTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  private def pkFromAddress(address: String) = PrivateKeyAccount.fromSeed(sender.seed(address)).right.get

  private val acc0 = pkFromAddress(firstAddress)
  private val acc1 = pkFromAddress(secondAddress)
  private val acc2 = pkFromAddress(thirdAddress)

  private val transferAmount: Long = 1.waves
  private val fee: Long            = 0.001.waves

  test("set contract, make leasing and cancel leasing") {
    val (balance1, eff1) = notMiner.accountBalances(acc0.address)
    val (balance2, eff2) = notMiner.accountBalances(thirdAddress)

    val txId = sender.transfer(sender.address, acc0.address, 10 * transferAmount, fee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    notMiner.assertBalances(firstAddress, balance1 + 10 * transferAmount, eff1 + 10 * transferAmount)

    val scriptText = {
      val sc = Parser(s"""
        let sigA = base58'${ByteStr(acc0.publicKey)}'
        let sigB = base58'${ByteStr(acc1.publicKey)}'
        let sigC = base58'${ByteStr(acc2.publicKey)}'

        let leaseTx =  ((tx.type == 8) && sigVerify(tx.bodyBytes,tx.proofs[0],sigA) && sigVerify(tx.bodyBytes,tx.proofs[2],sigC))
        let leaseCancelTx = ((tx.type == 9) && sigVerify(tx.bodyBytes,tx.proofs[1],sigA) && sigVerify(tx.bodyBytes,tx.proofs[2],sigB))

        leaseTx || leaseCancelTx
        """.stripMargin).get.value
      assert(sc.size == 1)
      CompilerV1(dummyTypeCheckerContext, sc.head).explicitGet()
    }

    val script = ScriptV1(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, acc0, Some(script), fee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val unsignedLeasing =
      LeaseTransactionV2
        .create(
          2,
          acc0,
          transferAmount,
          fee + 0.2.waves,
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
      sender.signedBroadcast(signedLeasing.json() + ("type" -> JsNumber(LeaseTransactionV2.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(leasingId)

    notMiner.assertBalances(firstAddress, balance1 + 10 * transferAmount - (2 * fee + 0.2.waves), eff1 + 9 * transferAmount - (2 * fee + 0.2.waves))
    notMiner.assertBalances(thirdAddress, balance2, eff2 + transferAmount)

    val unsignedCancelLeasing =
      LeaseCancelTransactionV2
        .create(
          version = 2,
          chainId = AddressScheme.current.chainId,
          sender = acc0,
          leaseId = ByteStr.decodeBase58(leasingId).get,
          fee = fee + 0.2.waves,
          timestamp = System.currentTimeMillis(),
          proofs = Proofs.empty
        )
        .explicitGet()

    val sigLeasingCancelA = ByteStr(crypto.sign(acc0, unsignedCancelLeasing.bodyBytes()))
    val sigLeasingCancelB = ByteStr(crypto.sign(acc1, unsignedCancelLeasing.bodyBytes()))

    val signedLeasingCancel =
      unsignedCancelLeasing.copy(proofs = Proofs(Seq(ByteStr.empty, sigLeasingCancelA, sigLeasingCancelB)))

    val leasingCancelId =
      sender.signedBroadcast(signedLeasingCancel.json() + ("type" -> JsNumber(LeaseCancelTransactionV2.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(leasingCancelId)

    notMiner.assertBalances(firstAddress,
                            balance1 + 10 * transferAmount - (3 * fee + 2 * 0.2.waves),
                            eff1 + 10 * transferAmount - (3 * fee + 2 * 0.2.waves))
    notMiner.assertBalances(thirdAddress, balance2, eff2)

  }
}
