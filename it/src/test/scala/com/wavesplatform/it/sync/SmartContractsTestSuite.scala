package com.wavesplatform.it.sync

import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.utils.dummyTypeCheckerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber
import scorex.account.PrivateKeyAccount
import scorex.transaction.Proofs
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer._

class SmartContractsTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  private def pkFromAddress(address: String) = PrivateKeyAccount.fromSeed(sender.seed(address)).right.get

  private val fourthAddress: String = sender.createAddress

  private val acc0 = pkFromAddress(firstAddress)
  private val acc1 = pkFromAddress(secondAddress)
  private val acc2 = pkFromAddress(thirdAddress)
  private val acc3 = pkFromAddress(fourthAddress)

  private val transferAmount: Long = 1.waves
  private val fee: Long            = 0.001.waves

  test("step1: make leasing, setup smart contract and cancel leasing") {
    val txId = sender.transfer(sender.address, acc0.address, 10 * transferAmount, fee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    val createdLeaseTxId = sender.lease(firstAddress, fourthAddress, transferAmount, fee + 0.2.waves).id
    nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

    val scriptText = {
      val sc = Parser(s"""

        let transferF = base58'${ByteStr(acc1.publicKey)}'
        let transferS = base58'${ByteStr(acc2.publicKey)}'

        let approveTxF = if (((tx.type == 4) || (tx.type == 11) || (tx.type == 13)) && sigVerify(tx.bodyBytes,tx.proof1,transferF)) then 1 else 0
        let approveTxS = if (((tx.type == 4) || (tx.type == 11) || (tx.type == 13)) && sigVerify(tx.bodyBytes,tx.proof2,transferS)) then 1 else 0

        let leasingA = base58'${ByteStr(acc0.publicKey)}'
        let cancelLeasing = if ((tx.type == 9) && sigVerify(tx.bodyBytes,tx.proof0,leasingA)) then true else false

        let changeT = base58'${ByteStr(acc3.publicKey)}'
        let changeScript = if ((tx.type == 13) && sigVerify(tx.bodyBytes,tx.proof3,changeT)) then 1 else 0

        approveTxF + approveTxS == 2 || cancelLeasing || approveTxF + approveTxS + changeScript == 3

      """.stripMargin).get.value
      CompilerV1(dummyTypeCheckerContext, sc).explicitGet()
    }

    val script = ScriptV1(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, acc0, Some(script), fee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

//    commented due to issue https://wavesplatform.atlassian.net/browse/NODE-727
//    val unsignedCancelLeasing =
//      LeaseCancelTransactionV2
//        .create(
//          version = 2,
//          chainId = AddressScheme.current.chainId,
//          sender = acc0,
//          leaseId = ByteStr.decodeBase58(createdLeaseTxId).get,
//          fee = fee + 0.2.waves,
//          timestamp = System.currentTimeMillis(),
//          proofs = Proofs.empty
//        )
//        .explicitGet()
//
//    val sigLeasingCancel = ByteStr(crypto.sign(acc0, unsignedCancelLeasing.bodyBytes()))
//
//    val signedLeasingCancel =
//      unsignedCancelLeasing.copy(proofs = Proofs(Seq(sigLeasingCancel)))
//
//    val leasingCancelId =
//      sender.signedBroadcast(signedLeasingCancel.json() + ("type" -> JsNumber(LeaseCancelTransactionV2.typeId.toInt))).id
//
//    nodes.waitForHeightAriseAndTxPresent(leasingCancelId)
  }

  test("step2: multisiged transfer") {
    val unsignedTransfer =
      TransferTransactionV2
        .create(
          version = 2,
          assetId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = fee + 0.004.waves,
          attachment = Array.emptyByteArray,
          proofs = Proofs.empty
        )
        .explicitGet()
    val sig1Transfer = ByteStr(crypto.sign(acc1, unsignedTransfer.bodyBytes()))
    val sig2Transfer = ByteStr(crypto.sign(acc2, unsignedTransfer.bodyBytes()))

    /* should be:
     val signedTransfer = unsigned.copy(proofs = Proofs(Seq(ByteStr.empty, sig1Transfer, sig2Transfer)))
     but issue https://wavesplatform.atlassian.net/browse/NODE-725 */
    val signedTransfer = unsignedTransfer.copy(proofs = Proofs(Seq(ByteStr("0".getBytes()), sig1Transfer, sig2Transfer)))

    val versionedTransferId =
      sender.signedBroadcast(signedTransfer.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(versionedTransferId)
  }

  test("step3: change smart contract") {
    val newScriptText = {
      val sc = Parser(s"""
        let sig1 = base58'${ByteStr(acc1.publicKey)}'
        let sig2 = base58'${ByteStr(acc2.publicKey)}'

        sigVerify(tx.bodyBytes,tx.proof0,sig1) && sigVerify(tx.bodyBytes,tx.proof1,sig2)

      """.stripMargin).get.value
      CompilerV1(dummyTypeCheckerContext, sc).explicitGet()
    }

    val newScript = ScriptV1(newScriptText).explicitGet()

    val unsignedScriptChange = SetScriptTransaction
      .create(
        version = SetScriptTransaction.supportedVersions.head,
        sender = acc0,
        script = Some(newScript),
        fee = fee + 0.004.waves,
        timestamp = System.currentTimeMillis(),
        proofs = Proofs.empty
      )
      .explicitGet()

    val sig1 = ByteStr(crypto.sign(acc1, unsignedScriptChange.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsignedScriptChange.bodyBytes()))
    val sig3 = ByteStr(crypto.sign(acc3, unsignedScriptChange.bodyBytes()))

    /* should be:
     val signedTransfer = unsigned.copy(proofs = Proofs(Seq(ByteStr.empty, sig1, sig2, sig3)))
     but issue https://wavesplatform.atlassian.net/browse/NODE-725 */
    val signedScriptChange = unsignedScriptChange.copy(proofs = Proofs(Seq(ByteStr("1".getBytes()), sig1, sig2, sig3)))
    val scriptChangeId = sender
      .signedBroadcast(signedScriptChange.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(scriptChangeId)
  }
}
