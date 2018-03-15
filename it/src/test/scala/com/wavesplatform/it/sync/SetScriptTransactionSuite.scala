package com.wavesplatform.it.sync

import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.{Parser, TypeChecker}
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs.smart.dummyTypeCheckerContext
import org.scalatest.CancelAfterFailure
import scorex.account.PrivateKeyAccount
import scorex.api.http.assets.{SignedSetScriptRequest, SignedVersionedTransferRequest}
import scorex.transaction.Proofs
import scorex.transaction.assets.VersionedTransferTransaction
import scorex.transaction.smart.{Script, SetScriptTransaction}

import scala.util.Random

class SetScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private def randomPk = PrivateKeyAccount(Array.fill[Byte](32)(Random.nextInt(Byte.MaxValue).toByte))

  private val acc1 = randomPk
  private val acc2 = randomPk
  private val acc3 = randomPk

  private val transferAmount: Long = 1.waves
  private val fee: Long            = 0.001.waves

  test("set 2of2 multisig") {

    val scriptText = {
      val untyped = Parser(s"""

        let A = base58'${ByteStr(acc1.publicKey)}'
        let B = base58'${ByteStr(acc2.publicKey)}'

        let AC = if(SIGVERIFY(TX.BODYBYTES,TX.PROOFA,A)) then 1 else 0
        let BC = if(SIGVERIFY(TX.BODYBYTES,TX.PROOFB,B)) then 1 else 0

         AC + BC == 2

      """.stripMargin).get.value
      TypeChecker(dummyTypeCheckerContext, untyped).explicitGet()
    }

    val script               = Script(scriptText)
    val setScriptTransaction = SetScriptTransaction.selfSigned(sender.privateKey, Some(script), fee, System.currentTimeMillis()).explicitGet()

    val setScriptId = sender
      .signedSetScript(
        SignedSetScriptRequest(
          senderPublicKey = sender.address,
          script = Some(script.bytes().base58),
          fee = setScriptTransaction.fee,
          timestamp = setScriptTransaction.timestamp,
          proofs = List(setScriptTransaction.proofs.proofs.head.base58)
        ))
      .id

    nodes.waitForHeightAraiseAndTxPresent(setScriptId)

    // get script by account
  }

  test("can't send using old pk ") {
    assertBadRequest(sender.transfer(sender.address, acc3.address, transferAmount, fee, None, None))
  }

  test("can send using multisig") {

    val unsigned =
      VersionedTransferTransaction
        .create(2, None, sender.publicKey, acc3, transferAmount, System.currentTimeMillis(), fee, Array.emptyByteArray, proofs = Proofs.empty)
        .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val request = SignedVersionedTransferRequest(sender.address,
                                                 None,
                                                 acc3.address,
                                                 transferAmount,
                                                 fee,
                                                 System.currentTimeMillis(),
                                                 2,
                                                 None,
                                                 List(sig1, sig2).map(_.base58))

    val versionedTransferId = sender.signedVersionedTransfer(request).id

    nodes.waitForHeightAraiseAndTxPresent(versionedTransferId)
  }
}
