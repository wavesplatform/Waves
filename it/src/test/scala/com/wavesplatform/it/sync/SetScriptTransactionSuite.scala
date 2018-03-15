package com.wavesplatform.it.sync

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.{Parser, TypeChecker}
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs.smart.dummyTypeCheckerContext
import org.scalatest.CancelAfterFailure
import scorex.account.PrivateKeyAccount
import scorex.api.http.assets.SignedSetScriptRequest
import scorex.transaction.smart.{Script, SetScriptTransaction}

import scala.util.Random

class SetScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  def randomPk = PrivateKeyAccount(Array.fill[Byte](32)(Random.nextInt(Byte.MaxValue).toByte))

  test("set 2of2 multisig") {
    val acc1 = randomPk
    val acc2 = randomPk

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
    val setScriptTransaction = SetScriptTransaction.selfSigned(sender.privateKey, Some(script), 0.001.waves, System.currentTimeMillis()).explicitGet()

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
  }
}
