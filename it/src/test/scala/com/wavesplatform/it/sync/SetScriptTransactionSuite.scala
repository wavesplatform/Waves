package com.wavesplatform.it.sync

import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.{Parser, TypeChecker}
import com.wavesplatform.state._
import com.wavesplatform.utils.dummyTypeCheckerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber
import scorex.account.PrivateKeyAccount
import scorex.transaction.Proofs
import scorex.transaction.assets.VersionedTransferTransaction
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scala.util.Random

class SetScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private def randomPk = PrivateKeyAccount(Array.fill[Byte](32)(Random.nextInt(Byte.MaxValue).toByte))

  private val acc0 = randomPk
  private val acc1 = randomPk
  private val acc2 = randomPk
  private val acc3 = randomPk

  private val transferAmount: Long = 1.waves
  private val fee: Long            = 0.001.waves

  test("setup acc0 with 1 waves") {
    val tx =
      VersionedTransferTransaction
        .selfSigned(
          version = 2,
          assetId = None,
          sender = sender.privateKey,
          recipient = acc0,
          amount = 3 * transferAmount + 3 * (0.00001.waves + 0.00002.waves), // Script fee
          timestamp = System.currentTimeMillis(),
          feeAmount = fee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(VersionedTransferTransaction.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("set acc0 as 2of2 multisig") {
    val scriptText = {
      val untyped = Parser(s"""

        let A = base58'${ByteStr(acc1.publicKey)}'
        let B = base58'${ByteStr(acc2.publicKey)}'

        let AC = if(sigVerify(tx.bodyBytes,tx.proof0,A)) then 1 else 0
        let BC = if(sigVerify(tx.bodyBytes,tx.proof1,B)) then 1 else 0

         AC + BC == 2

      """.stripMargin).get.value
      TypeChecker(dummyTypeCheckerContext, untyped).explicitGet()
    }

    val script = ScriptV1(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, acc0, Some(script), fee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val acc0ScriptInfo = sender.addressScriptInfo(acc0.address)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
  }

  test("can't send from acc0 using old pk") {
    val tx =
      VersionedTransferTransaction
        .selfSigned(
          version = 2,
          assetId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAmount = fee + 0.00001.waves + 0.00002.waves,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    assertBadRequest(sender.signedBroadcast(tx.json() + ("type" -> JsNumber(VersionedTransferTransaction.typeId.toInt))))
  }

  test("can send from acc0 using multisig of acc1 and acc2") {
    val unsigned =
      VersionedTransferTransaction
        .create(
          version = 2,
          assetId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAmount = fee + 0.00001.waves + 0.00002.waves,
          attachment = Array.emptyByteArray,
          proofs = Proofs.empty
        )
        .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val signed = unsigned.copy(proofs = Proofs(Seq(sig1, sig2)))

    val versionedTransferId =
      sender.signedBroadcast(signed.json() + ("type" -> JsNumber(VersionedTransferTransaction.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(versionedTransferId)
  }

  test("can clear script at acc0") {
    val unsigned = SetScriptTransaction
      .create(
        version = SetScriptTransaction.supportedVersions.head,
        sender = acc0,
        script = None,
        fee = fee + 0.00001.waves + 0.00002.waves,
        timestamp = System.currentTimeMillis(),
        proofs = Proofs.empty
      )
      .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val signed = unsigned.copy(proofs = Proofs(Seq(sig1, sig2)))
    val clearScriptId = sender
      .signedBroadcast(signed.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(clearScriptId)
  }

  test("can send using old pk of acc0") {
    val tx =
      VersionedTransferTransaction
        .selfSigned(
          version = 2,
          assetId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAmount = fee + 0.00001.waves + 0.00002.waves,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    val txId = sender.signedBroadcast(tx.json() + ("type" -> JsNumber(VersionedTransferTransaction.typeId.toInt))).id
    nodes.waitForHeightAriseAndTxPresent(txId)
  }
}
