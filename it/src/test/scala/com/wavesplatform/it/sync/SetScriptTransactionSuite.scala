package com.wavesplatform.it.sync

import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.{Parser, TypeChecker}
import com.wavesplatform.state2._
import com.wavesplatform.utils.dummyTypeCheckerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.{JsNumber, Json}
import scorex.account.PrivateKeyAccount
import scorex.api.http.assets.SignedSetScriptRequest.jsonFormat
import scorex.api.http.assets.SignedVersionedTransferRequest.jsonFormat
import scorex.api.http.assets.{SignedSetScriptRequest, SignedVersionedTransferRequest}
import scorex.transaction.Proofs
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.VersionedTransferTransaction
import scorex.transaction.smart.{Script, SetScriptTransaction}

import scala.util.Random

class SetScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private def randomPk = PrivateKeyAccount(Array.fill[Byte](32)(Random.nextInt(Byte.MaxValue).toByte))

  private val acc1 = randomPk
  private val acc2 = randomPk
  private val acc3 = randomPk

  private val transferAmount: Long          = 1.waves
  private val fee: Long                     = 0.001.waves
  private val senderPublicKeyString: String = ByteStr(sender.publicKey.publicKey).base58

  test("set 2of2 multisig") {

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

    val script               = Script(scriptText)
    val setScriptTransaction = SetScriptTransaction.selfSigned(sender.privateKey, Some(script), fee, System.currentTimeMillis()).explicitGet()

    val request = SignedSetScriptRequest(
      senderPublicKey = senderPublicKeyString,
      script = Some(script.bytes().base58),
      fee = setScriptTransaction.fee,
      timestamp = setScriptTransaction.timestamp,
      proofs = List(setScriptTransaction.proofs.proofs.head.base58)
    )

    val setScriptId = sender
      .signedBroadcast(Json.toJsObject(request) + ("type" -> JsNumber(TransactionType.SetScriptTransaction.id)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    // get script by account
  }

  test("can't send using old pk ") {
    assertBadRequest(sender.transfer(senderPublicKeyString, acc3.address, transferAmount, fee, None, None))
  }

  test("can send using multisig") {

    val unsigned =
      VersionedTransferTransaction
        .create(
          version = 2,
          assetId = None,
          sender = sender.publicKey,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAmount = fee,
          attachment = Array.emptyByteArray,
          proofs = Proofs.empty
        )
        .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val request = SignedVersionedTransferRequest(
      senderPublicKey = senderPublicKeyString,
      assetId = None,
      recipient = unsigned.recipient.toString,
      amount = unsigned.amount,
      fee = unsigned.fee,
      timestamp = unsigned.timestamp,
      version = unsigned.version,
      attachment = None,
      proofs = List(sig1, sig2).map(_.base58)
    )

    val versionedTransferId =
      sender.signedBroadcast(Json.toJsObject(request) + ("type" -> JsNumber(TransactionType.VersionedTransferTransaction.id))).id

    nodes.waitForHeightAriseAndTxPresent(versionedTransferId)
  }

  test("can clear script") {
    val unsigned = SetScriptTransaction
      .create(sender = sender.privateKey, script = None, fee = fee, timestamp = System.currentTimeMillis(), proofs = Proofs.empty)
      .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val request = SignedSetScriptRequest(
      senderPublicKey = senderPublicKeyString,
      script = None,
      fee = unsigned.fee,
      timestamp = unsigned.timestamp,
      proofs = List(sig1, sig2) map (_.base58)
    )
    val clearScriptId = sender
      .signedBroadcast(Json.toJsObject(request) + ("type" -> JsNumber(TransactionType.SetScriptTransaction.id)))
      .id

    nodes.waitForHeightAriseAndTxPresent(clearScriptId)

  }

  test("can send using old pk again") {
    val transferId = sender
      .transfer(sourceAddress = sender.address, recipient = acc3.address, amount = transferAmount, fee = fee, assetId = None, feeAssetId = None)
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }
}
