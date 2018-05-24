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
import scorex.crypto.encode.Base58
import scorex.transaction.Proofs
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer.MassTransferTransaction.Transfer
import scorex.transaction.transfer._
import scala.concurrent.duration._

/*
Scenario:
every month a foundation makes payments from two MassTransactions(type == 11):
1) 80% to users
2) 10% as tax and 10% to bank go after 30sec of payment from step 1)
 */

class MassTransferSmartContractSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val fourthAddress: String = sender.createAddress()
  private val transferAmount: Long  = 1.waves
  private val fee: Long             = 0.001.waves
  private val massTransferFee       = 0.004.waves + 0.0005.waves * 4

  test("airdrop emulation via MassTransfer") {
    val scriptText = {
      val untyped = Parser(s"""
        let commonAmount = (tx.transfers[0].amount + tx.transfers[1].amount)
        let totalAmountToUsers = commonAmount == 800000000
        let totalAmountToGov = commonAmount == 200000000
        let massTransferType = ((tx.type == 11) && (size(tx.transfers) == 2))

        let accountPK = base58'${ByteStr(sender.publicKey.publicKey)}'
        let accSig = sigVerify(tx.bodyBytes,tx.proofs[0],accountPK)

        let txToUsers = (massTransferType && totalAmountToUsers)

        let mTx = getTransactionById(tx.proofs[1])

        let txToGov = (massTransferType && totalAmountToGov)
        let txToGovComplete = if(isDefined(mTx)) then (((tx.timestamp > (extract(mTx).timestamp) + 30000)) && sigVerify(extract(mTx).bodyBytes,extract(mTx).proofs[0],accountPK)) else false

        (txToGovComplete && accSig && txToGov)  || (txToUsers && accSig)
        """.stripMargin).get.value
      assert(untyped.size == 1)
      CompilerV1(dummyTypeCheckerContext, untyped.head).explicitGet()
    }

    // set script
    val script = ScriptV1(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, sender.privateKey, Some(script), fee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    sender.addressScriptInfo(sender.address).scriptText.isEmpty shouldBe false

    //save time
    val currTime = System.currentTimeMillis()

    //make transfer to users
    val transfers =
      MassTransferTransaction
        .parseTransfersList(List(Transfer(thirdAddress, 4 * transferAmount), Transfer(secondAddress, 4 * transferAmount)))
        .right
        .get

    val unsigned =
      MassTransferTransaction
        .create(1, None, sender.publicKey, transfers, currTime, massTransferFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()

    val accountSig = ByteStr(crypto.sign(sender.privateKey, unsigned.bodyBytes()))
    val signed     = unsigned.copy(proofs = Proofs(Seq(accountSig)))
    val toUsersID  = sender.signedBroadcast(signed.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(toUsersID)

    //make transfer with incorrect time
    val heightBefore = sender.height

    val transfersToGov =
      MassTransferTransaction.parseTransfersList(List(Transfer(firstAddress, transferAmount), Transfer(fourthAddress, transferAmount))).right.get

    val unsignedToGov =
      MassTransferTransaction
        .create(1, None, sender.publicKey, transfersToGov, currTime, massTransferFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()
    val accountSigToGovFail = ByteStr(crypto.sign(sender.privateKey, unsignedToGov.bodyBytes()))
    val signedToGovFail     = unsignedToGov.copy(proofs = Proofs(Seq(accountSigToGovFail)))

    assertBadRequestAndResponse(sender.signedBroadcast(signedToGovFail.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))),
                                "Reason: TransactionNotAllowedByScript")

    //make correct transfer to government after some time
    sender.waitForHeight(heightBefore + 5, 1.minutes)

    val unsignedToGovSecond =
      MassTransferTransaction
        .create(1, None, sender.publicKey, transfersToGov, System.currentTimeMillis(), massTransferFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()

    val accountSigToGov = ByteStr(crypto.sign(sender.privateKey, unsignedToGovSecond.bodyBytes()))
    val signedToGovGood = unsignedToGovSecond.copy(proofs = Proofs(Seq(accountSigToGov, ByteStr(Base58.decode(toUsersID).get))))
    val massTransferID  = sender.signedBroadcast(signedToGovGood.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(massTransferID)
  }
}
