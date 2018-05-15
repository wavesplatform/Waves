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
import scorex.transaction.transfer.MassTransferTransaction.Transfer
import scorex.transaction.transfer._

import scala.concurrent.duration._

class MassTransferSmartContractSuite extends BaseTransactionSuite with CancelAfterFailure {
  private def pkFromAddress(address: String) = PrivateKeyAccount.fromSeed(sender.seed(address)).right.get

  private val fourthAddress: String = sender.createAddress()

  private val transferAmount: Long = 1.waves
  private val fee: Long            = 0.001.waves

  /*
  Every month a foundation makes payments which are consists from:
  1) 80% to users
  2) 15% as tax and 5% to bank go after 2 days(option 100sec) of payment from step 1)
   */

  test("airdrop emulation via MassTransfer") {
    val heightBefore = sender.height

    val scriptText = {
      val untyped = Parser(s"""
        let commonAmount = (tx.transfers[0].amount + tx.transfers[1].amount)
        let totalAmountToUsers = commonAmount == 800000000
        let totalAmountToGov = commonAmount == 200000000
        let massTransferType = ((tx.type == 11) && (size(tx.transfers) == 2))

        let accountPK = base58'${ByteStr(sender.publicKey.publicKey)}'
        let accSig = sigVerify(tx.bodyBytes,tx.proofs[0],accountPK)

        let txToUsers = (massTransferType && totalAmountToUsers)
        
        let massTransferTime = if(txToUsers) then Some(tx.timestamp) else None

        let txToGov = (massTransferType && totalAmountToGov)

        let txToGovComplete = if(isDefined(massTransferTime)) then ((tx.timestamp > (extract(massTransferTime) + 100000)) && txToGov) else false

        (txToGovComplete && accSig) || (txToUsers && accSig) || true
        """.stripMargin).get.value
      CompilerV1(dummyTypeCheckerContext, untyped).explicitGet()
    }

    val script = ScriptV1(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, sender.privateKey, Some(script), fee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    sender.addressScriptInfo(sender.address).scriptText.isEmpty shouldBe false

    val transfers =
      MassTransferTransaction
        .parseTransfersList(List(Transfer(thirdAddress, 4 * transferAmount), Transfer(secondAddress, 4 * transferAmount)))
        .right
        .get

    val massTransferFee = 0.004.waves + 0.0005.waves * 4

    val unsigned =
      MassTransferTransaction
        .create(1, None, sender.publicKey, transfers, System.currentTimeMillis(), massTransferFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()

    val accountSig = ByteStr(crypto.sign(sender.publicKey.publicKey, unsigned.bodyBytes()))

    val signed = unsigned.copy(proofs = Proofs(Seq(accountSig)))

    val toUsersID = sender.signedBroadcast(signed.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))).id
    nodes.waitForHeightAriseAndTxPresent(toUsersID)

    val transfersToGov =
      MassTransferTransaction.parseTransfersList(List(Transfer(firstAddress, transferAmount), Transfer(fourthAddress, transferAmount))).right.get

    val unsignedToGov =
      MassTransferTransaction
        .create(1, None, sender.publicKey, transfersToGov, System.currentTimeMillis(), massTransferFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()

    val signedToGov = unsignedToGov.copy(proofs = Proofs(Seq(accountSig)))

    assertBadRequestAndResponse(sender.signedBroadcast(signedToGov.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))),
                                "Reason: TransactionNotAllowedByScript")

    sender.waitForHeight(heightBefore + 10, 2.minutes)

    val massTransferID = sender.signedBroadcast(signedToGov.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(massTransferID)
  }
}
