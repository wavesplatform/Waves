package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer._
import org.scalatest.CancelAfterFailure

import scala.concurrent.duration._

/*
Scenario:
every month a foundation makes payments from two MassTransactions(type == 11):
1) 80% to users
2) 10% as tax and 10% to bank go after 30sec of payment from step 1)
 */

class MassTransferSmartContractSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val fourthAddress: String = notMiner.createAddress()

  test("airdrop emulation via MassTransfer") {
    val scriptText = s"""
        match tx {
          case ttx: MassTransferTransaction =>
            let commonAmount = (ttx.transfers[0].amount + ttx.transfers[1].amount)
            let totalAmountToUsers = commonAmount == 8000000000
            let totalAmountToGov = commonAmount == 2000000000
            let massTxSize = size(ttx.transfers) == 2

            let accountPK = base58'${ByteStr(notMiner.publicKey)}'
            let accSig = sigVerify(ttx.bodyBytes,ttx.proofs[0],accountPK)

            let txToUsers = (massTxSize && totalAmountToUsers)

            let mTx = transactionById(ttx.proofs[1])

            if (txToUsers && accSig) then true
            else
            if(isDefined(mTx)) then
                match extract(mTx) {
                  case mt2: MassTransferTransaction =>
                    let txToGov = (massTxSize && totalAmountToGov)
                    let txToGovComplete = (ttx.timestamp > mt2.timestamp + 30000) && sigVerify(mt2.bodyBytes,mt2.proofs[0], accountPK)
                    txToGovComplete && accSig && txToGov
                  case _ => false
                }
            else false
        case _ => false
        }
        """.stripMargin

    // set script
    val script = ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    notMiner.setScript(notMiner.address, Some(script), setScriptFee, waitForTx = true).id

    notMiner.addressScriptInfo(notMiner.address).scriptText.isEmpty shouldBe false

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
        .create(Waves, notMiner.publicKey, transfers, currTime, calcMassTransferFee(2) + smartFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()

    val accountSig = ByteStr(crypto.sign(notMiner.privateKey, unsigned.bodyBytes()))
    val signed     = unsigned.copy(proofs = Proofs(Seq(accountSig)))
    val toUsersID  = notMiner.signedBroadcast(signed.json(), waitForTx = true).id

    //make transfer with incorrect time
    val heightBefore = notMiner.height

    val transfersToGov =
      MassTransferTransaction.parseTransfersList(List(Transfer(firstAddress, transferAmount), Transfer(fourthAddress, transferAmount))).explicitGet()

    val unsignedToGov =
      MassTransferTransaction
        .create(Waves, notMiner.publicKey, transfersToGov, currTime, calcMassTransferFee(2) + smartFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()
    val accountSigToGovFail = ByteStr(crypto.sign(notMiner.privateKey, unsignedToGov.bodyBytes()))
    val signedToGovFail     = unsignedToGov.copy(proofs = Proofs(Seq(accountSigToGovFail)))

    assertBadRequestAndResponse(
      notMiner.signedBroadcast(signedToGovFail.json()),
      "Transaction is not allowed by account-script"
    )

    //make correct transfer to government after some time
    notMiner.waitForHeight(heightBefore + 10, 5.minutes)

    val unsignedToGovSecond =
      MassTransferTransaction
        .create(Waves,
                notMiner.publicKey,
                transfersToGov,
                System.currentTimeMillis(),
                calcMassTransferFee(2) + smartFee,
                Array.emptyByteArray,
                Proofs.empty)
        .explicitGet()

    val accountSigToGov = ByteStr(crypto.sign(notMiner.privateKey, unsignedToGovSecond.bodyBytes()))
    val signedToGovGood = unsignedToGovSecond.copy(proofs = Proofs(Seq(accountSigToGov, ByteStr(Base58.tryDecodeWithLimit(toUsersID).get))))
    notMiner.signedBroadcast(signedToGovGood.json(), waitForTx = true).id
  }
}
