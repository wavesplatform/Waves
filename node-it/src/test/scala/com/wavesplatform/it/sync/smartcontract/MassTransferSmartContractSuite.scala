package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

/*
Scenario:
every month a foundation makes payments from two MassTransactions(type == 11):
1) 80% to users
2) 10% as tax and 10% to bank go after 30sec of payment from step 1)
 */

class MassTransferSmartContractSuite extends BaseTransactionSuite with CancelAfterFailure {
  private lazy val fourthAddress = notMiner.createKeyPair().toAddress

  test("airdrop emulation via MassTransfer") {
    val scriptText = s"""
       |{-# STDLIB_VERSION 2 #-}
       |match tx {
       |  case ttx: MassTransferTransaction =>
       |    let commonAmount = (ttx.transfers[0].amount + ttx.transfers[1].amount)
       |    let totalAmountToUsers = commonAmount == 8000000000
       |    let totalAmountToGov = commonAmount == 2000000000
       |    let massTxSize = size(ttx.transfers) == 2
       |
       |    let accountPK = base58'${notMiner.publicKey}'
       |    let accSig = sigVerify(ttx.bodyBytes,ttx.proofs[0],accountPK)
       |
       |    let txToUsers = (massTxSize && totalAmountToUsers)
       |
       |    let mTx = transactionById(ttx.proofs[1])
       |
       |    if (txToUsers && accSig) then true
       |    else
       |    if(isDefined(mTx)) then
       |        match extract(mTx) {
       |          case mt2: MassTransferTransaction =>
       |            let txToGov = (massTxSize && totalAmountToGov)
       |            let txToGovComplete = (ttx.timestamp > mt2.timestamp + 30000) && sigVerify(mt2.bodyBytes,mt2.proofs[0], accountPK)
       |            txToGovComplete && accSig && txToGov
       |          case _ => false
       |        }
       |    else false
       |case _ => false
       |}
       |""".stripMargin

    // set script
    val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    notMiner.setScript(notMiner.keyPair, Some(script), setScriptFee, waitForTx = true).id

    notMiner.addressScriptInfo(notMiner.address).scriptText.isEmpty shouldBe false

    // make transfer to users
    val signedToUsers = TxHelpers.massTransfer(
      notMiner.keyPair,
      Seq(thirdKeyPair.toAddress -> 4 * transferAmount, secondKeyPair.toAddress -> 4 * transferAmount),
      fee = calcMassTransferFee(2) + smartFee
    )
    val toUsersID = ByteStr.decodeBase58(notMiner.signedBroadcast(signedToUsers.json(), waitForTx = true).id).get

    val transfersToGov = Seq(firstKeyPair.toAddress -> transferAmount, fourthAddress -> transferAmount)
    // make transfer with incorrect time
    val signedToGovWithIncorrectTs = TxHelpers.massTransfer(
      notMiner.keyPair,
      transfersToGov,
      fee = calcMassTransferFee(2) + smartFee,
      timestamp = signedToUsers.timestamp
    )

    assertBadRequestAndResponse(
      notMiner.signedBroadcast(signedToGovWithIncorrectTs.addProof(toUsersID).json()),
      "Transaction is not allowed by account-script"
    )

    // make correct transfer to government with correct timestamp
    val signedToGovGood =
      TxHelpers.massTransfer(notMiner.keyPair, transfersToGov, fee = calcMassTransferFee(2) + smartFee, timestamp = signedToUsers.timestamp + 30_001)
    notMiner.signedBroadcast(signedToGovGood.addProof(toUsersID).json(), waitForTx = true).id
  }
}
