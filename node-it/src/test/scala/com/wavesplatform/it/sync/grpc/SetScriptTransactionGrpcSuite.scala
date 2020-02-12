package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient, SetScriptTransactionData, SignedTransaction, TransferTransactionData, Transaction => PBTransaction}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import io.grpc.Status.Code

class SetScriptTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  test("able to set script to account (multisig)") {
    val scriptText = s"""
        match tx {
          case t: Transaction => {
            let A = base58'${ByteStr(secondAcc.publicKey)}'
            let B = base58'${ByteStr(thirdAcc.publicKey)}'
            let AC = sigVerify(tx.bodyBytes,tx.proofs[0],A)
            let BC = sigVerify(tx.bodyBytes,tx.proofs[1],B)
            AC && BC
          }
          case _ => false
        }
      """.stripMargin

    val script = ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
    val scriptComplexity = Script.estimate(Script.fromBase64String(script.bytes().base64).explicitGet(), ScriptEstimatorV3).explicitGet()
    val setScriptTx = sender.grpc.setScript(firstAcc, Some(script), setScriptFee, waitForTx = true)
    val setScriptTxId = PBTransactions.vanilla(setScriptTx).explicitGet().id().toString

    val scriptInfo = sender.grpc.scriptInfo(firstAddress)

    scriptInfo.script.map(PBTransactions.toVanillaScript) should contain(script)
    scriptInfo.scriptText shouldBe script.expr.toString
    scriptInfo.complexity shouldBe scriptComplexity

    sender.grpc.getTransaction(setScriptTxId).getTransaction.getSetScript.script.get shouldBe PBTransactions.toPBScript(script)
  }

  test("not able to broadcast tx from scripted acc if that is not allowed by account-script") {
    assertGrpcError(
      sender.grpc.broadcastTransfer(firstAcc, recipient = Recipient().withPublicKeyHash(thirdAddress), amount = transferAmount, fee = minFee + smartFee),
      "Transaction is not allowed by account-script",
      Code.INVALID_ARGUMENT
    )
  }

  test("able to broadcast tx if that is allowed by account-script") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress).available
    val thirdBalance = sender.grpc.wavesBalance(thirdAddress).available
    val transferFee = minFee + smartFee

    val unsignedTransfer = PBTransaction(
      chainId = AddressScheme.current.chainId,
      senderPublicKey = ByteString.copyFrom(firstAcc.publicKey),
      fee = Some(Amount.of(ByteString.EMPTY, transferFee)),
      timestamp = System.currentTimeMillis(),
      version = 2,
      data = PBTransaction.Data.Transfer(TransferTransactionData.of(
        recipient = Some(Recipient().withPublicKeyHash(thirdAddress)),
        amount = Some(Amount.of(ByteString.EMPTY, transferAmount)),
        None
      ))
    )
    val sig1 = ByteString.copyFrom(crypto.sign(secondAcc, PBTransactions.vanilla(SignedTransaction(Some(unsignedTransfer))).explicitGet().bodyBytes()))
    val sig2 = ByteString.copyFrom(crypto.sign(thirdAcc, PBTransactions.vanilla(SignedTransaction(Some(unsignedTransfer))).explicitGet().bodyBytes()))

    sender.grpc.broadcast(unsignedTransfer, Seq(sig1, sig2), waitForTx = true)

    sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance - transferAmount - transferFee
    sender.grpc.wavesBalance(thirdAddress).available shouldBe thirdBalance + transferAmount
  }

  test("able to clear script from scripted account") {
    val unsignedSetScript = PBTransaction(
      chainId = AddressScheme.current.chainId,
      senderPublicKey = ByteString.copyFrom(firstAcc.publicKey),
      fee = Some(Amount.of(ByteString.EMPTY, setScriptFee + smartFee)),
      timestamp = System.currentTimeMillis(),
      version = 1,
      data = PBTransaction.Data.SetScript(SetScriptTransactionData())
    )
    val sig1 = ByteString.copyFrom(crypto.sign(secondAcc, PBTransactions.vanilla(SignedTransaction(Some(unsignedSetScript))).explicitGet().bodyBytes()))
    val sig2 = ByteString.copyFrom(crypto.sign(thirdAcc, PBTransactions.vanilla(SignedTransaction(Some(unsignedSetScript))).explicitGet().bodyBytes()))

    sender.grpc.broadcast(unsignedSetScript, Seq(sig1, sig2), waitForTx = true)

    val scriptInfo = sender.grpc.scriptInfo(firstAddress)
    scriptInfo.script shouldBe empty
    scriptInfo.scriptText shouldBe ""
    scriptInfo.complexity shouldBe 0L

    val firstBalance = sender.grpc.wavesBalance(firstAddress).available
    val secondBalance = sender.grpc.wavesBalance(secondAddress).available

    sender.grpc.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), transferAmount, minFee, waitForTx = true)

    sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance - transferAmount - minFee
    sender.grpc.wavesBalance(secondAddress).available shouldBe secondBalance + transferAmount
  }

  test("not able to broadcast tx from scripted acc if tx fee doesn't include smart fee") {
    val script = ScriptCompiler(s"true", isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
    sender.grpc.setScript(firstAcc, Some(script), setScriptFee, waitForTx = true)

    val firstBalance = sender.grpc.wavesBalance(firstAddress).available
    val firstEffBalance = sender.grpc.wavesBalance(firstAddress).effective
    val thirdBalance = sender.grpc.wavesBalance(thirdAddress).available
    val thirdEffBalance = sender.grpc.wavesBalance(thirdAddress).effective

    assertGrpcError(
    sender.grpc.broadcastTransfer(firstAcc, recipient = Recipient().withPublicKeyHash(thirdAddress), amount = transferAmount, fee = minFee + smartFee - 1),
      "Transaction sent from smart account",
      Code.INVALID_ARGUMENT
    )

    sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance
    sender.grpc.wavesBalance(firstAddress).effective shouldBe firstEffBalance
    sender.grpc.wavesBalance(thirdAddress).available shouldBe thirdBalance
    sender.grpc.wavesBalance(thirdAddress).effective shouldBe thirdEffBalance
  }
}
