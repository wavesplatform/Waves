package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.smartcontract.setScrTxSupportedVersions
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient, SetScriptTransactionData, SignedTransaction, TransferTransactionData, Transaction => PBTransaction}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import io.grpc.Status.Code

class SetScriptTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  test("able to set script to account (multisig)") {
    for (v <- setScrTxSupportedVersions) {
      val (contract, contractAddr) = if (v < 2) (firstAcc, firstAddress) else (secondAcc, secondAddress)
      val scriptText =
        s"""
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
      val setScriptTx = sender.grpc.setScript(contract, Some(script), setScriptFee, waitForTx = true)
      val setScriptTxId = PBTransactions.vanilla(setScriptTx).explicitGet().id().toString

      val scriptInfo = sender.grpc.scriptInfo(contractAddr)

      scriptInfo.scriptBytes shouldBe ByteString.copyFrom(Base64.decode(script.bytes().base64))
      scriptInfo.scriptText shouldBe script.expr.toString
      scriptInfo.complexity shouldBe scriptComplexity

      sender.grpc.getTransaction(setScriptTxId).getTransaction.getSetScript.script.get shouldBe PBTransactions.toPBScript(script)
    }
  }

  test("not able to broadcast tx from scripted acc if that is not allowed by account-script") {
    for (v <- setScrTxSupportedVersions) {
      val contract = if (v < 2) firstAcc else secondAcc
      assertGrpcError(
        sender.grpc.broadcastTransfer(contract, recipient = Recipient().withPublicKeyHash(thirdAddress), amount = transferAmount, fee = minFee + smartFee),
        "Transaction is not allowed by account-script",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test("able to broadcast tx if that is allowed by account-script") {
    for (v <- setScrTxSupportedVersions) {
      val (contract, contractAddr) = if (v < 2) (firstAcc, firstAddress) else (secondAcc, secondAddress)
      val firstBalance = sender.grpc.wavesBalance(contractAddr).available
      val thirdBalance = sender.grpc.wavesBalance(thirdAddress).available
      val transferFee = minFee + smartFee

      val unsignedTransfer = PBTransaction(
        chainId = AddressScheme.current.chainId,
        senderPublicKey = ByteString.copyFrom(contract.publicKey),
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

      sender.grpc.wavesBalance(contractAddr).available shouldBe firstBalance - transferAmount - transferFee
      sender.grpc.wavesBalance(thirdAddress).available shouldBe thirdBalance + transferAmount
    }
  }

  test("able to clear script from scripted account") {
    for (v <- setScrTxSupportedVersions) {
      val (contract, contractAddr) = if (v < 2) (firstAcc, firstAddress) else (secondAcc, secondAddress)
      val unsignedSetScript = PBTransaction(
        chainId = AddressScheme.current.chainId,
        senderPublicKey = ByteString.copyFrom(contract.publicKey),
        fee = Some(Amount.of(ByteString.EMPTY, setScriptFee + smartFee)),
        timestamp = System.currentTimeMillis(),
        version = 1,
        data = PBTransaction.Data.SetScript(SetScriptTransactionData())
      )
      val sig1 = ByteString.copyFrom(crypto.sign(secondAcc, PBTransactions.vanilla(SignedTransaction(Some(unsignedSetScript))).explicitGet().bodyBytes()))
      val sig2 = ByteString.copyFrom(crypto.sign(thirdAcc, PBTransactions.vanilla(SignedTransaction(Some(unsignedSetScript))).explicitGet().bodyBytes()))

      sender.grpc.broadcast(unsignedSetScript, Seq(sig1, sig2), waitForTx = true)

      val scriptInfo = sender.grpc.scriptInfo(contractAddr)
      scriptInfo.scriptBytes shouldBe ByteString.EMPTY
      scriptInfo.scriptText shouldBe ""
      scriptInfo.complexity shouldBe 0L

      val firstBalance = sender.grpc.wavesBalance(contractAddr).available
      val secondBalance = sender.grpc.wavesBalance(thirdAddress).available

      sender.grpc.broadcastTransfer(contract, Recipient().withPublicKeyHash(thirdAddress), transferAmount, minFee, waitForTx = true)

      sender.grpc.wavesBalance(contractAddr).available shouldBe firstBalance - transferAmount - minFee
      sender.grpc.wavesBalance(thirdAddress).available shouldBe secondBalance + transferAmount
    }
  }

  test("not able to broadcast tx from scripted acc if tx fee doesn't include smart fee") {
    for (v <- setScrTxSupportedVersions) {
      val (contract, contractAddr) = if (v < 2) (firstAcc, firstAddress) else (secondAcc, secondAddress)
      val script = ScriptCompiler(s"true", isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
      sender.grpc.setScript(contract, Some(script), setScriptFee, waitForTx = true)

      val firstBalance = sender.grpc.wavesBalance(contractAddr).available
      val firstEffBalance = sender.grpc.wavesBalance(contractAddr).effective
      val thirdBalance = sender.grpc.wavesBalance(thirdAddress).available
      val thirdEffBalance = sender.grpc.wavesBalance(thirdAddress).effective

      assertGrpcError(
        sender.grpc.broadcastTransfer(contract, recipient = Recipient().withPublicKeyHash(thirdAddress), amount = transferAmount, fee = minFee + smartFee - 1),
        "Transaction sent from smart account",
        Code.INVALID_ARGUMENT
      )

      sender.grpc.wavesBalance(contractAddr).available shouldBe firstBalance
      sender.grpc.wavesBalance(contractAddr).effective shouldBe firstEffBalance
      sender.grpc.wavesBalance(thirdAddress).available shouldBe thirdBalance
      sender.grpc.wavesBalance(thirdAddress).effective shouldBe thirdEffBalance
    }
  }
}
