package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncGrpcApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.sync.smartcontract.setScrTxSupportedVersions
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{
  PBTransactions,
  Recipient,
  SetScriptTransactionData,
  SignedTransaction,
  TransferTransactionData,
  Transaction as PBTransaction
}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import io.grpc.Status.Code

class SetScriptTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  test("able to set script to account (multisig)") {
    for (v <- setScrTxSupportedVersions) {
      val (contract, contractAddr) = if (v < 2) (firstAcc, firstAddress) else (secondAcc, secondAddress)
      val scriptText =
        s"""
        match tx {
          case _: Transaction => {
            let A = base58'${secondAcc.publicKey}'
            let B = base58'${thirdAcc.publicKey}'
            let AC = sigVerify(tx.bodyBytes,tx.proofs[0],A)
            let BC = sigVerify(tx.bodyBytes,tx.proofs[1],B)
            AC && BC
          }
          case _ => false
        }
      """.stripMargin

      val script      = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1
      val estimatorV3 = ScriptEstimatorV3(fixOverflow = true, overhead = true, letFixes = false)
      val scriptComplexity = Script
        .estimate(
          Script.fromBase64String(script.bytes().base64).explicitGet(),
          estimatorV3,
          fixEstimateOfVerifier = true,
          useContractVerifierLimit = true
        )
        .explicitGet()
      val setScriptTx   = sender.setScript(contract, Right(Some(script)), setScriptFee, waitForTx = true)
      val setScriptTxId = PBTransactions.vanilla(setScriptTx, unsafe = false).explicitGet().id().toString

      val scriptInfo = sender.scriptInfo(contractAddr)

      PBTransactions.toVanillaScript(scriptInfo.scriptBytes) should contain(script)
      scriptInfo.scriptText shouldBe script.expr.toString
      scriptInfo.complexity shouldBe scriptComplexity

      sender.getTransaction(setScriptTxId).getWavesTransaction.getSetScript.script shouldBe PBTransactions.toPBScript(Some(script))
    }
  }

  test("not able to broadcast tx from scripted acc if that is not allowed by account-script") {
    for (v <- setScrTxSupportedVersions) {
      val contract = if (v < 2) firstAcc else secondAcc
      assertGrpcError(
        sender.broadcastTransfer(contract, recipient = Recipient().withPublicKeyHash(thirdAddress), amount = transferAmount, fee = minFee + smartFee),
        "Transaction is not allowed by account-script",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test("able to broadcast tx if that is allowed by account-script") {
    for (v <- setScrTxSupportedVersions) {
      val (contract, contractAddr) = if (v < 2) (firstAcc, firstAddress) else (secondAcc, secondAddress)
      val firstBalance             = sender.wavesBalance(contractAddr).available
      val thirdBalance             = sender.wavesBalance(thirdAddress).available
      val transferFee              = minFee + smartFee

      val unsignedTransfer = PBTransaction(
        chainId = AddressScheme.current.chainId,
        senderPublicKey = ByteString.copyFrom(contract.publicKey.arr),
        fee = Some(Amount.of(ByteString.EMPTY, transferFee)),
        timestamp = System.currentTimeMillis(),
        version = 2,
        data = PBTransaction.Data.Transfer(
          TransferTransactionData.of(
            recipient = Some(Recipient().withPublicKeyHash(thirdAddress)),
            amount = Some(Amount.of(ByteString.EMPTY, transferAmount)),
            ByteString.EMPTY
          )
        )
      )
      val sig1 =
        ByteString.copyFrom(
          crypto
            .sign(
              secondAcc.privateKey,
              PBTransactions
                .vanilla(SignedTransaction(SignedTransaction.Transaction.WavesTransaction(unsignedTransfer)), unsafe = false)
                .explicitGet()
                .bodyBytes()
            )
            .arr
        )
      val sig2 =
        ByteString.copyFrom(
          crypto
            .sign(
              thirdAcc.privateKey,
              PBTransactions
                .vanilla(SignedTransaction(SignedTransaction.Transaction.WavesTransaction(unsignedTransfer)), unsafe = false)
                .explicitGet()
                .bodyBytes()
            )
            .arr
        )

      sender.broadcast(unsignedTransfer, Seq(sig1, sig2), waitForTx = true)
      sender.wavesBalance(contractAddr).available shouldBe firstBalance - transferAmount - transferFee
      sender.wavesBalance(thirdAddress).available shouldBe thirdBalance + transferAmount
    }
  }

  test("able to clear script from scripted account") {
    for (v <- setScrTxSupportedVersions) {
      val (contract, contractAddr) = if (v < 2) (firstAcc, firstAddress) else (secondAcc, secondAddress)
      val unsignedSetScript = PBTransaction(
        chainId = AddressScheme.current.chainId,
        senderPublicKey = ByteString.copyFrom(contract.publicKey.arr),
        fee = Some(Amount.of(ByteString.EMPTY, setScriptFee + smartFee)),
        timestamp = System.currentTimeMillis(),
        version = v,
        data = PBTransaction.Data.SetScript(SetScriptTransactionData())
      )
      val sig1 =
        ByteString.copyFrom(
          crypto
            .sign(
              secondAcc.privateKey,
              PBTransactions
                .vanilla(SignedTransaction(SignedTransaction.Transaction.WavesTransaction(unsignedSetScript)), unsafe = false)
                .explicitGet()
                .bodyBytes()
            )
            .arr
        )
      val sig2 =
        ByteString.copyFrom(
          crypto
            .sign(
              thirdAcc.privateKey,
              PBTransactions
                .vanilla(SignedTransaction(SignedTransaction.Transaction.WavesTransaction(unsignedSetScript)), unsafe = false)
                .explicitGet()
                .bodyBytes()
            )
            .arr
        )

      sender.broadcast(unsignedSetScript, Seq(sig1, sig2), waitForTx = true)

      val scriptInfo = sender.scriptInfo(contractAddr)
      scriptInfo.scriptBytes shouldBe empty
      scriptInfo.scriptText shouldBe ""
      scriptInfo.complexity shouldBe 0L

      val contractBalance = sender.wavesBalance(contractAddr).available
      val thirdBalance    = sender.wavesBalance(thirdAddress).available

      sender.broadcastTransfer(contract, Recipient().withPublicKeyHash(thirdAddress), transferAmount, minFee, waitForTx = true)

      sender.wavesBalance(contractAddr).available shouldBe contractBalance - transferAmount - minFee
      sender.wavesBalance(thirdAddress).available shouldBe thirdBalance + transferAmount
    }
  }

  test("not able to broadcast tx from scripted acc if tx fee doesn't include smart fee") {
    for (v <- setScrTxSupportedVersions) {
      val (contract, contractAddr) = if (v < 2) (firstAcc, firstAddress) else (secondAcc, secondAddress)
      val script                   = ScriptCompiler.compile(s"true", ScriptEstimatorV2).explicitGet()._1
      sender.setScript(contract, Right(Some(script)), setScriptFee, waitForTx = true)

      val contractBalance    = sender.wavesBalance(contractAddr).available
      val contractEffBalance = sender.wavesBalance(contractAddr).effective
      val thirdBalance       = sender.wavesBalance(thirdAddress).available
      val thirdEffBalance    = sender.wavesBalance(thirdAddress).effective

      assertGrpcError(
        sender
          .broadcastTransfer(contract, recipient = Recipient().withPublicKeyHash(thirdAddress), amount = transferAmount, fee = minFee + smartFee - 1),
        "Transaction sent from smart account",
        Code.INVALID_ARGUMENT
      )

      sender.wavesBalance(contractAddr).available shouldBe contractBalance
      sender.wavesBalance(contractAddr).effective shouldBe contractEffBalance
      sender.wavesBalance(thirdAddress).available shouldBe thirdBalance
      sender.wavesBalance(thirdAddress).effective shouldBe thirdEffBalance
    }
  }
}
