package com.wavesplatform.state.diffs.freecall

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{TestTime, TransactionGen}
import org.scalatest.EitherValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class FreeCallTest extends PropSpec with ScalaCheckPropertyChecks with TransactionGen with WithDomain with EitherValues {

  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def expr(invoker: KeyPair, fee: Long) =
    TestCompiler(V6).compileFreeCall(
      s"""
         | let address   = Address(base58'${invoker.toAddress}')
         | let publicKey = base58'${invoker.publicKey}'
         | let check =
         |   this                    == address   &&
         |   i.caller                == address   &&
         |   i.originCaller          == address   &&
         |   i.callerPublicKey       == publicKey &&
         |   i.originCallerPublicKey == publicKey &&
         |   i.fee                   == $fee      &&
         |   i.payments              == []        &&
         |   i.feeAssetId            == unit
         | [
         |   BooleanEntry("check", check),
         |   BinaryEntry("transactionId", i.transactionId)
         | ]
       """.stripMargin
    )

  private val functionCallExpr =
    TestCompiler(V6).compileFreeCall(""" [BooleanEntry("check", true)] """.stripMargin)

  private def scenario(external: Boolean = false, functionCall: Boolean = false) =
    for {
      invoker  <- accountGen
      address2 <- accountGen
      fee      <- ciFee()
      gTx1    = GenesisTransaction.create(address2.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2    = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      call    = Some(if (functionCall) functionCallExpr else expr(invoker, fee))
      address = (if (external) address2 else invoker).toAddress
      invoke  = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, address, call, Nil, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2), invoke)

  property("global variable fields of free call") {
    val (genesisTxs, invoke) = scenario().sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.accountData(invoke.senderAddress, "check").get shouldBe BooleanDataEntry("check", true)
      d.blockchain.accountData(invoke.senderAddress, "transactionId").get shouldBe BinaryDataEntry("transactionId", invoke.txId)
    }
  }

  property("activation") {
    val (genesisTxs, invoke) = scenario().sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs: _*)
      (the[RuntimeException] thrownBy d.appendBlock(invoke)).getMessage should include("Free call is not activated yet")
    }
  }

  property("free call on external account") {
    val (genesisTxs, invoke) = scenario(external = true).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      (the[RuntimeException] thrownBy d.appendBlock(invoke)).getMessage should include("Free call could be performed only on the invoker account")
    }
  }

  property("free call using function call expression") {
    val (genesisTxs, invoke) = scenario(functionCall = true).sample.get
    invoke.exprOpt.get shouldBe a[FUNCTION_CALL]
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.accountData(invoke.senderAddress, "check").get shouldBe BooleanDataEntry("check", true)
    }
  }
}
