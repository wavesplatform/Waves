package com.wavesplatform.state.diffs.freecall

import com.wavesplatform.{TestTime, TransactionGen}
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
import com.wavesplatform.transaction.smart.InvokeExpressionTransaction
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import org.scalatest.EitherValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.wavesplatform.{TestTime, TransactionGen}

class InvokeExpressionTest extends PropSpec with ScalaCheckPropertyChecks with TransactionGen with WithDomain with EitherValues {

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

  private val scenario =
    for {
      invoker  <- accountGen
      address2 <- accountGen
      fee      <- ciFee()
      gTx1   = GenesisTransaction.create(address2.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2   = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      call   = expr(invoker, fee)
      invoke = InvokeExpressionTransaction.selfSigned(TxVersion.V1, invoker, call, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2), invoke)

  property("global variable fields of invoke expression") {
    val (genesisTxs, invoke) = scenario.sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.accountData(invoke.senderAddress, "check").get shouldBe BooleanDataEntry("check", true)
      d.blockchain.accountData(invoke.senderAddress, "transactionId").get shouldBe BinaryDataEntry("transactionId", invoke.txId)
    }
  }

  property("activation") {
    val (genesisTxs, invoke) = scenario.sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs: _*)
      (the[RuntimeException] thrownBy d.appendBlock(invoke)).getMessage should include("Ride V6 feature has not been activated yet")
    }
  }
}
