package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers, TxVersion}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed

class IllegalAddressChainIdTest extends PropSpec with WithDomain {
  import DomainPresets._

  private[this] def sigVerify(c: Boolean): String =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private[this] def contract(bigComplexity: Boolean) = TestCompiler(V5).compileContract(
    s"""
       |  @Callable(i)
       |  func default() = {
       |    ${sigVerify(bigComplexity)}
       |    let address = Address(base58'3PMj3yGPBEa1Sx9X4TSBFeJCMMaE3wvKR4N')
       |    [ ScriptTransfer(address, 1, unit) ]
       |  }
     """.stripMargin
  )

  private[this] def scenario(fail: Boolean, bigComplexity: Boolean = false) =
    for {
      master  <- accountGen
      invoker <- accountGen
      fee     <- ciFee()
      gTx1     = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, TxHelpers.timestamp).explicitGet()
      gTx2     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, TxHelpers.timestamp).explicitGet()
      ssTx     = SetScriptTransaction.selfSigned(1.toByte, master, Some(contract(bigComplexity)), fee, TxHelpers.timestamp).explicitGet()
      invokeTx = Signed.invokeScript(TxVersion.V3, invoker, master.toAddress, None, Nil, fee, Waves, TxHelpers.timestamp)
    } yield (Seq(gTx1, gTx2, ssTx), invokeTx)

  private val error = "Address belongs to another network: expected: 84(T), actual: 87(W)"

  property("no fail before fix") {
    withDomain(RideV5) { d =>
      val (genesisTxs, invokeTx) = scenario(fail = true).sample.get
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invokeTx)).getMessage should include(error)
    }
  }

  property("reject after fix") {
    withDomain(RideV6) { d =>
      val (genesisTxs, invokeTx) = scenario(fail = true).sample.get
      d.appendBlock(genesisTxs: _*)
      d.appendAndCatchError(invokeTx).toString should include(error)
    }
  }

  property("fail after fix and big complexity") {
    withDomain(RideV6) { d =>
      val (genesisTxs, invokeTx) = scenario(fail = true, bigComplexity = true).sample.get
      d.appendBlock(genesisTxs: _*)
      d.appendAndAssertFailed(invokeTx)
    }
  }
}
