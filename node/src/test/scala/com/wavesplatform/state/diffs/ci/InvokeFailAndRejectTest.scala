package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

class InvokeFailAndRejectTest extends PropSpec with WithDomain {
  import DomainPresets._

  private val assetFailScript = TestCompiler(V5).compileExpression(
    s"""
       | strict c = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
       | if (true) then throw() else true
     """.stripMargin
  )

  property("invoke fails by payment script") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val i     = issue(script = Some(assetFailScript))
      val asset = IssuedAsset(i.id())
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = []
        """.stripMargin
      )
      val invokeTx = invoke(secondAddress, payments = Seq(Payment(1, asset)))
      d.appendBlock(i)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(invokeTx)
      d.liquidDiff.errorMessage(invokeTx.id()).get.text should include(s"Transaction is not allowed by script of the asset $asset")
    }
  }

  property("invoke fails by ScriptTransfer script") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val i     = issue(script = Some(assetFailScript))
      val asset = IssuedAsset(i.id())
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   ScriptTransfer(i.caller, 1, base58'$asset')
           | ]
         """.stripMargin
      )
      val invokeTx = invoke(secondAddress)
      d.appendBlock(i)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(invokeTx)
      d.liquidDiff.errorMessage(invokeTx.id()).get.text should include(s"Transaction is not allowed by script of the asset $asset")
    }
  }
}
