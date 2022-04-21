package com.wavesplatform.state.diffs.ci
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, FUNC, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.FunctionIds.CREATE_LIST
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.{invoke, secondAddress, secondSigner, setScript}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.SetScriptTransaction

class ScriptTransferTest extends PropSpec with WithDomain {
  import DomainPresets._

  property("wrong address bytes length") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() =
          |   [
          |     ScriptTransfer(Address(base58'3P2pTpQhGbZrJXATKr75A1uZjeTrb4PHMY'), 1, unit)
          |   ]
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlockE(invoke()) should produce("Wrong addressBytes length: expected: 26, actual: 25")
    }
  }

  property("bad address checksum") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() =
          |   [
          |     ScriptTransfer(Address(base58'3N2pTpQhGbZrJXATKr75A1uZjeTrb4PHMYx'), 1, unit)
          |   ]
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlockE(invoke()) should produce("Bad address checksum")
    }
  }

  property("illegal alias") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() =
          |   [
          |     ScriptTransfer(Alias("世界冬🤦"), 1, unit)
          |   ]
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertFailed(
        invoke(),
        "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"
      )
    }
  }

  property("amount field absence") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dAppResult = FUNCTION_CALL(
        Native(CREATE_LIST),
        List(
          FUNCTION_CALL(
            User("ScriptTransfer"),
            List(FUNCTION_CALL(User("Alias"), List(CONST_STRING("alias").explicitGet())), REF("unit"))
          ),
          REF("nil")
        )
      )
      val dApp = ContractScriptImpl(V5, DApp(DAppMeta(), Nil, List(CallableFunction(CallableAnnotation("i"), FUNC("default", Nil, dAppResult))), None))
      val invokeTx = invoke()
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlockE(invokeTx)
      d.liquidDiff.errorMessage(invokeTx.id()).get.text should include("key not found: asset")
    }
  }

  property("assets from payment are available in transfer") {
    val paymentAmount  = 100
    val transferAmount = 99
    val setScriptFee   = FeeConstants(SetScriptTransaction.typeId) * FeeUnit
    withDomain(RideV5, Seq(AddrWithBalance(secondAddress, setScriptFee))) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() =
           |   [
           |     ScriptTransfer(i.caller, $transferAmount, unit)
           |   ]
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertSucceed(invoke(payments = Seq(Payment(paymentAmount, Waves))))
      d.blockchain.balance(secondAddress) shouldBe paymentAmount - transferAmount
    }
  }
}
