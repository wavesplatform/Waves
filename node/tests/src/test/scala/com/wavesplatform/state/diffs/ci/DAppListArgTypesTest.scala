package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, Ride4DApps, SynchronousCalls, RideV6 as RideV6F}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.{V3, V4}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, *}
import com.wavesplatform.lang.v1.compiler.{TestCompiler, Types}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed
import org.scalatest.Inside

class DAppListArgTypesTest extends PropSpec with WithDomain with Inside {
  import DomainPresets.*

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def preconditions(dAppScript: Script, args: List[EVALUATED]) = {
    val invoker = RandomKeyPair()
    val dApp    = RandomKeyPair()
    val fee     = ciFee().sample.get
    val call    = Some(FUNCTION_CALL(User("f"), args))
    val genesis = Seq(invoker, dApp).map(acc => TxHelpers.genesis(acc.toAddress, ENOUGH_AMT))
    val setDApp = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(dAppScript), 0.01.waves, ts).explicitGet()
    val ci      = () => Signed.invokeScript(1.toByte, invoker, dApp.toAddress, call, Nil, fee, Waves, ts)
    (genesis :+ setDApp, ci, dApp.toAddress)
  }

  private def rideList(args: EVALUATED*) =
    List(ARR(Vector(args*), false).explicitGet())

  property("NODE-801. can't pass list as callable argument before V4 activation") {
    // precompiled to avoid compilation error
    val callable = CallableFunction(CallableAnnotation("i"), FUNC("f", List("args"), REF(GlobalValNames.Nil)))
    val v3DApp   = DApp(DAppMeta(), Nil, List(callable), None)

    val (preparingTxs, invoke, _) = preconditions(ContractScript(V3, v3DApp).explicitGet(), rideList())
    withDomain(RideV3) { d =>
      d.appendBlock(preparingTxs*)
      (the[Exception] thrownBy d.appendBlock(invoke())).getMessage should include(
        "All arguments of InvokeScript must be one of the types: Int, ByteVector, Boolean, String"
      )
    }
  }

  property("NODE-800. list as callable argument is allowed after V4 activation") {
    val args = rideList(CONST_STRING("value1").explicitGet(), CONST_STRING("value2").explicitGet())
    val dApp =
      TestCompiler(V4).compileContract(
        s"""
           | @Callable(i)
           | func f(args: List[String]) =
           |   [
           |     StringEntry("entry1", args[0]),
           |     StringEntry("entry2", args[1])
           |   ]
       """.stripMargin
      )

    val (preparingTxs, invoke, dAppAddress) = preconditions(dApp, args)
    withDomain(RideV4) { d =>
      d.appendBlock(preparingTxs*)
      d.appendBlock(invoke())
      d.blockchain.accountData(dAppAddress, "entry1").get.value shouldBe "value1"
      d.blockchain.accountData(dAppAddress, "entry2").get.value shouldBe "value2"
    }
  }

  private val dApp =
    TestCompiler(V4).compileContract(
      s"""
         | @Callable(i)
         | func f(args: List[String]) = []
       """.stripMargin
    )

  property("NODE-241, NODE-243, NODE-244.list as callable argument is checked for primitives after V6 activation") {
    val settings =
      TestFunctionalitySettings.Enabled
        .copy(preActivatedFeatures = Map(Ride4DApps.id -> 0, BlockV5.id -> 0, SynchronousCalls.id -> 0, RideV6F.id -> 3))

    def assert(forbidAfterActivation: Boolean, args: List[EVALUATED]) = {
      withDomain(domainSettingsWithFS(settings)) { d =>
        val (preparingTxs, invoke, _) = preconditions(dApp, args)
        d.appendBlock(preparingTxs*)

        val invoke1 = invoke()
        d.appendBlock(invoke1)
        d.blockchain.transactionSucceeded(invoke1.id.value()) shouldBe true

        val invoke2 = invoke()
        if (forbidAfterActivation) {
          (the[Exception] thrownBy d.appendBlock(invoke2)).getMessage should include(
            s"All arguments of InvokeScript must be one of the types: List[], Boolean, Int, ByteVector, String"
          )
        } else {
          d.appendBlock(invoke2)
          d.blockchain.transactionSucceeded(invoke2.id.value()) shouldBe true
        }
      }
    }

    val primitives =
      Seq(
        CONST_STRING("s").explicitGet(),
        CONST_LONG(1),
        CONST_BOOLEAN(true),
        CONST_BYTESTR(ByteStr.empty).explicitGet()
      )

    primitives.foreach(v => assert(forbidAfterActivation = false, List(v)))
    assert(forbidAfterActivation = false, rideList(primitives*))
    assert(forbidAfterActivation = false, rideList())
    assert(forbidAfterActivation = true, rideList(rideList().head))
  }

  property("NODE-799. list of object as callable argument is forbidden by serialization") {
    val (_, invoke, _) = preconditions(dApp, rideList(CaseObj(Types.UNIT, Map())))
    (the[Throwable] thrownBy invoke()).getMessage should include("Serialization of value Unit is unsupported")
  }
}
