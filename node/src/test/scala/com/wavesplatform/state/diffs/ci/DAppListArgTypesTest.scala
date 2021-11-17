package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, Ride4DApps, SynchronousCalls, RideV6 => RideV6F}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.{V3, V4}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, _}
import com.wavesplatform.lang.v1.compiler.{TestCompiler, Types}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed
import org.scalatest.Inside

class DAppListArgTypesTest extends PropSpec with WithDomain with Inside {
  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def preconditions(dAppScript: Script, args: List[EVALUATED]) = {
    val invoker = accountGen.sample.get
    val dApp    = accountGen.sample.get
    val fee     = ciFee().sample.get
    val call    = Some(FUNCTION_CALL(User("f"), args))
    val genesis = Seq(invoker, dApp).map(acc => GenesisTransaction.create(acc.toAddress, ENOUGH_AMT, ts).explicitGet())
    val setDApp = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(dAppScript), fee, ts).explicitGet()
    val ci      = () => Signed.invokeScript(1.toByte, invoker, dApp.toAddress, call, Nil, fee, Waves, ts)
    (genesis :+ setDApp, ci, dApp.toAddress)
  }

  private def rideList(args: EVALUATED*) =
    List(ARR(Vector(args: _*), false).explicitGet())

  property("can't pass list as callable argument before V4 activation") {
    // precompiled to avoid compilation error
    val callable = CallableFunction(CallableAnnotation("i"), FUNC("f", List("args"), REF("nil")))
    val v3DApp   = DApp(DAppMeta(), Nil, List(callable), None)

    val (preparingTxs, invoke, _) = preconditions(ContractScript(V3, v3DApp).explicitGet(), rideList())
    withDomain(RideV3) { d =>
      d.appendBlock(preparingTxs: _*)
      (the[Exception] thrownBy d.appendBlock(invoke())).getMessage should include(
        "All arguments of InvokeScript must be one of the types: Int, ByteVector, Boolean, String")
    }
  }

  property("list as callable argument is allowed after V4 activation") {
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
      d.appendBlock(preparingTxs: _*)
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

  property("list as callable argument is checked for primitives after V6 activation") {
    val settings =
      TestFunctionalitySettings.Enabled
        .copy(preActivatedFeatures = Map(Ride4DApps.id -> 0, BlockV5.id -> 0, SynchronousCalls.id -> 0, RideV6F.id -> 3))

    def assert(forbidAfterActivation: Boolean, args: List[EVALUATED]) = {
      withDomain(domainSettingsWithFS(settings)) { d =>
        val (preparingTxs, invoke, _) = preconditions(dApp, args)
        d.appendBlock(preparingTxs: _*)

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
    assert(forbidAfterActivation = false, rideList(primitives: _*))
    assert(forbidAfterActivation = false, rideList())
    assert(forbidAfterActivation = true, rideList(rideList().head))
  }

  property("list of object as callable argument is forbidden by serialization") {
    val (_, invoke, _) = preconditions(dApp, rideList(CaseObj(Types.UNIT, Map())))
    (the[Throwable] thrownBy invoke()).getMessage should include("Serialization of value Unit is unsupported")
  }
}
