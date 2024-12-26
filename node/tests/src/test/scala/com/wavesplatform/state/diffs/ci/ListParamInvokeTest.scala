package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers, TxVersion}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import org.scalatest.Inside

class ListParamInvokeTest extends PropSpec with WithState with Inside {
  property("unactivated") {
    // precompiled to avoid compilation error
    val v3DApp =
      DApp(
        DAppMeta(),
        List(),
        List(
          CallableFunction(
            CallableAnnotation("i"),
            FUNC(
              "f",
              List("args"),
              FUNCTION_CALL(
                Native(FunctionIds.CREATE_LIST),
                List(
                  FUNCTION_CALL(
                    User("DataEntry"),
                    List(CONST_STRING("entry1").explicitGet(), FUNCTION_CALL(Native(FunctionIds.GET_LIST), List(REF("args"), CONST_LONG(0))))
                  ),
                  FUNCTION_CALL(
                    Native(FunctionIds.CREATE_LIST),
                    List(
                      FUNCTION_CALL(
                        User("DataEntry"),
                        List(CONST_STRING("entry2").explicitGet(), FUNCTION_CALL(Native(FunctionIds.GET_LIST), List(REF("args"), CONST_LONG(1))))
                      ),
                      REF(GlobalValNames.Nil)
                    )
                  )
                )
              )
            )
          )
        ),
        None
      )

    val (genesis, setScript, invoke, _) = paymentPreconditions(ContractScript(V3, v3DApp).explicitGet())
    assertDiffEi(Seq(TestBlock.create(genesis :+ setScript)), TestBlock.create(Seq(invoke)), features(withV4 = false)) {
      _ should produce("All arguments of InvokeScript must be one of the types: Int, ByteVector, Boolean, String")
    }
  }

  property("pass list args") {
    val (genesis, setScript, invoke, dAppAddress) = paymentPreconditions(dApp(V4))
    assertDiffAndState(Seq(TestBlock.create(genesis :+ setScript)), TestBlock.create(Seq(invoke)), features(withV4 = true)) { case (_, blockchain) =>
      blockchain.accountData(dAppAddress, "entry1").get.value shouldBe "value1"
      blockchain.accountData(dAppAddress, "entry2").get.value shouldBe "value2"
    }
  }

  private def paymentPreconditions(dApp: Script): (Seq[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, Address) = {
    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val genesis = Seq(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress)
    )
    val setScript = TxHelpers.setScript(master, dApp)

    val invoke = TxHelpers.invoke(
      dApp = master.toAddress,
      func = Some("f"),
      args = Seq(ARR(IndexedSeq(CONST_STRING("value1").explicitGet(), CONST_STRING("value2").explicitGet()), false).explicitGet()),
      invoker = invoker,
      fee = TxHelpers.ciFee(1),
      version = TxVersion.V1
    )
    (genesis, setScript, invoke, master.toAddress)
  }

  private def dApp(version: StdLibVersion): Script = {
    val result =
      if (version == V3)
        """
          |WriteSet([
          |  DataEntry("entry1", args[0]),
          |  DataEntry("entry2", args[1])
          |])
        """.stripMargin
      else
        """
          |[
          |  StringEntry("entry1", args[0]),
          |  StringEntry("entry2", args[1])
          |]
       """.stripMargin

    val script =
      s"""
         | {-# STDLIB_VERSION ${version.id} #-}
         | {-# CONTENT_TYPE   DAPP          #-}
         | {-# SCRIPT_TYPE    ACCOUNT       #-}
         |
         | @Callable(i)
         | func f(args: List[String]) = $result
         |
       """.stripMargin

    TestCompiler(version).compileContract(script)
  }

  private def features(withV4: Boolean) = {
    val v4ForkO = if (withV4) Seq(BlockchainFeatures.BlockV5) else Seq()
    val parameters = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps
    ) ++ v4ForkO
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = parameters.map(_.id -> 0).toMap)
  }
}
