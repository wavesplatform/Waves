package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produce}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ListParamInvokeTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithState with Inside {
  property("unactivated") {
    // precompiled to avoid compilation error
    val v3DApp =
      DApp(
        DAppMeta(),
        List(),
        List(
          CallableFunction(
            CallableAnnotation("i"),
            FUNC("f", List("args"),
              FUNCTION_CALL(
                Native(FunctionIds.CREATE_LIST),
                List(
                  FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("entry1").explicitGet(), FUNCTION_CALL(Native(FunctionIds.GET_LIST), List(REF("args"), CONST_LONG(0))))),
                  FUNCTION_CALL(
                    Native(FunctionIds.CREATE_LIST),
                    List(
                      FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("entry2").explicitGet(), FUNCTION_CALL(Native(FunctionIds.GET_LIST), List(REF("args"), CONST_LONG(1))))),
                      REF("nil")))
                )
              )
            )
          )
        ),
        None
      )
    forAll(paymentPreconditions(ContractScript(V3, v3DApp).explicitGet())) {
      case (genesis, setScript, invoke, _) =>
        assertDiffEi(Seq(TestBlock.create(genesis :+ setScript)), TestBlock.create(Seq(invoke)), features(withV4 = false)) {
          _ should produce("All arguments of InvokeScript must be one of the types: Int, ByteVector, Boolean, String")
        }
    }
  }

  property("pass list args") {
    forAll(paymentPreconditions(dApp(V4))) {
      case (genesis, setScript, invoke, dAppAddress) =>
        assertDiffAndState(Seq(TestBlock.create(genesis :+ setScript)), TestBlock.create(Seq(invoke)), features(withV4 = true)) {
          case (_, blockchain) =>
            blockchain.accountData(dAppAddress, "entry1").get.value shouldBe "value1"
            blockchain.accountData(dAppAddress, "entry2").get.value shouldBe "value2"
        }
    }
  }

  private def paymentPreconditions(dApp: Script): Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, Address)] =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      fee     <- ciFee(1)
    } yield {
      val functionCall =
        Some(
          FUNCTION_CALL(
            User("f"),
            List(ARR(IndexedSeq(
              CONST_STRING("value1").explicitGet(),
              CONST_STRING("value2").explicitGet()
            ), false).explicitGet())
          )
        )
      for {
        genesis  <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, Some(dApp), fee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, functionCall, Nil, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, master.toAddress)
    }.explicitGet()

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
      BlockchainFeatures.Ride4DApps,
    ) ++ v4ForkO
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = parameters.map(_.id -> 0).toMap)
  }
}
