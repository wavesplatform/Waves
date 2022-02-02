package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, CONST_STRING, FUNC, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{EitherValues, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DAppDataEntryTypeTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Inside
    with WithState
    with DBCacheSettings
    with WithDomain
    with EitherValues {

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id   -> 0,
      BlockchainFeatures.SmartAssets.id     -> 0,
      BlockchainFeatures.Ride4DApps.id      -> 0,
      BlockchainFeatures.FeeSponsorship.id  -> 0,
      BlockchainFeatures.DataTransaction.id -> 0,
      BlockchainFeatures.BlockReward.id     -> 0,
      BlockchainFeatures.BlockV5.id         -> 0
    ),
    estimatorPreCheckHeight = Int.MaxValue
  )

  private def dApp(constructor: String): Script = {
    val value = if (constructor == "BooleanEntry") CONST_LONG(1) else CONST_BOOLEAN(true)
    ContractScriptImpl(
      V4,
      DApp(
        DAppMeta(),
        Nil,
        List(
          CallableFunction(
            CallableAnnotation("i"),
            FUNC(
              "default",
              Nil,
              FUNCTION_CALL(
                FunctionHeader.Native(FunctionIds.CREATE_LIST),
                List(
                  FUNCTION_CALL(
                    FunctionHeader.User(constructor),
                    List(CONST_STRING("key").explicitGet(), value)
                  ),
                  REF("nil")
                )
              )
            )
          )
        ),
        None
      )
    )
  }

  private def assert(constructor: String) = {
    val dAppAcc = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val preparingTxs = Seq(
      TxHelpers.genesis(dAppAcc.toAddress),
      TxHelpers.genesis(invoker.toAddress),
      TxHelpers.setScript(dAppAcc, dApp(constructor))
    )

    val invoke = TxHelpers.invoke(dAppAcc.toAddress, func = None, invoker = invoker)

    withDomain(domainSettingsWithFS(fsWithV5)) { d =>
      d.appendBlock(preparingTxs: _*)
      val value = if (constructor == "BooleanEntry") "1" else "true"
      (the[RuntimeException] thrownBy d.appendBlock(invoke)).getMessage should include(
        s"can't reconstruct $constructor from Map(key -> key, value -> $value)"
      )
    }
  }

  property(s"DataEntry value type is checked") {
    List("IntegerEntry", "StringEntry", "BinaryEntry", "BooleanEntry").foreach(assert)
  }
}
