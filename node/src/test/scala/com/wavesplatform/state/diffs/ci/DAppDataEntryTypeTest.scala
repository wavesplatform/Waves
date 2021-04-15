package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, CONST_STRING, FUNC, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DAppDataEntryTypeTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with Inside
    with WithState
    with DBCacheSettings
    with MockFactory
    with WithDomain
    with EitherValues {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

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

  private def paymentPreconditions(constructor: String): Gen[(List[Transaction], InvokeScriptTransaction)] =
    for {
      dAppAcc <- accountGen
      invoker <- accountGen
      fee     <- ciFee()
    } yield {
      for {
        genesis  <- GenesisTransaction.create(dAppAcc.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, dAppAcc, Some(dApp(constructor)), fee, ts)
        invoke   <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, dAppAcc.toAddress, None, Nil, fee, Waves, ts)
      } yield (List(genesis, genesis2, setDApp), invoke)
    }.explicitGet()

  private def assert(constructor: String) = {
    val (preparingTxs, invoke) = paymentPreconditions(constructor).sample.get
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
