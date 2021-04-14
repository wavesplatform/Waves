package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.FunctionIds.TO_BIGINT
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

class BigIntRestrictionTest
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
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockReward.id      -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> 0
    ),
    estimatorPreCheckHeight = Int.MaxValue
  )

  private val bigIntValue = 12345

  private def dApp(action: EXPR => FUNCTION_CALL): Script = {
    ContractScriptImpl(
      V5,
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
                List(action(FUNCTION_CALL(Native(TO_BIGINT), List(CONST_LONG(bigIntValue)))), REF("nil"))
              )
            )
          )
        ),
        None
      )
    )
  }

  private def paymentPreconditions(action: EXPR => FUNCTION_CALL): Gen[(List[Transaction], InvokeScriptTransaction)] =
    for {
      dAppAcc <- accountGen
      invoker <- accountGen
      fee     <- ciFee()
    } yield {
      for {
        genesis  <- GenesisTransaction.create(dAppAcc.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, dAppAcc, Some(dApp(action)), fee, ts)
        invoke   <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, dAppAcc.toAddress, None, Nil, fee, Waves, ts)
      } yield (List(genesis, genesis2, setDApp), invoke)
    }.explicitGet()

  private def assert(action: EXPR => FUNCTION_CALL, message: String) = {
    val (preparingTxs, invoke) = paymentPreconditions(action).sample.get
    withDomain(domainSettingsWithFS(fsWithV5)) { d =>
      d.appendBlock(preparingTxs: _*)
      (the[RuntimeException] thrownBy d.appendBlock(invoke)).getMessage should include(message)
    }
  }

  private def dataEntry(`type`: String): EXPR => FUNCTION_CALL = { expr =>
    FUNCTION_CALL(
      FunctionHeader.User(`type`),
      List(CONST_STRING("key").explicitGet(), expr)
    )
  }

  private val lease: EXPR => FUNCTION_CALL = { expr =>
    FUNCTION_CALL(
      FunctionHeader.User("Lease"),
      List(FUNCTION_CALL(FunctionHeader.User("Address"), List(CONST_BYTESTR(ByteStr.empty).explicitGet())), expr)
    )
  }

  private val transfer: EXPR => FUNCTION_CALL = { expr =>
    FUNCTION_CALL(
      FunctionHeader.User("ScriptTransfer"),
      List(FUNCTION_CALL(FunctionHeader.User("Address"), List(CONST_BYTESTR(ByteStr.empty).explicitGet())), expr, REF("unit"))
    )
  }

  private val reissue: EXPR => FUNCTION_CALL = { expr =>
    FUNCTION_CALL(
      FunctionHeader.User("Reissue"),
      List(CONST_BYTESTR(ByteStr.empty).explicitGet(), expr, CONST_BOOLEAN(true))
    )
  }

  private val burn: EXPR => FUNCTION_CALL = { expr =>
    FUNCTION_CALL(
      FunctionHeader.User("Burn"),
      List(CONST_BYTESTR(ByteStr.empty).explicitGet(), expr)
    )
  }

  property(s"BigInt is forbidden for DApp actions") {
    List("IntegerEntry", "StringEntry", "BinaryEntry", "BooleanEntry")
      .foreach(a => assert(dataEntry(a), s"can't reconstruct $a from Map(key -> key, value -> $bigIntValue)"))

    assert(lease, "12345),None)' instead of Lease")
    assert(transfer, "12345,Unit)' instead of ScriptTransfer")
    assert(reissue, "Some(12345),Some(true))' instead of Reissue")
    assert(burn, "Some(12345))' instead of Burn")
  }
}
