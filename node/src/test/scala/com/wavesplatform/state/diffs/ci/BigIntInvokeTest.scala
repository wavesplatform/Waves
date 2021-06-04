package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.TestCompiler
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

class BigIntInvokeTest
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

  property("BigInt is forbidden for DApp actions") {
    def dApp(action: EXPR => FUNCTION_CALL): Script = {
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

    def paymentPreconditions(action: EXPR => FUNCTION_CALL): Gen[(List[Transaction], InvokeScriptTransaction)] =
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

    def assert(action: EXPR => FUNCTION_CALL, message: String) = {
      val (preparingTxs, invoke) = paymentPreconditions(action).sample.get
      withDomain(domainSettingsWithFS(fsWithV5)) { d =>
        d.appendBlock(preparingTxs: _*)
        (the[RuntimeException] thrownBy d.appendBlock(invoke)).getMessage should include(message)
      }
    }

    def dataEntry(`type`: String): EXPR => FUNCTION_CALL = { expr =>
      FUNCTION_CALL(
        FunctionHeader.User(`type`),
        List(CONST_STRING("key").explicitGet(), expr)
      )
    }

    val lease: EXPR => FUNCTION_CALL = { expr =>
      FUNCTION_CALL(
        FunctionHeader.User("Lease"),
        List(FUNCTION_CALL(FunctionHeader.User("Address"), List(CONST_BYTESTR(ByteStr.empty).explicitGet())), expr)
      )
    }

    val transfer: EXPR => FUNCTION_CALL = { expr =>
      FUNCTION_CALL(
        FunctionHeader.User("ScriptTransfer"),
        List(FUNCTION_CALL(FunctionHeader.User("Address"), List(CONST_BYTESTR(ByteStr.empty).explicitGet())), expr, REF("unit"))
      )
    }

    val reissue: EXPR => FUNCTION_CALL = { expr =>
      FUNCTION_CALL(
        FunctionHeader.User("Reissue"),
        List(CONST_BYTESTR(ByteStr.empty).explicitGet(), expr, CONST_BOOLEAN(true))
      )
    }

    val burn: EXPR => FUNCTION_CALL = { expr =>
      FUNCTION_CALL(
        FunctionHeader.User("Burn"),
        List(CONST_BYTESTR(ByteStr.empty).explicitGet(), expr)
      )
    }

    List("IntegerEntry", "StringEntry", "BinaryEntry", "BooleanEntry")
      .foreach(a => assert(dataEntry(a), s"can't reconstruct $a from Map(key -> key, value -> $bigIntValue)"))

    assert(lease, s"$bigIntValue),None)' instead of Lease")
    assert(transfer, s"$bigIntValue,Unit)' instead of ScriptTransfer")
    assert(reissue, s"Some($bigIntValue),Some(true))' instead of Reissue")
    assert(burn, s"Some($bigIntValue))' instead of Burn")
  }

  property("BigInt as Invoke return value") {
    def dApp1(nextDApp: Address): Script = TestCompiler(V5).compileContract(
      s"""
         | {-# STDLIB_VERSION 5       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | @Callable(i)
         | func default() = {
         |   let address = Address(base58'$nextDApp')
         |   strict r = invoke(address, "default", [], [])
         |   if (r == toBigInt($bigIntValue))
         |     then []
         |     else throw("")
         | }
     """.stripMargin
    )

    val dApp2: Script = TestCompiler(V5).compileContract(
      s"""
         | {-# STDLIB_VERSION 5       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | @Callable(i)
         | func default() = {
         |   ([IntegerEntry("key", 1)], toBigInt($bigIntValue))
         | }
       """.stripMargin
    )

    val preconditions =
      for {
        dApp1Acc <- accountGen
        dApp2Acc <- accountGen
        fee      <- ciFee()
        genesis1 = GenesisTransaction.create(dApp1Acc.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2Acc.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1Acc, Some(dApp1(dApp2Acc.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2Acc, Some(dApp2), fee, ts).explicitGet()
        invoke = InvokeScriptTransaction
          .selfSigned(1.toByte, dApp1Acc, dApp1Acc.toAddress, Some(FUNCTION_CALL(User("default"), Nil)), Nil, fee, Waves, ts)
          .explicitGet()
      } yield (List(genesis1, genesis2, setDApp1, setDApp2), invoke)

    val (preparingTxs, invoke) = preconditions.sample.get
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      fsWithV5
    ) {
      case (diff, _) =>
        diff.errorMessage(invoke.id.value()) shouldBe None
        diff.scriptsRun shouldBe 2
        diff.accountData.head._2.data("key").value shouldBe 1
    }
  }
}
