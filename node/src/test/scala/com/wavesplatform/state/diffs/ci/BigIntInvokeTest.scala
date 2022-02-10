package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.FunctionIds.TO_BIGINT
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{Assertion, EitherValues, Inside}

class BigIntInvokeTest extends PropSpec with Inside with WithState with DBCacheSettings with WithDomain with EitherValues {

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

    def assert(action: EXPR => FUNCTION_CALL, message: String): Assertion = {
      val dAppAcc = TxHelpers.signer(0)
      val invoker = TxHelpers.signer(1)
      val setScript = Seq(
        TxHelpers.setScript(dAppAcc, dApp(action))
      )
      val invoke = TxHelpers.invoke(dAppAcc.toAddress, func = None, invoker = invoker)

      withDomain(domainSettingsWithFS(fsWithV5), AddrWithBalance.enoughBalances(dAppAcc, invoker)) { d =>
        d.appendBlock(setScript: _*)
        d.appendBlockE(invoke) should produce(message)
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

    val dAppAcc1 = TxHelpers.signer(0)
    val dAppAcc2 = TxHelpers.signer(1)

    val preparingTxs = Seq(
      TxHelpers.genesis(dAppAcc1.toAddress),
      TxHelpers.genesis(dAppAcc2.toAddress),
      TxHelpers.setScript(dAppAcc1, dApp1(dAppAcc2.toAddress)),
      TxHelpers.setScript(dAppAcc2, dApp2)
    )
    val invoke = TxHelpers.invoke(dAppAcc1.toAddress, func = Some("default"), invoker = dAppAcc1)

    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      fsWithV5
    ) {
      case (diff, _) =>
        diff.errorMessage(invoke.id()) shouldBe None
        diff.scriptsRun shouldBe 2
        diff.accountData.head._2.data("key").value shouldBe 1
    }
  }
}
