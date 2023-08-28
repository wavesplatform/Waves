package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.contract
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.FunctionIds.TO_BIGINT
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{EitherValues, Inside}

class BigIntInvokeTest extends PropSpec with Inside with WithState with DBCacheSettings with WithDomain with EitherValues {

  private val bigIntValue = 12345

  property("BigInt is forbidden for DApp actions") {
    def dApp(action: EXPR => FUNCTION_CALL, version: StdLibVersion): Script = {
      ContractScriptImpl(
        version,
        contract.DApp(
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
                  List(action(FUNCTION_CALL(Native(TO_BIGINT), List(CONST_LONG(bigIntValue)))), REF(GlobalValNames.Nil))
                )
              )
            )
          ),
          None
        )
      )
    }

    def assert(action: EXPR => FUNCTION_CALL, message: String): Unit = {
      val dAppAcc = TxHelpers.signer(0)
      val invoker = TxHelpers.signer(1)

      testDomain(AddrWithBalance.enoughBalances(dAppAcc, invoker), from = V5) { case (version, d) =>
        val setScript = TxHelpers.setScript(dAppAcc, dApp(action, version))
        val invoke    = TxHelpers.invoke(dAppAcc.toAddress, func = None, invoker = invoker)

        d.appendBlock(setScript)
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
        List(FUNCTION_CALL(FunctionHeader.User("Address"), List(CONST_BYTESTR(ByteStr.empty).explicitGet())), expr, REF(GlobalValNames.Unit))
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
    def dApp1(nextDApp: Address, version: StdLibVersion): Script = TestCompiler(version).compileContract(
      s"""
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

    def dApp2(version: StdLibVersion): Script = TestCompiler(version).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ([IntegerEntry("key", 1)], toBigInt($bigIntValue))
         | }
       """.stripMargin
    )

    val dAppAcc1 = TxHelpers.signer(0)
    val dAppAcc2 = TxHelpers.signer(1)

    val invoke = TxHelpers.invoke(dAppAcc1.toAddress, func = Some("default"), invoker = dAppAcc1)

    testDomain(from = V5) { case (version, d) =>
      val preparingTxs = Seq(
        TxHelpers.genesis(dAppAcc1.toAddress),
        TxHelpers.genesis(dAppAcc2.toAddress),
        TxHelpers.setScript(dAppAcc1, dApp1(dAppAcc2.toAddress, version)),
        TxHelpers.setScript(dAppAcc2, dApp2(version))
      )

      d.appendBlock(preparingTxs*)
      d.appendBlock(invoke)

      d.liquidDiff.errorMessage(invoke.id()) shouldBe None
      inside(d.liquidDiff.scriptResults.toSeq) { case Seq((_, call1)) =>
        inside(call1.invokes) { case Seq(call2) =>
          call2.stateChanges.error shouldBe empty
          call2.stateChanges.invokes shouldBe empty
        }
      }
      d.liquidDiff.accountData.head._2("key").value shouldBe 1
    }
  }
}
