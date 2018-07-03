package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState}
import com.wavesplatform.utils.dummyCompilerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.{GenesisTransaction}

class ContextFunctionsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val script: String =
    s"""
       | let mulLong = 1000 * 2 == 2000
       | let divLong = 1000 / 2 == 500
       | let modLong = 1000 % 2 == 0
       | let sumLong = 1000 + 2 == 1002
       | let subLong = 1000 - 2 == 998
       | let sumString = "ha" + "-" +"ha" == "ha-ha"
       | let uMinus = -1 == -1
       | let uNot = -1 != 0
       |
       | mulLong && divLong && modLong && sumLong && subLong && sumString && uMinus && uNot
       |
    """.stripMargin

  val preconditionsAndPayments = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis1 = GenesisTransaction.create(master, ENOUGH_AMT * 3, ts).explicitGet()
    genesis2 = GenesisTransaction.create(recipient, ENOUGH_AMT * 3, ts).explicitGet()

    untypedScript = {
      val r = Parser(script).get.value
      assert(r.size == 1)
      r.head
    }

    typedScript = ScriptV1(CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1).explicitGet()

    setScriptTransaction: SetScriptTransaction = SetScriptTransaction
      .selfSigned(1, recipient, Some(typedScript), 100000000L, ts)
      .explicitGet()
    nextTransfer <- transferGeneratorP(ts, master, recipient.toAddress, 100000000L)
  } yield (Seq(genesis1, genesis2), setScriptTransaction, nextTransfer)

  property("Functions from context validation") {
    forAll(preconditionsAndPayments) {
      case ((genesis, setScriptTransaction, nextTransfer)) =>
        assertDiffAndState(smartEnabledFS) { append =>
          append(genesis).explicitGet()
          append(Seq(setScriptTransaction, nextTransfer)).explicitGet()
        }
    }
  }
}
