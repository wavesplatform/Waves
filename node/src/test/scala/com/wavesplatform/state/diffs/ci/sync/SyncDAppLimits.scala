package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5, V6}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.SetScriptTransaction
import org.scalatest.{EitherValues, OptionValues}

import scala.annotation.tailrec

class SyncDAppLimits extends PropSpec with WithDomain with OptionValues with EitherValues {
  private val alice     = TxHelpers.signer(1) // signer(0) forges blocks and this affects the balance
  private val aliceAddr = alice.toAddress

  complexityLimitTests("NODE-521 A limit of complexity is 52000 if the first script is V6", V6, 52000)
  complexityLimitTests("NODE-527 A limit of complexity is 26000 if the first script is V5", V5, 26000)
  private def complexityLimitTests(title: String, v: StdLibVersion, complexityLimit: Int): Unit = {
    val mkSetScriptTx: SetScriptTransaction = TxHelpers.setScript(
      alice,
      TestCompiler(v).compileContract(
        // One call of foo has 2000 complexity
        s""" @Callable(inv)
           | func foo(n: Int) = {
           |   let complexInt1 = ${mkIntExprWithComplexity(2000 - 82 - 2)} # 1916
           |   # 82 = 1 for "n > 1", 81 for branches
           |   let complexInt2 = if (n > 1) then {
           |     # 81 = 2 for valueOrElse, 75 for invoke, 1 for Address, 1 for "n - 1", 1 for list, 1 for as
           |     valueOrElse(invoke(Address(base58'$aliceAddr'), "foo", [n - 1], []).as[Int], 0)
           |   } else {
           |     ${mkIntExprWithComplexity(81)} # 81
           |   }
           |   # 2 = 1 for tuple, 1 for "+"
           |   ([], complexInt1 + complexInt2)
           | }
           | """.stripMargin
      ),
      fee = 1.waves
    )

    property(s"$title - positive") {
      withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(alice)) { d =>
        d.appendBlock(mkSetScriptTx)

        val calls    = complexityLimit / 2000
        val invokeTx = TxHelpers.invoke(aliceAddr, Some("foo"), Seq(CONST_LONG(calls)), invoker = alice)

        d.appendBlock(invokeTx)
        val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
        invokeTxMeta.spentComplexity shouldBe complexityLimit

        invokeTxMeta.status == Status.Succeeded shouldBe true
      }
    }

    property(s"$title - negative") {
      withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(alice)) { d =>
        d.appendBlock(mkSetScriptTx)

        val calls    = complexityLimit / 2000 + 1
        val invokeTx = TxHelpers.invoke(aliceAddr, Some("foo"), Seq(CONST_LONG(calls)), invoker = alice)

        val diff              = d.createDiffE(invokeTx).value
        val (_, scriptResult) = diff.scriptResults.headOption.value
        scriptResult.error.value.text should include(s"Invoke complexity limit = $complexityLimit is exceeded")

        d.appendBlock(invokeTx)
        val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
        invokeTxMeta.spentComplexity shouldBe complexityLimit
        invokeTxMeta.status == Status.Succeeded shouldBe false
      }
    }
  }

  property("NODE-726 A limit of inner invokes is 100 in RideV6") {
    withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(alice)) { d =>
      d.appendBlock(
        TxHelpers.setScript(
          alice,
          TestCompiler(V6).compileContract(
            s""" @Callable(inv)
               | func foo(n: Int) = {
               |   if (n > 0) then {
               |     strict res = invoke(Address(base58'$aliceAddr'), "foo", [n - 1], [])
               |     ([], 0)
               |   } else {
               |     ([], 1 + 1)
               |   }
               | }
               | """.stripMargin
          ),
          fee = 1.waves
        )
      )

      val invokeTx = TxHelpers.invoke(aliceAddr, Some("foo"), Seq(CONST_LONG(101)), invoker = alice)

      val diff              = d.createDiffE(invokeTx).value
      val (_, scriptResult) = diff.scriptResults.headOption.value
      scriptResult.error.value.text should include("DApp calls limit = 100 is exceeded")

      d.appendBlock(invokeTx)
      val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
      invokeTxMeta.status == Status.Succeeded shouldBe false
    }
  }

  property("NODE-763 A total number of inner invokes is unlimited in RideV6") {
    withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(alice)) { d =>
      d.appendBlock(
        TxHelpers.setScript(
          alice,
          TestCompiler(V6).compileContract(
            s""" @Callable(inv)
               | func foo() = {
               |   strict res1 = invoke(this, "bar", [99], [])
               |   strict res2 = invoke(this, "bar", [98], [])
               |   []
               | }
               |
               | @Callable(inv)
               | func bar(c: Int) = if (c == 0) then [] else {
               |  strict r  = this.invoke("bar", [c - 1], [])
               |  []
               | }
               | """.stripMargin
          ),
          fee = 1.waves
        )
      )

      val invokeTx = TxHelpers.invoke(aliceAddr, Some("foo"), invoker = alice)
      d.appendBlock(invokeTx)

      val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
      invokeTxMeta.status == Status.Succeeded shouldBe true
    }
  }

  private lazy val complexityExamples = List(
    // 1 for toInt, 200 for log, 65+65 for parseBigInt
    331 -> s"""toInt(log(parseBigIntValue("1625"), 2, parseBigIntValue("27"), 1, 2, HALFUP))""",
    100 -> s"""log(1625, 2, 27, 1, 2, HALFUP)""",
    12  -> s"""valueOrElse(getInteger("k"), 0)""" // 2 for valueOrElse, 10 for getInteger
  )

  private def mkIntExprWithComplexity(targetComplexity: Int): String = {
    @tailrec
    def loop(restComplexity: Int, restComplexityExamples: List[(Int, String)], acc: String): String =
      if (restComplexity == 0) acc
      else
        restComplexityExamples match {
          case Nil => loop(0, Nil, acc + " + 1" * restComplexity)
          case (exprComplexity, expr) :: restXs =>
            if (restComplexity >= exprComplexity) {
              val uncompletedExprComplexity = exprComplexity + 1 // 1 for "+"
              val times                     = restComplexity / uncompletedExprComplexity
              loop(restComplexity % uncompletedExprComplexity, restXs, acc + s" + $expr" * times)
            } else loop(restComplexity, restXs, acc)
        }

    loop(targetComplexity, complexityExamples, "1")
  }
}
