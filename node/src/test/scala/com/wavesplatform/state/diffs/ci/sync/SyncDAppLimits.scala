package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5, V6}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{EitherValues, OptionValues}

import scala.annotation.tailrec

class SyncDAppLimits extends PropSpec with WithDomain with OptionValues with EitherValues {
  private val alice     = TxHelpers.signer(1) // signer(0) forges blocks and this affects the balance
  private val aliceAddr = alice.toAddress

  complexityLimitTest("NODE-521 A limit of complexity is 52000 if the first script is V6", V6, 52000)
  complexityLimitTest("NODE-527 A limit of complexity is 26000 if the first script is V5", V5, 26000)
  private def complexityLimitTest(title: String, v: StdLibVersion, complexityLimit: Int): Unit = {
    property(title) {
      withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(alice)) { d =>
        d.appendBlock(
          TxHelpers.setScript(
            alice,
            TestCompiler(v).compileContract(
              // 81 = 1 for ">", 1+1 for strict, 75 for invoke, 1 for Address, 1 for "-", 1 for list
              // One call of foo(n > 0) has 2000 complexity
              s""" @Callable(inv)
                 | func foo(n: Int) = {
                 |   if (n > 0) then {
                 |     strict complexInt = ${mkIntExprWithComplexity(2000 - 81)}
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

        val calls    = complexityLimit / 2000 + 1
        val invokeTx = TxHelpers.invoke(aliceAddr, Some("foo"), Seq(CONST_LONG(calls)), invoker = alice)

        val diff              = d.createDiffE(invokeTx).value
        val (_, scriptResult) = diff.scriptResults.headOption.value
        scriptResult.error.value.text should include(s"Invoke complexity limit = $complexityLimit is exceeded")

        d.appendBlock(invokeTx)
        val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
        invokeTxMeta.spentComplexity shouldBe complexityLimit
        invokeTxMeta.succeeded shouldBe false
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
      invokeTxMeta.succeeded shouldBe false
    }
  }

  property("NODE-727 A sum of inner invokes is unlimited in RideV6") {
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
      invokeTxMeta.succeeded shouldBe true
    }
  }

  private val complexityExamples = Seq(
    // 1 for toInt, 200 for log, 65+65 for parseBigInt
    331 -> s"""toInt(log(parseBigIntValue("1625"), 2, parseBigIntValue("27"), 1, 2, HALFUP))""",
    100 -> s"""log(1625, 2, 27, 1, 2, HALFUP)""",
    12  -> s"""valueOrElse(getInteger("k"), 0)""" // 2 for valueOrElse, 10 for getInteger
  )

  private def mkIntExprWithComplexity(targetComplexity: Int): String = {
    @tailrec
    def loop(restComplexity: Int, restComplexityExamples: Seq[(Int, String)], acc: String): String =
      if (restComplexity == 0) acc
      else
        restComplexityExamples match {
          case (exprComplexity, expr) +: restXs =>
            val uncompletedExprComplexity = exprComplexity + 1 // 1 for "+"
            val times                     = restComplexity / uncompletedExprComplexity
            loop(restComplexity % uncompletedExprComplexity, restXs, acc + s" + $expr" * times)

          case Nil => loop(0, Nil, acc + " + 1" * restComplexity)
        }

    loop(targetComplexity, complexityExamples, "1")
  }
}
