package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5, V6}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{EitherValues, OptionValues}

import scala.collection.Seq

class SyncDAppLimits extends PropSpec with WithDomain with OptionValues with EitherValues {
  private val alice     = TxHelpers.signer(1) // signer(0) forges blocks and this affects the balance
  private val aliceAddr = alice.toAddress

  complexityLimitTest("NODE-521 A limit of complexity is 52000 if the first script is V6", V6, 52000)
  complexityLimitTest("NODE-527 A limit of complexity is 26000 if the first script is V6", V5, 26000)
  private def complexityLimitTest(title: String, v: StdLibVersion, complexityLimit: Int): Unit = {
    property(title) {
      withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(alice)) { d =>
        d.appendBlock(
          TxHelpers.setScript(
            alice,
            TestCompiler(v).compileContract(
              // 1 for ">", 1+1 for strict, 75 for invoke, 1 for Address, 1 for "-", 1 for list
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

  property("NODE-726 A limit of inner invokes is 100 before the ContinuationTransaction(id=18) feature activation") {
    // 160 calls, 52000/325 = 160
    withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(alice)) { d =>
      d.appendBlock(
        TxHelpers.setScript(
          alice,
          TestCompiler(V6).compileContract(
            // 1 for ">", 1+1 for strict, 75 for invoke, 1 for Address, 1 for "-", 1 for list
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

      val calls    = 101 // 52000 / 2000 + 1
      val invokeTx = TxHelpers.invoke(aliceAddr, Some("foo"), Seq(CONST_LONG(calls)), invoker = alice)

      val diff              = d.createDiffE(invokeTx).value
      val (_, scriptResult) = diff.scriptResults.headOption.value
      scriptResult.error.value.text should include("DApp calls limit = 100 is exceeded")

      d.appendBlock(invokeTx)
      val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
      // invokeTxMeta.spentComplexity shouldBe complexityLimit
      invokeTxMeta.succeeded shouldBe false
    }
  }

  // TODO
  property("NODE-727 A number of inner invokes is unlimited after the ContinuationTransaction(id=18) feature activation") {
    // 160 calls, 52000/325 = 160
    withDomain(DomainPresets.ContinuationTransaction, AddrWithBalance.enoughBalances(alice)) { d =>
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
               |     ([], 0)
               |   }
               | }
               | """.stripMargin
          ),
          fee = 1.waves
        )
      )

      val calls    = 120 // 52000 / 2000 + 1
      val invokeTx = TxHelpers.invoke(aliceAddr, Some("foo"), Seq(CONST_LONG(calls)), invoker = alice)

      val diff              = d.createDiffE(invokeTx).value
      val (_, scriptResult) = diff.scriptResults.headOption.value
      scriptResult.error.value.text should include("????")

      d.appendBlock(invokeTx)
      val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
      // invokeTxMeta.spentComplexity shouldBe complexityLimit
      invokeTxMeta.succeeded shouldBe false
    }
  }

  private def mkIntExprWithComplexity(targetComplexity: Int): String = {
    if (targetComplexity == 0) "1"
    else if (targetComplexity < 13) mkIntExprWithComplexityM1(targetComplexity)
    else if (targetComplexity < 101) {
      val m13 = mkIntExprWithComplexityM13(targetComplexity / 13)
      s"$m13 + ${mkIntExprWithComplexity(targetComplexity % 13 - 1)}" // 1 for "+"
    } else if (targetComplexity < 332) {
      val m101 = mkIntExprWithComplexityM101(targetComplexity / 101)
      s"$m101 + ${mkIntExprWithComplexity(targetComplexity % 101 - 1)}" // 1 for "+"
    } else {
      val m332 = mkIntExprWithComplexityM332(targetComplexity / 332)
      s"$m332 + ${mkIntExprWithComplexity(targetComplexity % 332 - 1)}" // 1 for "+"
    }
  }

  private def mkIntExprWithComplexityM1(n: Int): String = s"1${" + 1" * n}"

  private def mkIntExprWithComplexityM13(n: Int): String = {
    val baseExpr = s"""log(1625, 2, 27, 1, 2, HALFUP)""" // 2 for valueOrElse, 10 for getInteger
    if (n <= 0) "1"
    else if (n == 1) s"$baseExpr + 1" // 13
    else s"$baseExpr + 1${s" + $baseExpr" * (n - 1)}"
  }

  private def mkIntExprWithComplexityM101(n: Int): String = {
    val baseExpr = s"""log(1625, 2, 27, 1, 2, HALFUP)""" // 100
    if (n <= 0) "1"
    else if (n == 1) s"$baseExpr + 1" // 101
    else s"$baseExpr + 1${s" + $baseExpr" * (n - 1)}"
  }

  private def mkIntExprWithComplexityM332(n: Int): String = {
    // 331 = 1 for toInt, 200 for log, 65+65 for parseBigInt
    val baseExpr = s"""toInt(log(parseBigIntValue("1625"), 2, parseBigIntValue("27"), 1, 2, HALFUP))"""
    if (n <= 0) "1"
    else if (n == 1) s"$baseExpr + 1"                 // 332
    else s"$baseExpr + 1${s" + $baseExpr" * (n - 1)}" // 332 * n
  }
}
