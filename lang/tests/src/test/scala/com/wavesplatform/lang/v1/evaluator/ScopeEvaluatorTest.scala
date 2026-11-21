package com.wavesplatform.lang.v1.evaluator

import cats.Id
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.ctx.{EnabledLogEvaluationContext, EvaluationContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{Common, ExecutionError}
import com.wavesplatform.test.PropSpec

import scala.collection.mutable.ListBuffer

class ScopeEvaluatorTest extends PropSpec {
  private val version     = V6
  private val ctx         = lazyContexts((DirectiveSet(version, Account, DApp).explicitGet(), true, true, true))()
  private val environment = Common.emptyBlockchainEnvironment()

  private def compile(script: String): EXPR =
    ExpressionCompiler(ctx.compilerContext, version, Parser.parseExpr(script).get.value).explicitGet()._1

  private type Raw = Either[(String, Int), (Option[EVALUATED], Int)] // None: incomplete

  private def evaluateV2(expr: EXPR, limit: Int, newMode: Boolean, correctScope: Boolean, ec: EvaluationContext[Environment, Id]) =
    EvaluatorV2
      .applyLimitedCoevalV2(expr, LogExtraInfo(), limit, ec, version, correctScope, newMode, false, true, fixedThrownError = true)
      .value()

  private def evaluateScope(expr: EXPR, limit: Int, newMode: Boolean, correctScope: Boolean, ec: EvaluationContext[Environment, Id]) = {
    val log       = ListBuffer[LogItem[Id]]()
    val loggedCtx = EnabledLogEvaluationContext[Environment, Id](name => value => log.append((name, value)), ec)
    val outcome   = new ScopeEvaluator(loggedCtx, version, limit, correctScope, newMode, true, false, fixedThrownError = true).run(expr).attempt.value()
    (outcome, log.toList)
  }

  // Both evaluators on every limit up to the full cost of `script`: the scope evaluator completes exactly where EvaluatorV2 does, with the
  // same value or error, unused complexity and log.
  private def checkAllLimits(script: String): Unit = checkAllLimits(compile(script))

  private def checkAllLimits(expr: EXPR): Unit =
    for {
      newMode      <- Seq(true, false)
      correctScope <- Seq(true, false)
    } {
      val ec       = ctx.evaluationContext(environment)
      val fullCost = evaluateV2(expr, Int.MaxValue, newMode, correctScope, ec).fold(_._2, r => Int.MaxValue - r._2)
      val maxLimit = if (fullCost < 0 || fullCost > 2000) 2000 else fullCost + 1
      (0 to maxLimit).foreach { limit =>
        val clue                 = s"limit $limit, newMode $newMode, correctFunctionCallScope $correctScope"
        val expected             = evaluateV2(expr, limit, newMode, correctScope, ec)
        val (outcome, actualLog) = evaluateScope(expr, limit, newMode, correctScope, ec)
        val actual: Raw = outcome match {
          case Left(e)                               => Left((e.getMessage, limit))
          case Right(ScopeEvaluator.Incomplete)      => Right((None, -1))
          case Right(Left((e: ExecutionError, u)))   => Left((e.message, u))
          case Right(Right((v: EVALUATED, u: Int))) => Right((Some(v), u))
          case Right(other)                          => fail(s"unexpected outcome $other")
        }
        expected match {
          case Left((e, u, log)) =>
            withClue(clue)(actual shouldBe Left((e.message, u)))
            withClue(clue)(actualLog shouldBe log)
          case Right((v: EVALUATED, u, log)) =>
            withClue(clue)(actual shouldBe Right((Some(v), u)))
            withClue(clue)(actualLog shouldBe log)
          case Right(_) => withClue(clue)(actual shouldBe Right((None, -1)))
        }
      }
    }

  private def sum(a: EXPR, b: EXPR)  = FUNCTION_CALL(FunctionHeader.Native(FunctionIds.SUM_LONG), List(a, b))
  private def call(name: String)     = FUNCTION_CALL(FunctionHeader.User(name), Nil)
  private def let(name: String, value: EXPR, body: EXPR) = BLOCK(LET(name, value), body)

  property("lets referenced several times and nested") {
    checkAllLimits(
      """
        | let a = 1 + 10 + 100
        | let b = { let d = 5; d * 2 } + a
        | let c = a + b + a
        | c + a + b
      """.stripMargin
    )
  }

  // The compiler rejects shadowing, but a script is bytes: a LET may shadow another one.
  property("a shadowing LET hides the outer one in its scope only") {
    checkAllLimits(let("a", CONST_LONG(1), let("b", let("a", CONST_LONG(5), sum(REF("a"), REF("a"))), sum(REF("a"), REF("b")))))
  }

  property("user functions, nested, recursive through arguments and calling each other") {
    checkAllLimits(
      """
        | func inc(x: Int) = x + 1
        | func twice(x: Int) = { let y = inc(x); inc(y) + y }
        | func outer(a: Int, b: Int) = { func inner(c: Int) = c * a; inner(b) + twice(a) }
        | outer(twice(1), inc(2)) + twice(outer(1, 2))
      """.stripMargin
    )
  }

  property("a function body sees the caller's LETs only with the old function call scope") {
    checkAllLimits(
      let(
        "x",
        CONST_LONG(1),
        BLOCK(FUNC("f", Nil, REF("x")), BLOCK(FUNC("g", Nil, let("x", CONST_LONG(2), call("f"))), sum(call("g"), call("f"))))
      )
    )
  }

  property("ifs, matches and getters") {
    checkAllLimits(
      """
        | let l = [1, 2, 3]
        | let t = (1, "a", true)
        | let v = if (size(l) > 2) then l[1] else l[0]
        | let m = match t._2 {
        |   case s: String => s + "b"
        |   case _ => "c"
        | }
        | let o = if (t._3) then Address(base58'aaa') else Address(base58'bbb')
        | if (v == 2 && m == "ab") then o.bytes.size() + v else 0
      """.stripMargin
    )
  }

  property("folds") {
    checkAllLimits(
      """
        | func sum(acc: Int, e: Int) = acc + e
        | let xs = [1, 2, 3, 4, 5]
        | FOLD<5>(xs, 0, sum) + FOLD<5>(xs, 10, sum)
      """.stripMargin
    )
  }

  property("errors in LETs, functions and conditions") {
    checkAllLimits(
      """
        | let a = 1
        | let b = if (a == 1) then throw("boom") else 2
        | func f(x: Int) = if (x > 0) then b else x
        | a + f(1)
      """.stripMargin
    )
    checkAllLimits("""let a = [1, 2]; a[5] + 1""")
    checkAllLimits("""let x = 9223372036854775807; x + 1""")
  }

  property("value() errors describe the call they were given") {
    checkAllLimits("""let s = "a"; let k = s + "b"; getIntegerValue(Address(base58'aaa'), k)""")
    checkAllLimits("""func f() = unit; let x = if (true) then 1 else unit; value(x) + 1""")
  }

  property("constructors, global values and strings") {
    checkAllLimits(
      """
        | let addr = Address(base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC')
        | let s = toBase58String(addr.bytes) + toString(height)
        | size(s) + if (isDefined(getInteger(addr, "k"))) then 1 else 0
      """.stripMargin
    )
  }
}
