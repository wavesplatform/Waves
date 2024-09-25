package com.wavesplatform.lang.evaluator

import cats.syntax.either.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, *}
import com.wavesplatform.lang.v1.compiler.{Decompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV2, FunctionIds}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.test.*
import org.scalatest.Inside

import scala.annotation.tailrec
import scala.util.Random

class EvaluatorV2Test extends PropSpec with Inside {
  private val version     = V4
  private val ctx         = lazyContexts((DirectiveSet(version, Account, DApp).explicitGet(), true, true))()
  private val environment = Common.emptyBlockchainEnvironment()

  private def evalEither(expr: EXPR, limit: Int, newMode: Boolean): Either[String, (EXPR, Int)] =
    EvaluatorV2
      .applyLimitedCoeval(
        expr,
        LogExtraInfo(),
        limit,
        ctx.evaluationContext(environment),
        version,
        correctFunctionCallScope = true,
        newMode,
        fixedThrownError = true
      )
      .value()
      .bimap(_._1.message, { case (result, complexity, _) => (result, complexity) })

  private def evalBothEither(expr: EXPR, limit: Int): Either[String, (EXPR, Int)] = {
    val result  = evalEither(expr, limit, newMode = true)
    val result2 = evalEither(expr, limit, newMode = false)
    result shouldBe result2
    result
  }

  private def evalBoth(script: String, limit: Int): (EXPR, String, Int) = {
    val (result, unusedComplexity) = evalBothEither(compile(script), limit).explicitGet()
    (result, Decompiler(result, ctx.decompilerContext), limit - unusedComplexity)
  }

  private def evalNew(expr: EXPR, limit: Int): (EXPR, String, Int) = {
    val (result, unusedComplexity) = evalEither(expr, limit, newMode = true).explicitGet()
    (result, Decompiler(result, ctx.decompilerContext), limit - unusedComplexity)
  }

  private def evalNew(script: String, limit: Int): (EXPR, String, Int) =
    evalNew(compile(script), limit)

  private def evalOld(expr: EXPR, limit: Int): (EXPR, String, Int) = {
    val (result, unusedComplexity) = evalEither(expr, limit, newMode = false).explicitGet()
    (result, Decompiler(result, ctx.decompilerContext), limit - unusedComplexity)
  }

  private def evalOld(script: String, limit: Int): (EXPR, String, Int) =
    evalOld(compile(script), limit)

  private def compile(script: String): EXPR = {
    val parsed = Parser.parseExpr(script).get.value
    ExpressionCompiler(ctx.compilerContext, version, parsed).explicitGet()._1
  }

  property("multiple lets by step") {
    val script =
      """
        | let a = 1 + 10 + 100
        | let b = 1000 + a + 10000
        | let c = a + b + 100000
        | c + a
      """.stripMargin

    inside(evalOld(script, limit = 0)) { case (_, result, cost) =>
      cost shouldBe 0
      result shouldBe
        """
          |let a = ((1 + 10) + 100)
          |let b = ((1000 + a) + 10000)
          |let c = ((a + b) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 1)) { case (_, result, cost) =>
      cost shouldBe 1
      result shouldBe
        """
          |let a = (11 + 100)
          |let b = ((1000 + a) + 10000)
          |let c = ((a + b) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 2)) { case (_, result, cost) =>
      cost shouldBe 2
      result shouldBe
        """
          |let a = 111
          |let b = ((1000 + a) + 10000)
          |let c = ((a + b) + 100000)
          |(c + a)
        """.stripMargin.trim
    }

    inside(evalOld(script, limit = 3)) { case (_, result, cost) =>
      cost shouldBe 3
      result shouldBe
        """
          |let a = 111
          |let b = ((1000 + a) + 10000)
          |let c = ((111 + b) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 4)) { case (_, result, cost) =>
      cost shouldBe 4
      result shouldBe
        """
          |let a = 111
          |let b = ((1000 + 111) + 10000)
          |let c = ((111 + b) + 100000)
          |(c + a)
          """.stripMargin.trim

    }

    inside(evalOld(script, limit = 5)) { case (_, result, cost) =>
      cost shouldBe 5
      result shouldBe
        """
          |let a = 111
          |let b = (1111 + 10000)
          |let c = ((111 + b) + 100000)
          |(c + a)
          """.stripMargin.trim

    }

    inside(evalOld(script, limit = 6)) { case (_, result, cost) =>
      cost shouldBe 6
      result shouldBe
        """
          |let a = 111
          |let b = 11111
          |let c = ((111 + b) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 7)) { case (_, result, cost) =>
      cost shouldBe 7
      result shouldBe
        """
          |let a = 111
          |let b = 11111
          |let c = ((111 + 11111) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 8)) { case (_, result, cost) =>
      cost shouldBe 8
      result shouldBe
        """
          |let a = 111
          |let b = 11111
          |let c = (11222 + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 9)) { case (_, result, cost) =>
      cost shouldBe 9
      result shouldBe
        """
          |let a = 111
          |let b = 11111
          |let c = 111222
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 10)) { case (_, result, cost) =>
      cost shouldBe 10
      result shouldBe
        """
          |let a = 111
          |let b = 11111
          |let c = 111222
          |(111222 + a)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 11)) { case (_, result, cost) =>
      cost shouldBe 11
      result shouldBe
        """
          |let a = 111
          |let b = 11111
          |let c = 111222
          |(111222 + 111)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 12)) { case (_, result, cost) =>
      cost shouldBe 12
      result shouldBe "111333"
    }

    inside(evalOld(script, limit = 13)) { case (_, result, cost) =>
      cost shouldBe 12
      result shouldBe "111333"
    }

    inside(evalNew(script, limit = 0)) { case (_, result, cost) =>
      cost shouldBe 0
      result shouldBe
        """
          |let a = ((1 + 10) + 100)
          |let b = ((1000 + a) + 10000)
          |let c = ((a + b) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 1)) { case (_, result, cost) =>
      cost shouldBe 1
      result shouldBe
        """
          |let a = (11 + 100)
          |let b = ((1000 + a) + 10000)
          |let c = ((a + b) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 2)) { case (_, result, cost) =>
      cost shouldBe 2
      result shouldBe
        """
          |let a = 111
          |let b = ((1000 + a) + 10000)
          |let c = ((a + b) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 3)) { case (_, result, cost) =>
      cost shouldBe 3
      result shouldBe
        """
          |let a = 111
          |let b = (1111 + 10000)
          |let c = ((111 + b) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 4)) { case (_, result, cost) =>
      cost shouldBe 4
      result shouldBe
        """
          |let a = 111
          |let b = 11111
          |let c = ((111 + b) + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 5)) { case (_, result, cost) =>
      cost shouldBe 5
      result shouldBe
        """
          |let a = 111
          |let b = 11111
          |let c = (11222 + 100000)
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 6)) { case (_, result, cost) =>
      cost shouldBe 6
      result shouldBe
        """
          |let a = 111
          |let b = 11111
          |let c = 111222
          |(c + a)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 7)) { case (_, result, cost) =>
      cost shouldBe 7
      result shouldBe "111333"
    }
  }

  property("user function evaluation by step") {
    val script =
      """
        | func f(a: Int, b: Int) = {
        |   let c = a + b
        |   let d = a - b
        |   c * d - 1
        | }
        | f(1, 2)
      """.stripMargin

    inside(evalOld(script, limit = 0)) { case (_, result, cost) =>
      cost shouldBe 0
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |f(1, 2)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 1)) { case (_, result, cost) =>
      cost shouldBe 1
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = (1 + b)
          |let d = (a - b)
          |((c * d) - 1)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 2)) { case (_, result, cost) =>
      cost shouldBe 2
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = (1 + 2)
          |let d = (a - b)
          |((c * d) - 1)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 3)) { case (_, result, cost) =>
      cost shouldBe 3
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = (a - b)
          |((c * d) - 1)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 4)) { case (_, result, cost) =>
      cost shouldBe 4
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = (a - b)
          |((3 * d) - 1)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 5)) { case (_, result, cost) =>
      cost shouldBe 5
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = (1 - b)
          |((3 * d) - 1)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 6)) { case (_, result, cost) =>
      cost shouldBe 6
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = (1 - 2)
          |((3 * d) - 1)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 7)) { case (_, result, cost) =>
      cost shouldBe 7
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = -1
          |((3 * d) - 1)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 8)) { case (_, result, cost) =>
      cost shouldBe 8
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = -1
          |((3 * -1) - 1)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 9)) { case (_, result, cost) =>
      cost shouldBe 9
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = -1
          |(-3 - 1)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 10)) { case (_, result, cost) =>
      cost shouldBe 10
      result shouldBe "-4"
    }

    inside(evalNew(script, limit = 0)) { case (_, result, cost) =>
      cost shouldBe 0
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |f(1, 2)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 1)) { case (_, result, cost) =>
      cost shouldBe 1
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = (a - b)
          |((c * d) - 1)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 2)) { case (_, result, cost) =>
      cost shouldBe 2
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = -1
          |((3 * d) - 1)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 3)) { case (_, result, cost) =>
      cost shouldBe 3
      result shouldBe
        """
          |func f (a,b) = {
          |    let c = (a + b)
          |    let d = (a - b)
          |    ((c * d) - 1)
          |    }
          |
          |let a = 1
          |let b = 2
          |let c = 3
          |let d = -1
          |(-3 - 1)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 4)) { case (_, result, cost) =>
      cost shouldBe 4
      result shouldBe "-4"
    }
  }

  property("multiple user functions and refs") {
    val script =
      """                                            # old complexity                 new complexity
        | let x = 1 + 1 + 1                          # 2 (should be calculated once)  2
        | let a = 1 + 1                              # 1 (should be calculated once)  1
        | func f(a: Int, b: Int) = a - b + x         # 5                              2
        | let b = 4                                  #
        | func g(a: Int, b: Int) = a * b             # 3                              1
        | let expected = (a - b + x) * (b - a + x)   # 11                             5
        | let actual = g(f(a, b), f(b, a))           # 3 + 5 * 2 + 4 = 17             1 + 2 * 2 = 5
        | actual == expected &&                      # 11 + 17 + 4 = 32               5 + 5 + 1 = 11
        | actual == expected &&                      # 4                              1
        | x == 3             &&                      # 3                              1
        | a == 2             &&                      # 3                              1
        | b == 4                                     # 2                              1
        |
        | # Total old: 32 + 4 + 3 + 3 + 2 + 2 (x value) + 1 (a value) = 47
        | # Total new: 11 + 1 + 1 + 1 + 1 + 2 (x value) + 1 (a value) = 18
      """.stripMargin

    inside(evalOld(script, limit = 47)) { case (_, result, cost) =>
      cost shouldBe 47
      result shouldBe "true"
    }

    inside(evalNew(script, limit = 18)) { case (_, result, cost) =>
      cost shouldBe 18
      result shouldBe "true"
    }
  }

  property("let overlap through function param") {
    val script =
      """                                   # old complexity  new complexity
        | let x = 1 + 1 + 1 + 1 + 1         # 4               4
        | let y = x + 1                     # 2               1
        |
        | func f(x: Int) = x + 1            # 2               1
        | func g(x: Int) = x + 1 + 1        # 3               2
        | func h(x: Int) = x + 1 + 1 + 1    # 4               3
        |
        | f(g(h(y))) == x + x + 2
        |
        | # Total old: 2 (f) + 3 (g) + 4(h) + 1 (y ref) + 2 (y value) + 1 (==) + 2 (2 x ref) + 4 (x value) + 2 (2 +) = 21
        | # Total new: 1 (f) + 2 (g) + 3(h) + 1 (y value) + 1 (==) + 4 (x value) + 2 (2 +) = 14
      """.stripMargin

    inside(evalOld(script, limit = 21)) { case (_, result, cost) =>
      cost shouldBe 21
      result shouldBe "true"
    }

    inside(evalNew(script, limit = 14)) { case (_, result, cost) =>
      cost shouldBe 14
      result shouldBe "true"
    }
  }

  property("let overlap inside let value block") {
    val expr = BLOCK(
      LET(
        "x",
        FUNCTION_CALL(
          FunctionHeader.Native(FunctionIds.SUM_LONG),
          List(
            FUNCTION_CALL(
              FunctionHeader.Native(FunctionIds.SUM_LONG),
              List(CONST_LONG(1), CONST_LONG(1))
            ),
            CONST_LONG(1)
          )
        )
      ),
      BLOCK(
        LET(
          "y",
          BLOCK(LET("x", CONST_LONG(1)), REF("x"))
        ),
        FUNCTION_CALL(
          FunctionHeader.Native(FunctionIds.SUM_LONG),
          List(REF("y"), REF("x"))
        )
      )
    )

    /*                   # complexity
      let x = 1 + 1 + 1  # 2
      let y = {          # 1
        let x = 1        #
        x                # 1
      }
      y + x              # 1 (y ref) + 1 (+) + 1 (x ref) + 2 (x value) + 1 (y value)
     */

    val (_, result, cost) = evalOld(expr, limit = 6)
    cost shouldBe 6
    result shouldBe "4"

    val (_, result2, cost2) = evalNew(expr, limit = 6)
    cost2 shouldBe 3
    result2 shouldBe "4"
  }

  property("let recursion") {
    val expr =
      BLOCK(
        LET("a", REF("b")),
        BLOCK(
          LET("b", REF("a")),
          REF("a")
        )
      )

    evalBothEither(expr, limit = 100) shouldBe Left("A definition of 'b' not found")

    val expr2 =
      BLOCK(
        LET("a", FUNCTION_CALL(FunctionHeader.User("b"), Nil)),
        BLOCK(
          FUNC("b", Nil, REF("a")),
          REF("a")
        )
      )

    evalBothEither(expr2, limit = 100) shouldBe Left("Function or type 'b' not found")
  }

  property("function context leak") {
    val expr = BLOCK(
      FUNC("f", Nil, BLOCK(LET("x", CONST_LONG(1)), REF("x"))),
      FUNCTION_CALL(
        FunctionHeader.Native(FunctionIds.SUM_LONG),
        List(
          FUNCTION_CALL(FunctionHeader.User("f"), Nil),
          REF("x")
        )
      )
    )

    /*
      func f() = {
        let x = 1
        x
      }
      f() + x
     */

    evalBothEither(expr, limit = 100) shouldBe Left("A definition of 'x' not found")

    val expr2 = BLOCK(
      FUNC("f", Nil, BLOCK(FUNC("g", Nil, CONST_LONG(1)), FUNCTION_CALL(FunctionHeader.User("g"), Nil))),
      FUNCTION_CALL(
        FunctionHeader.Native(FunctionIds.SUM_LONG),
        List(
          FUNCTION_CALL(FunctionHeader.User("f"), Nil),
          FUNCTION_CALL(FunctionHeader.User("g"), Nil)
        )
      )
    )

    /*
      func f() = {
        func g() = 1
        g()
      }
      f() + g()
     */

    evalBothEither(expr2, limit = 100) shouldBe Left("Function or type 'g' not found")
  }

  property("if block by step") {
    val script = "if (2 > 1) then 1 + 2 + 3 else 3 + 4"

    inside(evalOld(script, limit = 0)) { case (_, result, cost) =>
      cost shouldBe 0
      result shouldBe
        """
          |if ((2 > 1))
          |    then ((1 + 2) + 3)
          |    else (3 + 4)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 1)) { case (_, result, cost) =>
      cost shouldBe 1
      result shouldBe
        """
          |if (true)
          |    then ((1 + 2) + 3)
          |    else (3 + 4)
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 2)) { case (_, result, cost) =>
      cost shouldBe 2
      result shouldBe "((1 + 2) + 3)"
    }

    inside(evalOld(script, limit = 3)) { case (_, result, cost) =>
      cost shouldBe 3
      result shouldBe "(3 + 3)"
    }

    inside(evalOld(script, limit = 4)) { case (_, result, cost) =>
      cost shouldBe 4
      result shouldBe "6"
    }

    inside(evalNew(script, limit = 0)) { case (_, result, cost) =>
      cost shouldBe 0
      result shouldBe
        """
          |if ((2 > 1))
          |    then ((1 + 2) + 3)
          |    else (3 + 4)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 1)) { case (_, result, cost) =>
      cost shouldBe 1
      result shouldBe
        """
          |if (true)
          |    then ((1 + 2) + 3)
          |    else (3 + 4)
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 2)) { case (_, result, cost) =>
      cost shouldBe 2
      result shouldBe "(3 + 3)"
    }

    inside(evalNew(script, limit = 3)) { case (_, result, cost) =>
      cost shouldBe 3
      result shouldBe "6"
    }
  }

  property("getter by step") {
    val script =
      """
        |let address = Address(base58'aaaa')
        |address.bytes
      """.stripMargin.trim

    inside(evalOld(script, limit = 0)) { case (_, result, cost) =>
      cost shouldBe 0
      result shouldBe script
    }

    inside(evalOld(script, limit = 1)) { case (_, result, cost) =>
      cost shouldBe 1
      result shouldBe
        """
          |let address = Address(
          |	bytes = base58'aaaa'
          |)
          |Address(
          |	bytes = base58'aaaa'
          |).bytes
          """.stripMargin.trim
    }

    inside(evalOld(script, limit = 2)) { case (_, result, cost) =>
      cost shouldBe 2
      result shouldBe "base58'aaaa'"
    }

    inside(evalNew(script, limit = 1)) { case (_, result, cost) =>
      cost shouldBe 1
      result shouldBe
        """
          |let address = Address(
          |	bytes = base58'aaaa'
          |)
          |address.bytes
          """.stripMargin.trim
    }

    inside(evalNew(script, limit = 2)) { case (_, result, cost) =>
      cost shouldBe 1
      result shouldBe "base58'aaaa'"
    }
  }

  property("big function assignment chain") {
    val count = 5000
    val script =
      s"""
         | func a0() = 1 + 1
         | ${1 to count map (i => s"func a$i() = a${i - 1}()") mkString "\n"}
         | a$count() == a$count()
      """.stripMargin

    val (_, result, cost) = evalBoth(script, 10000)
    result shouldBe "true"
    cost shouldBe 3
  }

  property("big let assignment chain with function") {
    val count = 5000
    val script =
      s"""
         | let a0 = 1
         | ${1 to count map (i => s"let a$i = a${i - 1} + 1") mkString "\n"}
         | a$count == a$count
      """.stripMargin

    val (_, result, cost) = evalOld(script, 10003)
    result shouldBe "true"
    cost shouldBe 10003

    val (_, result2, cost2) = evalNew(script, 5001)
    result2 shouldBe "true"
    cost2 shouldBe 5001
  }

  property("let ctx") {
    val expr =
      BLOCK(
        LET("a", FUNCTION_CALL(FunctionHeader.Native(FunctionIds.SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
        BLOCK(
          LET("b", REF("a")),
          BLOCK(
            FUNC(
              "g",
              Nil,
              BLOCK(
                LET(
                  "a",
                  FUNCTION_CALL(
                    FunctionHeader.Native(FunctionIds.SUM_LONG),
                    List(
                      FUNCTION_CALL(
                        FunctionHeader.Native(FunctionIds.SUM_LONG),
                        List(CONST_LONG(2), CONST_LONG(2))
                      ),
                      CONST_LONG(2)
                    )
                  )
                ),
                BLOCK(
                  LET("c", REF("a")),
                  FUNCTION_CALL(
                    FunctionHeader.Native(FunctionIds.SUM_LONG),
                    List(
                      REF("c"),
                      FUNCTION_CALL(
                        FunctionHeader.Native(FunctionIds.SUM_LONG),
                        List(REF("b"), REF("a"))
                      )
                    )
                  )
                )
              )
            ),
            FUNCTION_CALL(
              FunctionHeader.Native(FunctionIds.SUM_LONG),
              List(FUNCTION_CALL(FunctionHeader.User("g"), Nil), REF("a"))
            )
          )
        )
      )
    /*
                                  # Old complexity  New complexity  Value
           let a = 1 + 1          # 1 (once)        1               2
           let b = a              # 1 (once)        0               2
                                  #
           func g() = {           #
             let a = 2 + 2 + 2    # 2 (once)        2               6
             let c = a            # 1 (once)        0               6
             c + b + a            # 5               2               14
           }

           g() + a                # 7               6
                                  # Total: 12       Total: 6        Result: 16
     */

    val (_, result, cost) = evalOld(expr, 100)
    result shouldBe "16"
    cost shouldBe 12

    val (_, result2, cost2) = evalNew(expr, 100)
    result2 shouldBe "16"
    cost2 shouldBe 6
  }

  property("big script randomly splitted") {
    val body =
      """
        | func f(a: Int) = {
        |   func f(a: Int) = {
        |     func f(a: Int) = {
        |       func g(a: Int) = {
        |         func h(a: Int) = {
        |           func f(a: Int) = a
        |           f(a)
        |         }
        |         h(a)
        |       }
        |       g(a)
        |     }
        |     f(a)
        |   }
        |   1 + f(a) + height
        | }
        | func g(a: Int) = f(1) + f(a)
        | func h(a: Int) = f(1) + g(a) + g(a)
        |
        |
        | let a = {
        |   if (false) then throw()
        |   else {
        |     let a = throw()
        |     let b = 1
        |     b + b
        |   }
        | }
        | let b = 1
        | let length = Address((let bytes = base58'aaaa'; bytes)).bytes.size()
        | if (h(a) == f(a) + g(a) + length)
        |   then (h(b) + f(b) + g(b) + 1) > (func ff(a: Int) = a + b; ff(h(f(a))))
        |   else (h(b) + f(b) + g(b) + 1) > (func ff(a: Int) = a + b; ff(h(f(a))))
        |
      """.stripMargin

    val script =
      s"""
         | func r(x: Boolean) =
         |  if (x)
         |   then {
         |     $body
         |   } else {
         |     $body
         |   }
         |
         | let y = {
         |   $body
         | }
         | r(r(y)) == r(r(true))
         |
       """.stripMargin

    val random = new Random()
    /*
      a = (a(1) + ... + a(n))
      a(i) = random from (0, a - (a1 + ... + a(i-1)) - n + i]
      a(n) = a - (a1 + ... + a(i-1))
     */
    @tailrec def randomPieces(
        expectedSum: Int,
        piecesNumber: Int,
        generatedSum: Int = 0,
        acc: List[Int] = Nil
    ): List[Int] =
      if (acc.size + 1 == piecesNumber) expectedSum - generatedSum :: acc
      else {
        val max                     = expectedSum - generatedSum - piecesNumber + acc.size + 1
        val distributionCoefficient = random.nextInt(Math.min(max, piecesNumber)) + 1
        val next                    = random.nextInt(max / distributionCoefficient) + 1
        randomPieces(expectedSum, piecesNumber, generatedSum + next, next :: acc)
      }

    def expr      = compile(script)
    val startCost = 0

    Seq(
      evalOld(_: EXPR, _: Int),
      evalNew(_: EXPR, _: Int)
    ).foreach { eval =>
      val (evaluated, _, precalculatedComplexity) = eval(expr, 1500)

      val pieces = randomPieces(precalculatedComplexity, Random.nextInt(99) + 2)
      val (resultExpr, summarizedCost) =
        pieces.foldLeft((expr, startCost)) { case ((currentExpr, costSum), nextCostLimit) =>
          currentExpr should not be an[EVALUATED]
          val (nextExpr, _, cost) = eval(currentExpr, nextCostLimit)
          (nextExpr, cost + costSum)
        }
      summarizedCost shouldBe precalculatedComplexity
      resultExpr shouldBe evaluated
    }
  }

  property("strict evaluation") {
    val strictScript =
      """
        |func testFunc() = {
        |  strict a = 100500 + 42
        |  a
        |}
        |testFunc()
        |
      """.stripMargin.trim

    inside(evalOld(strictScript, 100)) { case (expr, _, cost) =>
      expr shouldBe CONST_LONG(100542)
      cost shouldBe 6
    }

    inside(evalNew(strictScript, 100)) { case (expr, _, cost) =>
      expr shouldBe CONST_LONG(100542)
      cost shouldBe 2
    }
  }

  property("strict with throw expression") {
    val strictScript =
      """
        |func testFunc() = {
        |  strict a = throw("Strict executed error")
        |  true
        |}
        |testFunc()
        |
      """.stripMargin.trim

    evalBothEither(compile(strictScript), limit = 100) shouldBe Left("Strict executed error")
  }

  property("strict var add cost without usage") {
    val defaultScript =
      """
        |func testFunc() = {
        |  let a = 1 + 2 + 3 + 4 + 5 + 5 + 6 + 7 + 8 + 9 + 100500
        |  let z = "42"
        |  z
        |}
        |testFunc()
        |
      """.stripMargin.trim

    inside(evalBoth(defaultScript, 100)) { case (expr, _, cost) =>
      expr shouldBe CONST_STRING("42").explicitGet()
      cost shouldBe 1
    }

    val strictScript =
      """
        |func testFunc() = {
        |  strict a = 1 + 2 + 3 + 4 + 5 + 5 + 6 + 7 + 8 + 9 + 100500
        |  let z = "42"
        |  z
        |}
        |testFunc()
        |
      """.stripMargin.trim

    inside(evalOld(strictScript, 100)) { case (expr, _, cost) =>
      expr shouldBe CONST_STRING("42").explicitGet()
      cost shouldBe 15
    }
    inside(evalNew(strictScript, 100)) { case (expr, _, cost) =>
      expr shouldBe CONST_STRING("42").explicitGet()
      cost shouldBe 11
    }
  }

  property("no checks for constructor") {
    val exprWithCorrectArgs = FUNCTION_CALL(
      FunctionHeader.User("IntegerEntry"),
      List(CONST_STRING("key").explicitGet(), CONST_LONG(1))
    )
    val expected =
      """
        |IntegerEntry(
        |	key = "key"
        |	value = 1
        |)
      """.stripMargin.trim
    evalOld(exprWithCorrectArgs, 100)._2 shouldBe expected
    evalNew(exprWithCorrectArgs, 100)._2 shouldBe expected

    val exprWithIllegalArgs = FUNCTION_CALL(
      FunctionHeader.User("IntegerEntry"),
      List(CONST_STRING("key").explicitGet(), CONST_BOOLEAN(true))
    )
    val expectedBool =
      """
        |IntegerEntry(
        |	key = "key"
        |	value = true
        |)
      """.stripMargin.trim
    evalOld(exprWithIllegalArgs, 100)._2 shouldBe expectedBool
    evalOld(exprWithIllegalArgs, 100)._2 shouldBe expectedBool

    val exprWithTooManyArgs = FUNCTION_CALL(
      FunctionHeader.User("IntegerEntry"),
      List(CONST_STRING("key").explicitGet(), CONST_BOOLEAN(true), CONST_LONG(1))
    )
    evalOld(exprWithTooManyArgs, 100)._2 shouldBe expectedBool
    evalNew(exprWithTooManyArgs, 100)._2 shouldBe expectedBool
  }

  property("arg of the first function should NOT overlap var accessed from body of the second function AFTER fix") {
    val script =
      """
        | let a = 4
        | func g(b: Int) = a
        | func f(a: Int) = g(a)
        | f(1)
      """.stripMargin

    evalOld(script, 1000)._1 shouldBe CONST_LONG(4)
    evalNew(script, 1000)._1 shouldBe CONST_LONG(4)
  }

  property("arg of the function should not overlap var accessed from the let") {
    val script =
      """
        | let a = 4
        | let x = a
        | func f(a: Int) = x
        | f(1)
      """.stripMargin

    evalOld(script, 1000)._1 shouldBe CONST_LONG(4)
    evalNew(script, 1000)._1 shouldBe CONST_LONG(4)
  }

  property("updated evaluator should use predefined user function complexity") {
    evalOld("1 != 1", 100) shouldBe ((FALSE, "false", 5))
    evalNew("1 != 1", 100) shouldBe ((FALSE, "false", 1))

    val script =
      """
        | let x =
        |   if (1 != 1)
        |     then throw()
        |     else 1 != 1
        |
        | func f() =
        |   if (1 != 1)
        |     then throw()
        |     else 1 != 1
        |
        | f() || x
        |
      """.stripMargin

    evalOld(script, 100) shouldBe ((FALSE, "false", 24))
    evalNew(script, 100) shouldBe ((FALSE, "false", 4))
  }

  property("function call should not be free for new mode") {
    val script =
      """
        | func f() = true
        | f()
      """.stripMargin

    evalOld(script, 100) shouldBe ((TRUE, "true", 0))
    evalNew(script, 100) shouldBe ((TRUE, "true", 1))

    val script2 =
      """
        | func f() = true
        | func g() = {
        |   func g1() = {
        |     let x = {
        |       func g2() = true
        |       g2()
        |     }
        |     x
        |   }
        |   g1()
        | }
        | func h() = if true then g() else f()
        |
        | f() && g() && h()
      """.stripMargin

    evalOld(script2, 100) shouldBe ((TRUE, "true", 5)) // 3 conditions + ref twice
    evalNew(script2, 100) shouldBe ((TRUE, "true", 3)) // 3 function call
  }

  property("estimator global vars problem cases") {
    val script =
      """
        | func f(a: Boolean) = {
        |   func g(b1: Boolean, b2: Boolean) = b1 || b2
        |   a || g(true, true)
        | }
        | let a  = groth16Verify(base58'', base58'', base58'')
        | let b1 = groth16Verify(base58'', base58'', base58'')
        | let b2 = groth16Verify(base58'', base58'', base58'')
        | f(true)
      """.stripMargin
    evalNew(script, 100)._3 shouldBe 1

    val script2 =
      """
        | let a = sigVerify(base58'', base58'', base58'')
        | func f() = a
        | f()
      """.stripMargin
    evalNew(script2, 1000)._3 shouldBe 200

    val script3 =
      """
        | func f(a: Int) = a
        | f(1 + 2 + 3)
      """.stripMargin
    evalNew(script3, 100)._3 shouldBe 3
  }
}
