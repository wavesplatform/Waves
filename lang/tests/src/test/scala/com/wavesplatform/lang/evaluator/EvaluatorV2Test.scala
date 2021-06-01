package com.wavesplatform.lang.evaluator

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.{Common, Global}
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Decompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2.EvaluationException
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV2, FunctionIds}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.annotation.tailrec
import scala.util.Random

class EvaluatorV2Test extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink with Inside {
  private val version = V4
  private val ctx =
    PureContext.build(version, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
    WavesContext.build(Global, DirectiveSet(version, Account, DApp).explicitGet())

  private val environment = Common.emptyBlockchainEnvironment()
  private val evaluator =
    new EvaluatorV2(LoggedEvaluationContext(_ => _ => (), ctx.evaluationContext(environment)), version)

  private def eval(expr: EXPR, limit: Int): (EXPR, String, Int) = {
    val (result, unusedComplexity) = evaluator(expr, limit)
    (result, Decompiler(result, ctx.decompilerContext), limit - unusedComplexity)
  }

  private def eval(script: String, limit: Int): (EXPR, String, Int) =
    eval(compile(script), limit)

  private def compile(script: String): EXPR = {
    val parsed = Parser.parseExpr(script).get.value
    ExpressionCompiler(ctx.compilerContext, parsed).explicitGet()._1
  }

  property("multiple lets by step") {
    val script =
      """
        | let a = 1 + 10 + 100
        | let b = 1000 + a + 10000
        | let c = a + b + 100000
        | c + a
      """.stripMargin

    inside(eval(script, limit = 0)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe
          """
            |let a = ((1 + 10) + 100)
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 1)) {
      case (_, decompiled, cost) =>
        cost shouldBe 1
        decompiled shouldBe
          """
            |let a = (11 + 100)
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 2)) {
      case (_, decompiled, cost) =>
        cost shouldBe 2
        decompiled shouldBe
          """
            |let a = 111
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 3)) {
      case (_, decompiled, cost) =>
        cost shouldBe 3
        decompiled shouldBe
          """
            |let a = 111
            |let b = ((1000 + a) + 10000)
            |let c = ((111 + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 4)) {
      case (_, decompiled, cost) =>
        cost shouldBe 4
        decompiled shouldBe
          """
            |let a = 111
            |let b = ((1000 + 111) + 10000)
            |let c = ((111 + b) + 100000)
            |(c + a)
          """.stripMargin.trim

    }

    inside(eval(script, limit = 5)) {
      case (_, decompiled, cost) =>
        cost shouldBe 5
        decompiled shouldBe
          """
            |let a = 111
            |let b = (1111 + 10000)
            |let c = ((111 + b) + 100000)
            |(c + a)
          """.stripMargin.trim

    }

    inside(eval(script, limit = 6)) {
      case (_, decompiled, cost) =>
        cost shouldBe 6
        decompiled shouldBe
          """
            |let a = 111
            |let b = 11111
            |let c = ((111 + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 7)) {
      case (_, decompiled, cost) =>
        cost shouldBe 7
        decompiled shouldBe
          """
            |let a = 111
            |let b = 11111
            |let c = ((111 + 11111) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 8)) {
      case (_, decompiled, cost) =>
        cost shouldBe 8
        decompiled shouldBe
          """
            |let a = 111
            |let b = 11111
            |let c = (11222 + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 9)) {
      case (_, decompiled, cost) =>
        cost shouldBe 9
        decompiled shouldBe
          """
            |let a = 111
            |let b = 11111
            |let c = 111222
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 10)) {
      case (_, decompiled, cost) =>
        cost shouldBe 10
        decompiled shouldBe
          """
            |let a = 111
            |let b = 11111
            |let c = 111222
            |(111222 + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 11)) {
      case (_, decompiled, cost) =>
        cost shouldBe 11
        decompiled shouldBe
          """
            |let a = 111
            |let b = 11111
            |let c = 111222
            |(111222 + 111)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 12)) {
      case (_, decompiled, cost) =>
        cost shouldBe 12
        decompiled shouldBe "111333"
    }

    inside(eval(script, limit = 13)) {
      case (_, decompiled, cost) =>
        cost shouldBe 12
        decompiled shouldBe "111333"
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

    inside(eval(script, limit = 0)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe
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

    inside(eval(script, limit = 1)) {
      case (_, decompiled, cost) =>
        cost shouldBe 1
        decompiled shouldBe
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

    inside(eval(script, limit = 2)) {
      case (_, decompiled, cost) =>
        cost shouldBe 2
        decompiled shouldBe
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

    inside(eval(script, limit = 3)) {
      case (_, decompiled, cost) =>
        cost shouldBe 3
        decompiled shouldBe
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

    inside(eval(script, limit = 4)) {
      case (_, decompiled, cost) =>
        cost shouldBe 4
        decompiled shouldBe
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

    inside(eval(script, limit = 5)) {
      case (_, decompiled, cost) =>
        cost shouldBe 5
        decompiled shouldBe
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

    inside(eval(script, limit = 6)) {
      case (_, decompiled, cost) =>
        cost shouldBe 6
        decompiled shouldBe
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

    inside(eval(script, limit = 7)) {
      case (_, decompiled, cost) =>
        cost shouldBe 7
        decompiled shouldBe
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

    inside(eval(script, limit = 8)) {
      case (_, decompiled, cost) =>
        cost shouldBe 8
        decompiled shouldBe
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

    inside(eval(script, limit = 9)) {
      case (_, decompiled, cost) =>
        cost shouldBe 9
        decompiled shouldBe
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

    inside(eval(script, limit = 10)) {
      case (_, decompiled, cost) =>
        cost shouldBe 10
        decompiled shouldBe "-4"
    }
  }

  property("multiple user functions and refs") {
    inside(eval(
      """                                            # complexity
        | let x = 1 + 1 + 1                          # 2 (should be calculated once)
        | let a = 1 + 1                              # 1 (should be calculated once)
        | func f(a: Int, b: Int) = a - b + x         # 5
        | let b = 4                                  #
        | func g(a: Int, b: Int) = a * b             # 3
        | let expected = (a - b + x) * (b - a + x)   # 11
        | let actual = g(f(a, b), f(b, a))           # 3 + 5 * 2 + 4 = 17
        | actual == expected &&                      # 11 + 17 + 4 = 32
        | actual == expected &&                      # 4
        | x == 3             &&                      # 3
        | a == 2             &&                      # 3
        | b == 4                                     # 2  Total: 32 + 4 + 3 + 3 + 2 + 2 (x value) + 1 (a value) = 47
      """.stripMargin,
      limit = 47
    )) {
      case (_, decompiled, cost) =>
        cost shouldBe 47
        decompiled shouldBe "true"
    }
  }

  property("let overlap through function param") {
    inside(eval(
      """                                   # complexity
        | let x = 1 + 1 + 1 + 1 + 1         # 4
        | let y = x + 1                     # 2
        |
        | func f(x: Int) = x + 1            # 2
        | func g(x: Int) = x + 1 + 1        # 3
        | func h(x: Int) = x + 1 + 1 + 1    # 4
        |
        | f(g(h(y))) == x + x + 2
        |
        | # Total: 2 (f) + 3 (g) + 4(h) + 1 (y ref) + 2 (y value) + 1 (==) + 2 (2 x ref) + 4 (x value) + 2 (2 +)
      """.stripMargin,
      limit = 21
    )) {
      case (_, decompiled, cost) =>
        cost shouldBe 21
        decompiled shouldBe "true"
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

    val (_, decompiled, cost) = eval(expr, limit = 6)
    cost shouldBe 6
    decompiled shouldBe "4"
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

    (the[EvaluationException] thrownBy eval(expr, limit = 100)).getMessage shouldBe "A definition of 'b' not found"

    val expr2 =
      BLOCK(
        LET("a", FUNCTION_CALL(FunctionHeader.User("b"), Nil)),
        BLOCK(
          FUNC("b", Nil, REF("a")),
          REF("a")
        )
      )

    (the[EvaluationException] thrownBy eval(expr2, limit = 100)).getMessage shouldBe "Function or type 'b' not found"
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

    (the[EvaluationException] thrownBy eval(expr, limit = 100)).getMessage shouldBe "A definition of 'x' not found"

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

    (the[NoSuchElementException] thrownBy eval(expr2, limit = 100)).getMessage shouldBe "Function or type 'g' not found"
  }

  property("if block by step") {
    val script = "if (2 > 1) then 1 + 2 + 3 else 3 + 4"

    inside(eval(script, limit = 0)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe
          """
            |if ((2 > 1))
            |    then ((1 + 2) + 3)
            |    else (3 + 4)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 1)) {
      case (_, decompiled, cost) =>
        cost shouldBe 1
        decompiled shouldBe
          """
            |if (true)
            |    then ((1 + 2) + 3)
            |    else (3 + 4)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 2)) {
      case (_, decompiled, cost) =>
        cost shouldBe 2
        decompiled shouldBe "((1 + 2) + 3)"
    }

    inside(eval(script, limit = 3)) {
      case (_, decompiled, cost) =>
        cost shouldBe 3
        decompiled shouldBe "(3 + 3)"
    }

    inside(eval(script, limit = 4)) {
      case (_, decompiled, cost) =>
        cost shouldBe 4
        decompiled shouldBe "6"
    }
  }

  property("getter by step") {
    val script =
      """
        |let address = Address(base58'aaaa')
        |address.bytes
      """.stripMargin.trim

    inside(eval(script, limit = 0)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe script
    }

    inside(eval(script, limit = 1)) {
      case (_, decompiled, cost) =>
        cost shouldBe 1
        decompiled shouldBe
        """
          |let address = Address(
          |	bytes = base58'aaaa'
          |)
          |Address(
          |	bytes = base58'aaaa'
          |).bytes
        """.stripMargin.trim
    }

    inside(eval(script, limit = 2)) {
      case (_, decompiled, cost) =>
        cost shouldBe 2
        decompiled shouldBe "base58'aaaa'"
    }
  }

  property("big function assignment chain") {
    val count = 3000
    val script =
      s"""
         | func a0() = {
         |   1 + 1
         | }
         | ${1 to count map (i => s"func a$i() = a${i - 1}()") mkString "\n"}
         | a$count() == a$count()
      """.stripMargin

    eval(script, 10000)
  }

  property("big let assignment chain with function") {
    val count = 5000
    val script =
      s"""
         | let a0 = 1
         | ${1 to count map (i => s"let a$i = a${i - 1} + 1") mkString "\n"}
         | a$count == a$count
      """.stripMargin

    eval(script, 20000)._3
  }

  property("let ctx") {
    val expr =
      BLOCK(
        LET("a", FUNCTION_CALL(FunctionHeader.Native(FunctionIds.SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
        BLOCK(
          LET("b", REF("a")),
          BLOCK(
            FUNC("g", Nil, BLOCK(
              LET("a", FUNCTION_CALL(
                FunctionHeader.Native(FunctionIds.SUM_LONG),
                List(
                  FUNCTION_CALL(
                    FunctionHeader.Native(FunctionIds.SUM_LONG),
                    List(CONST_LONG(2), CONST_LONG(2))
                  ),
                  CONST_LONG(2)
                )
              )),
              BLOCK(
                LET("c", REF("a")),
                FUNCTION_CALL(
                  FunctionHeader.Native(FunctionIds.SUM_LONG),
                  List(REF("c"),
                    FUNCTION_CALL(
                      FunctionHeader.Native(FunctionIds.SUM_LONG),
                      List(REF("b"), REF("a"))
                    )
                  )
                )
              )
            )),
            FUNCTION_CALL(
              FunctionHeader.Native(FunctionIds.SUM_LONG),
              List(FUNCTION_CALL(FunctionHeader.User("g"), Nil), REF("a"))
            )
          )
        )
      )
      /*
                                  # Complexity  Value
           let a = 1 + 1          # 1 (once)    2
           let b = a              # 1 (once)    2
                                  #
           func g() = {           #
             let a = 2 + 2 + 2    # 2 (once)    6
             let c = a            # 1 (once)    6
             c + b + a            # 5           14
           }

           g() + a                # 7           16
                                  # Total: 12   Result: 16
      */

    val (_, result, cost) = eval(expr, 100)
    result shouldBe "16"
    cost shouldBe 12
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
      if (acc.size + 1 == piecesNumber)
        expectedSum - generatedSum :: acc
      else {
        val max = expectedSum - generatedSum - piecesNumber + acc.size + 1
        val distributionCoefficient = random.nextInt(Math.min(max, piecesNumber)) + 1
        val next = random.nextInt(max / distributionCoefficient) + 1
        randomPieces(expectedSum, piecesNumber, generatedSum + next, next :: acc)
      }

    val (evaluated, _, precalculatedComplexity) = eval(script, 1500)
    val startCost = 0
    def expr() = compile(script)

    val piecesGen = Gen.choose(2, 100)
      .map(randomPieces(precalculatedComplexity, _))

    forAll(piecesGen) { pieces =>
      val (resultExpr, summarizedCost) =
        pieces.foldLeft((expr(), startCost)) {
          case ((currentExpr, costSum), nextCostLimit) =>
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

    inside(eval(strictScript, limit = 100)) {
      case (expr, _, cost) =>
        expr shouldBe CONST_LONG(100542)
        cost shouldBe 6
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

    (the[RuntimeException] thrownBy eval(strictScript, limit = 100)).getMessage shouldBe "Strict executed error"
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

    inside(eval(defaultScript, limit = 100)) {
      case (expr, _, cost) =>
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

    inside(eval(strictScript, limit = 100)) {
      case (expr, _, cost) =>
        expr shouldBe CONST_STRING("42").explicitGet()
        cost shouldBe 15
    }
  }

  property("no checks for constructor") {
    val exprWithCorrectArgs = FUNCTION_CALL(
      FunctionHeader.User("IntegerEntry"),
      List(CONST_STRING("key").explicitGet(), CONST_LONG(1))
    )
    eval(exprWithCorrectArgs, 100)._2 shouldBe
      """
        |IntegerEntry(
        |	key = "key"
        |	value = 1
        |)
      """.stripMargin.trim

    val exprWithIllegalArgs = FUNCTION_CALL(
      FunctionHeader.User("IntegerEntry"),
      List(CONST_STRING("key").explicitGet(), CONST_BOOLEAN(true))
    )
    eval(exprWithIllegalArgs, 100)._2 shouldBe
      """
        |IntegerEntry(
        |	key = "key"
        |	value = true
        |)
      """.stripMargin.trim

    val exprWithTooManyArgs = FUNCTION_CALL(
      FunctionHeader.User("IntegerEntry"),
      List(CONST_STRING("key").explicitGet(), CONST_BOOLEAN(true), CONST_LONG(1))
    )
    eval(exprWithTooManyArgs, 100)._2 shouldBe
      """
        |IntegerEntry(
        |	key = "key"
        |	value = true
        |)
      """.stripMargin.trim
  }
}
