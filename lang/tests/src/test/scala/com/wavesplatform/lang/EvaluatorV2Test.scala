package com.wavesplatform.lang

import cats.implicits._
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.{Decompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class EvaluatorV2Test extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink with Inside {
  private val version = V4
  private val ctx =
    PureContext.build(Global, version).withEnvironment[Environment] |+|
    WavesContext.build(DirectiveSet.contractDirectiveSet)

  private def eval(expr: EXPR, limit: Int): (EXPR, String, Int) = {
    val environment = Common.emptyBlockchainEnvironment()
    val evaluator = new EvaluatorV2(limit, version)
    val evaluatorCtx = evaluator.Context(ctx.evaluationContext(environment))
    val (resultExpr, resultCtx) = evaluator.root(expr, evaluatorCtx)
    (resultExpr, Decompiler(resultExpr, ctx.decompilerContext), resultCtx.cost)
  }

  private def eval(script: String, limit: Int): Either[String, (EXPR, String, Int)] = {
    val parsed = Parser.parseExpr(script).get.value
    ExpressionCompiler(ctx.compilerContext, parsed)
      .map(compiled => eval(compiled._1, limit))
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
        cost shouldBe 12
        decompiled shouldBe "111333"
    }

    inside(eval(script, limit = 13)) {
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
        cost shouldBe 10
        decompiled shouldBe "-4"
    }
  }

  property("multiple user functions and refs") {
    inside(eval(
      """
        | let x = 10
        | let a = 1                                  # complexity
        | func f(a: Int, b: Int) = a - b + x         #     5
        | let b = 2
        | func g(a: Int, b: Int) = a * b             #     3
        | let expected = (a - b + x) * (b - a + x)   #     11
        | let actual = g(f(a, b), f(b, a))           #     3 + 5 * 2 + 4 = 17
        | actual == expected &&                      #     11 + 17 + 4 = 32
        | actual == expected                         #     3
      """.stripMargin,
      limit = 35
    )) {
      case Right((_, decompiled, cost)) =>
        cost shouldBe 35
        decompiled shouldBe "true"
    }
  }

  property("if block by step") {
    val script = "if (2 > 1) then 1 + 2 + 3 else 3 + 4"

    inside(eval(script, limit = 0)) {
      case Right((_, decompiled, cost)) =>
        cost shouldBe 0
        decompiled shouldBe
          """
            |if ((2 > 1))
            |    then ((1 + 2) + 3)
            |    else (3 + 4)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 1)) {
      case Right((_, decompiled, cost)) =>
        cost shouldBe 1
        decompiled shouldBe
          """
            |if (true)
            |    then ((1 + 2) + 3)
            |    else (3 + 4)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 2)) {
      case Right((_, decompiled, cost)) =>
        cost shouldBe 2
        decompiled shouldBe "((1 + 2) + 3)"
    }

    inside(eval(script, limit = 3)) {
      case Right((_, decompiled, cost)) =>
        cost shouldBe 3
        decompiled shouldBe "(3 + 3)"
    }

    inside(eval(script, limit = 4)) {
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
        cost shouldBe 0
        decompiled shouldBe script
    }

    inside(eval(script, limit = 1)) {
      case Right((_, decompiled, cost)) =>
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
      case Right((_, decompiled, cost)) =>
        cost shouldBe 2
        decompiled shouldBe "base58'aaaa'"
    }
  }

/*  property("stop if complexity exceeds limit") {
    val n = 50
    val script =
      s"""
         | func f0() = 0
         | ${(0 until n).map(i => s"func f${i + 1}() = if (true) then f$i() else f$i()").mkString("\n") }
         | f$n()
      """.stripMargin
    // produce 101 complexity

    eval(script) shouldBe 'left
  }*/
}