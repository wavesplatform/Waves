package com.wavesplatform.lang.v1

import cats.kernel.Monoid
import com.wavesplatform.lang.utils.environment
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class FoldTest extends PropSpec with PropertyChecks with Matchers with NoShrink {
  private def eval[T <: EVALUATED](code: String): Either[String, T] = {
    val untyped = Parser.parseExpr(code).get.value
    val ctx: CTX[Environment] =
      Monoid.combineAll(
        Seq(
          PureContext.build(V3, fixUnicodeFunctions = true).withEnvironment[Environment],
          WavesContext.build(Global, DirectiveSet.contractDirectiveSet),
          CryptoContext.build(Global, V3).withEnvironment[Environment]
        )
      )
    val typed = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => EvaluatorV1().apply[T](ctx.evaluationContext(environment), v._1))
  }

  property("sum") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | let total = FOLD<5>(arr, 9, sum)
         | total
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_LONG(1 + 2 + 3 + 4 + 5 + 9))
  }

  property("all is odd") {
    val script =
      s"""
         | func checkOdd(acc: Boolean, a: Int) = acc && (a % 2 == 1)
         | let arr = [1, 3, 5, 7]
         | FOLD<5>(arr, true, checkOdd)
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("limit") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | let total = FOLD<4>(arr, 9, sum)
         | total
         |
      """.stripMargin

    eval(script) shouldBe Left("List size exceed 4")
  }

  property("limit for limit") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | let total = FOLD<1001>(arr, 9, sum)
         | total
         |
      """.stripMargin

    val index = script.indexOf("1001")
    eval(script) shouldBe Left(s"Compilation failed: [List size limit in FOLD is oversized, 1001 must be less or equal 1000 in $index-${index+4}]")
  }

  property("Maximun limit") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [${"1,"*999}1]
         | let total = FOLD<1000>(arr, 2, sum)
         | total
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_LONG(1002L))
  }


  property("FOLD as FOLD param") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | let total = FOLD<5>(arr, FOLD<5>(arr, 9, sum), sum)
         | total
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_LONG(9 + 2 * (1 + 2 + 3 + 4 + 5)))
  }
}
