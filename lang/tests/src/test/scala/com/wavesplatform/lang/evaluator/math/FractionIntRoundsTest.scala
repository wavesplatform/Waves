package com.wavesplatform.lang.evaluator.math

import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5, V6}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.evaluator.ctx.impl.Rounding._
import com.wavesplatform.test._

class FractionIntRoundsTest extends EvaluatorSpec {
  private implicit val version: StdLibVersion = V5

  private val max = Long.MaxValue
  private val min = Long.MinValue

  property("fraction with long limits") {
    val limitError = "-85070591730234615856620279821087277056 out of integers range"
    eval(s"fraction($max, $min, 1, HALFEVEN)")(V5, checkNext = false) should produce("toInt: BigInt " + limitError)
    eval(s"fraction($max, $min, 1, HALFEVEN)")(V6) should produce("Fraction result " + limitError)

    eval(s"fraction($max, $min, $min, HALFEVEN)") shouldBe Right(CONST_LONG(max))
    eval(s"fraction(1, $min, 1, HALFEVEN)") shouldBe Right(CONST_LONG(min))
  }

  property("plain fraction with long limits") {
    eval(s"fraction($max, $min, 1)") should produce("Long overflow")
    eval(s"fraction($max, $min, $min)") shouldBe Right(CONST_LONG(max))
    eval(s"fraction(1, $min, 1)") shouldBe Right(CONST_LONG(min))

    DirectiveDictionary[StdLibVersion].all
      .filter(_ < V5)
      .foreach { v =>
        eval(s"fraction($max, $min, $min)")(v, checkNext = false) should produce("Long overflow")
        eval(s"fraction(1, $min, 1)")(v, checkNext = false) should produce("Long overflow")
      }
  }

  property("divide by zero") {
    eval(s"fraction(1, -1, 0)") should produce("Fraction: division by zero")
    eval(s"fraction(1, -1, 0, HALFEVEN)") should produce("Fraction: division by zero")
  }

  property("fraction roundings") {
    // https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/math/RoundingMode.html
    val exprs = List[String => String](
      r => s"fraction(5, 1, 2, $r)",   //  2.5
      r => s"fraction(2, 4, 5, $r)",   //  1.6
      r => s"fraction(-2, 4, 5, $r)",  // -1.6
      r => s"fraction(-5, 11, 10, $r)" // -5.5
    )
    val resultByRounding = Map(
      Down     -> List(2, 1, -1, -5),
      HalfUp   -> List(3, 2, -2, -6),
      HalfEven -> List(2, 2, -2, -6),
      Ceiling  -> List(3, 2, -1, -5),
      Floor    -> List(2, 1, -2, -6)
    )
    for {
      (expr, i)           <- exprs.zipWithIndex
      (rounding, results) <- resultByRounding
    } eval(expr(rounding.definition._1)) shouldBe Right(CONST_LONG(results(i)))
  }
}
