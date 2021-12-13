package com.wavesplatform.lang.evaluator.math

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V5, V6}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.test.produce

class MathFunctionsTest extends EvaluatorSpec {
  private implicit val startVersion: StdLibVersion = V3

  private val max = Long.MaxValue

  property("math functions") {
    eval("pow(12, 1, 3456, 3, 2, DOWN)") shouldBe Right(CONST_LONG(187))
    eval("pow(12, 1, 3456, 3, 2, UP)")(V3, checkNext = false) shouldBe Right(CONST_LONG(188))
    eval("pow(0, 1, 3456, 3, 2, UP)")(V3, checkNext = false) shouldBe Right(CONST_LONG(0))
    eval("pow(20, 1, -1, 0, 4, DOWN)") shouldBe Right(CONST_LONG(5000))
    eval("pow(-20, 1, -1, 0, 4, DOWN)") shouldBe Right(CONST_LONG(-5000))
    eval("pow(0, 1, -1, 0, 4, DOWN)") shouldBe Left("Division by zero")
    eval("log(16, 0, 2, 0, 0, CEILING)") shouldBe Right(CONST_LONG(4))
    eval("log(16, 0, -2, 0, 0, CEILING)") shouldBe Left("Illegal log(x) for x <= 0: x = -2")
    eval("log(-16, 0, 2, 0, 0, CEILING)") shouldBe Left("Illegal log(x) for x <= 0: x = -16")
  }

  property("math functions scale limits") {
    eval("pow(2,  0, 2, 9, 0, DOWN)") should produce("out of range 0-8")
    eval("log(2,  0, 2, 9, 0, DOWN)") should produce("out of range 0-8")
    eval("pow(2, -2, 2, 0, 5, DOWN)") should produce("out of range 0-8")
    eval("log(2, -2, 2, 0, 5, DOWN)") should produce("out of range 0-8")
  }

  property("pow result size max") {
    eval("pow(2, 0, 62, 0, 0, DOWN)") shouldBe Right(CONST_LONG(Math.pow(2, 62).toLong))
    eval("pow(2, 0, 63, 0, 0, DOWN)")(V3, checkNext = false) should produce("out of long range")
    eval("pow(2, 0, 63, 0, 0, DOWN)")(V5) should produce("out of long range")
  }

  property("pow result size abs min") {
    eval("pow(10, 0, -8, 0, 8, HALFUP)") shouldBe Right(CONST_LONG(1))
    eval("pow(10, 0, -9, 0, 8, HALFUP)") shouldBe Right(CONST_LONG(0))
  }

  property("manual check fractional number with huge exponent") {
    eval(s"pow(98765432, 8, $max, 8, 8, DOWN)") shouldBe Right(CONST_LONG(0))
    eval("pow(987654, 8, 987654321, 0, 0, DOWN)") shouldBe Right(CONST_LONG(0))
  }

  property("manual check overflow") {
    eval("pow(9, 0, 999999, 0, 0, FLOOR)") should produce("Pow overflow")
    eval("pow(987654321, 8, 98765432101234, 8, 8, DOWN)") should produce("Pow overflow")
  }

  property("pow result abs min with the greatest digits count") {
    eval("pow(98765432, 8, 145998765432, 8, 8, HALFUP)") shouldBe Right(CONST_LONG(1))
  }

  property("pow result close to max with the greatest digits count") {
    eval("pow(198765432, 8, 6298765432, 8, 0, DOWN)")(V5) shouldBe Right(CONST_LONG(6191427136334512235L))
  }

  property("on the limit") {
    eval(s"pow($max, 0, $max, 8, 8, DOWN)") shouldBe Left("Overflow")
    eval(s"pow($max, 0, $max, 0, 0, DOWN)") shouldBe Left("Overflow")
    eval(s"pow(-$max, 0, $max, 0, 0, DOWN)") shouldBe Left("Overflow")
    eval(s"pow($max, 0, -$max, 0, 8, DOWN)") shouldBe Left("Overflow")
    eval(s"pow(1, 8, -$max, 0, 8, DOWN)") shouldBe Left("Underflow")
    eval(s"pow(98765432, 8, -$max, 8, 8, DOWN)") shouldBe Left("Pow overflow")
    eval(s"pow(98765432, 8, -$max, 0, 8, DOWN)") shouldBe Left("Underflow")
    eval(s"pow(98765432, 8, $max, 8, 8, DOWN)") shouldBe Right(CONST_LONG(0))
    eval(s"pow(98765432, 8, $max, 0, 8, DOWN)") shouldBe Left("Underflow")
  }

  property("sqrt") {
    eval(s"pow(${Long.MaxValue}, 0, 5, 1, 8, DOWN)") shouldBe Right(CONST_LONG(303700049997604969L))
    eval(s"pow(${Long.MaxValue}, 8, 5, 1, 8, DOWN)") shouldBe Right(CONST_LONG(30370004999760L))
    eval(s"sqrt(${Long.MaxValue}, 0, 8, DOWN)")(V6) shouldBe Right(CONST_LONG(303700049997604969L))
    eval(s"sqrt(${Long.MaxValue}, 8, 8, DOWN)")(V6) shouldBe Right(CONST_LONG(30370004999760L))
  }
}
