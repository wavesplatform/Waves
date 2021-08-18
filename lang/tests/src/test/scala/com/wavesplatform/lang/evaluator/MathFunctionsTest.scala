package com.wavesplatform.lang.evaluator
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, EVALUATED}
import com.wavesplatform.test.produce

class MathFunctionsTest extends EvaluatorSpec {
  private implicit val startVersion: StdLibVersion = V3

  property("math functions") {
    eval("pow(12, 1, 3456, 3, 2, DOWN)") shouldBe Right(CONST_LONG(187))
    eval("pow(12, 1, 3456, 3, 2, UP)")(V3, checkNext = false) shouldBe Right(CONST_LONG(188))
    eval("pow(0, 1, 3456, 3, 2, UP)")(V3, checkNext = false) shouldBe Right(CONST_LONG(0))
    eval("pow(20, 1, -1, 0, 4, DOWN)") shouldBe Right(CONST_LONG(5000))
    eval("pow(-20, 1, -1, 0, 4, DOWN)") shouldBe Right(CONST_LONG(-5000))
    eval("pow(0, 1, -1, 0, 4, DOWN)") shouldBe Symbol("left")
    eval("log(16, 0, 2, 0, 0, CEILING)") shouldBe Right(CONST_LONG(4))
    eval("log(16, 0, -2, 0, 0, CEILING)") shouldBe Symbol("left")
    eval("log(-16, 0, 2, 0, 0, CEILING)") shouldBe Symbol("left")
  }

  property("math functions scale limits") {
    eval("pow(2,  0, 2, 9, 0, DOWN)") should produce("out of range 0-8")
    eval("log(2,  0, 2, 9, 0, DOWN)") should produce("out of range 0-8")
    eval("pow(2, -2, 2, 0, 5, DOWN)") should produce("out of range 0-8")
    eval("log(2, -2, 2, 0, 5, DOWN)") should produce("out of range 0-8")
  }

  property("pow result size max") {
    eval("pow(2, 0, 62, 0, 0, DOWN)") shouldBe Right(CONST_LONG(Math.pow(2, 62).toLong))
    eval("pow(2, 0, 63, 0, 0, DOWN)") should produce("out of long range")
  }

  property("pow result size abs min") {
    eval("pow(10, 0, -8, 0, 8, HALFUP)") shouldBe Right(CONST_LONG(1))
    eval("pow(10, 0, -9, 0, 8, HALFUP)") shouldBe Right(CONST_LONG(0))
  }
}
