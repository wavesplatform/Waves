package com.wavesplatform.lang.evaluator
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V5}
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit

class BrokenUnicodeTest extends EvaluatorSpec {
  implicit val v: StdLibVersion = V5

  property("unicode broken") {
    implicit val v: StdLibVersion = V4
    implicit val checkNext: Boolean = false
    eval(s"""take("x冬x", 2)""") shouldBe CONST_STRING("x\ud87e")
    eval(s"""size("x冬x")""") shouldBe Right(CONST_LONG(4))
    eval(s"""drop("x冬x", 2)""") shouldBe CONST_STRING("\udc1ax")
    eval(s"""takeRight("x冬x", 2)""") shouldBe CONST_STRING("\udc1ax")
    eval(s"""dropRight("x冬x", 2)""") shouldBe CONST_STRING("x\ud87e")
  }

  property("unicode indexOf") {
    eval(""" "x冬xqweqwe".indexOf("we") """) shouldBe Right(CONST_LONG(4L))
    eval(""" "世界x冬x".take(4).indexOf("冬".take(1)) """)(V4) shouldBe Right(CONST_LONG(3L))
  }

  property("unicode indexOf with zero offset") {
    eval(""" "x冬xqweqwe".indexOf("x冬xqw", 0) """)(V4) shouldBe Right(CONST_LONG(0L))
  }

  property("unicode indexOf with start offset") {
    eval(""" "冬weqwe".indexOf("we", 2) """) shouldBe Right(CONST_LONG(4L))
  }

  property("unicode indexOf (not present)") {
    eval(""" "x冬xqweqwe".indexOf("ww") """) shouldBe Right(unit)
  }

  property("unicode indexOf from empty string") {
    eval(""" "".indexOf("x冬x") """) shouldBe Right(unit)
  }

  property("unicode indexOf from empty string with offset") {
    eval(""" "".indexOf("x冬x", 1) """) shouldBe Right(unit)
  }

  property("split unicode") {
    eval(""" "strx冬x1;🤦;🤦strx冬x2;🤦strx冬x3".split(";🤦") """) shouldBe
      ARR(
        IndexedSeq(
          CONST_STRING("strx冬x1").explicitGet(),
          CONST_STRING("").explicitGet(),
          CONST_STRING("strx冬x2").explicitGet(),
          CONST_STRING("strx冬x3").explicitGet()
        ),
        false
      )

    eval(""" "冬x🤦冬".split("") """) shouldBe
      ARR(
        IndexedSeq(
          CONST_STRING("冬").explicitGet(),
          CONST_STRING("x").explicitGet(),
          CONST_STRING("🤦").explicitGet(),
          CONST_STRING("冬").explicitGet()
        ),
        false
      )
  }

  property("unicode support") {
    eval(s"""take("x冬x", 2)""") shouldBe CONST_STRING("x冬")
    eval(s"""size("x冬x")""") shouldBe Right(CONST_LONG(3))
    eval(s"""drop("x冬x", 2)""") shouldBe CONST_STRING("x")
    eval(s"""takeRight("x冬x", 2)""") shouldBe CONST_STRING("冬x")
    eval(s"""dropRight("x冬x", 2)""") shouldBe CONST_STRING("x")
  }
}
