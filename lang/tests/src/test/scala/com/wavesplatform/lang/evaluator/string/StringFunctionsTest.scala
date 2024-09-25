package com.wavesplatform.lang.evaluator.string

import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
import com.wavesplatform.test.*

class StringFunctionsTest extends EvaluatorSpec {
  private val min   = Long.MinValue
  private val max   = Long.MaxValue
  private val limit = 32767

  private val maxString = "a" * limit

  property("take") {
    eval(""" take("abc", 0) """) shouldBe CONST_STRING("")
    eval(""" take("abc", 2) """) shouldBe CONST_STRING("ab")
    eval(s""" take("abc", $limit) """) shouldBe CONST_STRING("abc")

    evalVerRange(s""" take("abc", $max) """, V1, V5) shouldBe CONST_STRING("abc")
    eval(s""" take("abc", $max) """)(V6) shouldBe Left(s"Number = $max passed to take() exceeds String limit = $limit")
    eval(s""" take("abc", ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to take() exceeds String limit = $limit")

    evalVerRange(""" take("abc", -1) """, V1, V5) shouldBe CONST_STRING("")
    eval(""" take("abc", -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to take()")
    evalVerRange(s""" take("abc", $min) """, V1, V5) shouldBe CONST_STRING("")
    eval(s""" take("abc", $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to take()")

    eval(s"""take("$maxString", $limit) """) shouldBe CONST_STRING(maxString)
  }

  property("takeRight") {
    eval(""" takeRight("abc", 0) """) shouldBe CONST_STRING("")
    eval(""" takeRight("abc", 2) """) shouldBe CONST_STRING("bc")
    eval(s""" takeRight("abc", $limit) """) shouldBe CONST_STRING("abc")

    evalVerRange(s""" takeRight("abc", $max) """, V1, V5) shouldBe CONST_STRING("abc")
    eval(s""" takeRight("abc", $max) """)(V6) shouldBe Left(s"Number = $max passed to takeRight() exceeds String limit = $limit")
    eval(s""" takeRight("abc", ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to takeRight() exceeds String limit = $limit")

    evalVerRange(""" takeRight("abc", -1) """, V1, V5) shouldBe CONST_STRING("")
    eval(""" takeRight("abc", -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to takeRight()")
    evalVerRange(s""" takeRight("abc", $min) """, V1, V5) should produce("long overflow")
    eval(s""" takeRight("abc", $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to takeRight()")

    eval(s"""takeRight("$maxString", $limit) """) shouldBe CONST_STRING(maxString)
  }

  property("drop") {
    eval(""" drop("abc", 0) """) shouldBe CONST_STRING("abc")
    eval(""" drop("abc", 2) """) shouldBe CONST_STRING("c")
    eval(s""" drop("abc", $limit) """) shouldBe CONST_STRING("")

    evalVerRange(s""" drop("abc", $max) """, V1, V5) shouldBe CONST_STRING("")
    eval(s""" drop("abc", $max) """)(V6) shouldBe Left(s"Number = $max passed to drop() exceeds String limit = $limit")
    eval(s""" drop("abc", ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to drop() exceeds String limit = $limit")

    evalVerRange(""" drop("abc", -1) """, V1, V5) shouldBe CONST_STRING("abc")
    eval(""" drop("abc", -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to drop()")
    evalVerRange(s""" drop("abc", $min) """, V1, V5) shouldBe CONST_STRING("abc")
    eval(s""" drop("abc", $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to drop()")

    eval(s"""drop("$maxString", $limit) """) shouldBe CONST_STRING("")
  }

  property("dropRight") {
    eval(""" dropRight("abc", 0) """) shouldBe CONST_STRING("abc")
    eval(""" dropRight("abc", 2) """) shouldBe CONST_STRING("a")
    eval(s""" dropRight("abc", $limit) """) shouldBe CONST_STRING("")

    evalVerRange(s""" dropRight("abc", $max) """, V1, V5) shouldBe CONST_STRING("")
    eval(s""" dropRight("abc", $max) """)(V6) shouldBe Left(s"Number = $max passed to dropRight() exceeds String limit = $limit")
    eval(s""" dropRight("abc", ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to dropRight() exceeds String limit = $limit")

    evalVerRange(""" dropRight("abc", -1) """, V1, V5) shouldBe CONST_STRING("abc")
    eval(""" dropRight("abc", -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to dropRight()")
    evalVerRange(s""" dropRight("abc", $min) """, V1, V5) should produce("long overflow")
    eval(s""" dropRight("abc", $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to dropRight()")

    eval(s"""dropRight("$maxString", $limit) """) shouldBe CONST_STRING("")
  }

  property("size") {
    eval(""" "".size() """) shouldBe Right(CONST_LONG(0))
    eval(""" "abc".size() """) shouldBe Right(CONST_LONG(3))
    eval(s""" "$maxString".size() """) shouldBe Right(CONST_LONG(limit))
  }

  property("indexOf") {
    eval(""" "qweqwe".indexOf("we") """)(V3) shouldBe Right(CONST_LONG(1L))
  }

  property("indexOf with zero offset") {
    eval(""" "qweqwe".indexOf("qw", 0) """)(V3) shouldBe Right(CONST_LONG(0L))
  }

  property("indexOf with start offset") {
    eval(""" "qweqwe".indexOf("we", 2) """)(V3) shouldBe Right(CONST_LONG(4L))
  }

  property("indexOf from end of max sized string") {
    val n = 32766
    eval(s""" "${"a" * n}z".indexOf("z", $n) """)(V3) shouldBe Right(CONST_LONG(n))
  }

  property("indexOf (not present)") {
    eval(""" "qweqwe".indexOf("ww") """)(V3) shouldBe Right(unit)
  }

  property("indexOf from empty string") {
    eval(""" "".indexOf("!") """)(V3) shouldBe Right(unit)
  }

  property("indexOf from empty string with offset") {
    eval(""" "".indexOf("!", 1) """)(V3) shouldBe Right(unit)
  }

  property("indexOf from string with Long.MaxValue offset") {
    eval(s""" "abc".indexOf("c", ${Long.MaxValue}) """)(V3) shouldBe Right(unit)
  }

  property("indexOf from string with negative offset") {
    eval(""" "abc".indexOf("a", -1) """)(V3) shouldBe Right(unit)
  }

  property("indexOf from string with negative Long.MinValue offset") {
    eval(s""" "abc".indexOf("a", ${Long.MinValue}) """)(V3) shouldBe Right(unit)
  }

  property("indexOf empty string from non-empty string") {
    eval(""" "abc".indexOf("") """)(V3) shouldBe Right(CONST_LONG(0))
  }

  property("indexOf empty string from empty string") {
    eval(""" "".indexOf("") """)(V3) shouldBe Right(CONST_LONG(0))
  }

  property("lastIndexOf") {
    eval(""" "qweqwe".lastIndexOf("we") """)(V3) shouldBe Right(CONST_LONG(4))
  }

  property("lastIndexOf with zero offset") {
    eval(""" "qweqwe".lastIndexOf("qw", 0) """)(V3) shouldBe Right(CONST_LONG(0))
  }

  property("lastIndexOf with start offset") {
    eval(""" "qweqwe".lastIndexOf("we", 4) """)(V3) shouldBe Right(CONST_LONG(4L))
  }

  property("lastIndexOf from end of max sized string") {
    val n = 32766
    eval(s""" "${"a" * n}z".lastIndexOf("z", $n) """)(V3) shouldBe Right(CONST_LONG(n))
  }

  property("lastIndexOf (not present)") {
    eval(""" "qweqwe".lastIndexOf("ww") """)(V3) shouldBe Right(unit)
  }

  property("lastIndexOf from empty string") {
    eval(""" "".lastIndexOf("!") """)(V3) shouldBe Right(unit)
  }

  property("lastIndexOf from empty string with offset") {
    eval(""" "".lastIndexOf("!", 1) """)(V3) shouldBe Right(unit)
  }

  property("lastIndexOf from string with Int.MaxValue offset") {
    eval(s""" "abc".lastIndexOf("c", $max) """)(V3) shouldBe Right(CONST_LONG(2))
  }

  property("lastIndexOf from string with Long.MaxValue offset") {
    eval(s""" "abc".lastIndexOf("c", ${Long.MaxValue}) """)(V3) shouldBe Right(CONST_LONG(2))
  }

  property("lastIndexOf from string with negative offset") {
    eval(""" "abc".lastIndexOf("a", -1) """)(V3) shouldBe Right(unit)
  }

  property("lastIndexOf from string with negative Long.MinValue offset") {
    eval(s""" "abc".lastIndexOf("a", ${Long.MinValue}) """)(V3) shouldBe Right(unit)
  }

  property("lastIndexOf empty string from non-empty string") {
    val str = "abcde"
    eval(s""" "$str".lastIndexOf("") """)(V3) shouldBe Right(CONST_LONG(str.length))
  }

  property("lastIndexOf empty string from empty string") {
    eval(""" "".lastIndexOf("") """)(V3) shouldBe Right(CONST_LONG(0))
  }

  property("string contains") {
    implicit val v: StdLibVersion = V4
    eval(""" "qwerty".contains("we") """) shouldBe Right(CONST_BOOLEAN(true))
    eval(""" "qwerty".contains("xx") """) shouldBe Right(CONST_BOOLEAN(false))
    eval(s""" "$maxString".contains("$maxString") """) shouldBe Right(CONST_BOOLEAN(true))
  }
}
