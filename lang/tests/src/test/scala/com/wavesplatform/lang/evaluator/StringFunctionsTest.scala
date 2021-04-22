package com.wavesplatform.lang.evaluator

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.produce
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit

class StringFunctionsTest extends EvaluatorSpec {
  property("take") {
    eval(""" take("abc", 0) """) shouldBe CONST_STRING("")
    eval(""" take("abc", 2) """) shouldBe CONST_STRING("ab")
    eval(""" take("abc", 100) """) shouldBe CONST_STRING("abc")
    eval(""" take("abc", -100) """) shouldBe CONST_STRING("")
    eval(s"""take("${"a" * Short.MaxValue}", ${Short.MaxValue}) """) shouldBe CONST_STRING("a" * Short.MaxValue)
  }

  property("takeRight") {
    eval(""" takeRight("abc", 0) """) shouldBe CONST_STRING("")
    eval(""" takeRight("abc", 2) """) shouldBe CONST_STRING("bc")
    eval(""" takeRight("abc", 100) """) shouldBe CONST_STRING("abc")
    eval(""" takeRight("abc", -100) """) shouldBe CONST_STRING("")
    eval(s"""takeRight("${"a" * Short.MaxValue}", ${Short.MaxValue}) """) shouldBe CONST_STRING("a" * Short.MaxValue)
  }

  property("drop") {
    eval(""" drop("abc", 0) """) shouldBe CONST_STRING("abc")
    eval(""" drop("abc", 2) """) shouldBe CONST_STRING("c")
    eval(""" drop("abc", 100) """) shouldBe CONST_STRING("")
    eval(""" drop("abc", -100) """) shouldBe CONST_STRING("abc")
    eval(s"""drop("${"a" * Short.MaxValue}", ${Short.MaxValue}) """) shouldBe CONST_STRING("")
  }

  property("dropRight") {
    eval(""" dropRight("abc", 0) """) shouldBe CONST_STRING("abc")
    eval(""" dropRight("abc", 2) """) shouldBe CONST_STRING("a")
    eval(""" dropRight("abc", 100) """) shouldBe CONST_STRING("")
    eval(""" dropRight("abc", -100) """) shouldBe CONST_STRING("abc")
    eval(s"""dropRight("${"a" * Short.MaxValue}", ${Short.MaxValue}) """) shouldBe CONST_STRING("")
  }

  property("size") {
    eval(""" "".size() """) shouldBe Right(CONST_LONG(0))
    eval(""" "abc".size() """) shouldBe Right(CONST_LONG(3))
    eval(s""" "${"a" * Short.MaxValue}".size() """) shouldBe Right(CONST_LONG(Short.MaxValue))
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
    eval(s""" "abc".lastIndexOf("c", ${Int.MaxValue}) """)(V3) shouldBe Right(CONST_LONG(2))
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
    eval(s""" "${"a" * Short.MaxValue}".contains("${"a" * Short.MaxValue}") """) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("makeString") {
    implicit val v: StdLibVersion = V4
    eval(""" ["cat", "dog", "pig"].makeString(", ") """) shouldBe CONST_STRING("cat, dog, pig")
    eval(""" [].makeString(", ") """) shouldBe CONST_STRING("")
    eval(""" ["abc"].makeString(", ") == "abc" """) shouldBe Right(CONST_BOOLEAN(true))

    val script = s""" [${s""" "${"a" * 1000}", """ * 32} "${"a" * 704}"].makeString(", ") """
    eval(script) should produce("Constructing string size = 32768 bytes will exceed 32767")
    // 1000 * 32 + 704 + 2 * 32 = 32768

    val script2 = s""" [${s""" "${"a" * 1000}", """ * 32} "${"a" * 703}"].makeString(", ") """
    eval(script2).explicitGet().asInstanceOf[CONST_STRING].s.length shouldBe 32767
  }
}
