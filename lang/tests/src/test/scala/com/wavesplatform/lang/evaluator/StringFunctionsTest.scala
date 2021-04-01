package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, CONST_STRING, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{PureContext, unit}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StringFunctionsTest extends PropSpec with ScalaCheckPropertyChecks with ScriptGen with Matchers with NoShrink {
  implicit val startVersion: StdLibVersion = V3

  private def eval(code: String)(implicit startVersion: StdLibVersion): Either[String, EVALUATED] = {
    val parsedExpr = Parser.parseExpr(code).get.value
    val results = DirectiveDictionary[StdLibVersion].all
      .filter(_ >= startVersion)
      .map(version => eval(parsedExpr, version))
    if (results.toList.distinct.size == 1)
      results.head
    else
      throw new TestFailedException(s"Evaluation results are not the same: $results", 0)
  }

  private def eval(parsedExpr: Expressions.EXPR, version: StdLibVersion): Either[String, EVALUATED] = {
    val ctx           = PureContext.build(version).withEnvironment[Environment]
    val typed         = ExpressionCompiler(ctx.compilerContext, parsedExpr)
    val evaluationCtx = ctx.evaluationContext(Common.emptyBlockchainEnvironment())
    typed.flatMap(v => EvaluatorV2.applyCompleted(evaluationCtx, v._1, version)._3)
  }

  property("take") {
    eval(""" take("abc", 0) """)    shouldBe CONST_STRING("")
    eval(""" take("abc", 2) """)    shouldBe CONST_STRING("ab")
    eval(""" take("abc", 100) """)  shouldBe CONST_STRING("abc")
    eval(""" take("abc", -100) """) shouldBe CONST_STRING("")
  }

  property("takeRight") {
    eval(""" takeRight("abc", 0) """)    shouldBe CONST_STRING("")
    eval(""" takeRight("abc", 2) """)    shouldBe CONST_STRING("bc")
    eval(""" takeRight("abc", 100) """)  shouldBe CONST_STRING("abc")
    eval(""" takeRight("abc", -100) """) shouldBe CONST_STRING("")
  }

  property("drop") {
    eval(""" drop("abc", 0) """)    shouldBe CONST_STRING("abc")
    eval(""" drop("abc", 2) """)    shouldBe CONST_STRING("c")
    eval(""" drop("abc", 100) """)  shouldBe CONST_STRING("")
    eval(""" drop("abc", -100) """) shouldBe CONST_STRING("abc")
  }

  property("dropRight") {
    eval(""" dropRight("abc", 0) """)    shouldBe CONST_STRING("abc")
    eval(""" dropRight("abc", 2) """)    shouldBe CONST_STRING("a")
    eval(""" dropRight("abc", 100) """)  shouldBe CONST_STRING("")
    eval(""" dropRight("abc", -100) """) shouldBe CONST_STRING("abc")
  }

  property("size") {
    eval(""" "".size() """)    shouldBe Right(CONST_LONG(0))
    eval(""" "abc".size() """) shouldBe Right(CONST_LONG(3))
  }

  property("indexOf") {
    eval(""" "qweqwe".indexOf("we") """) shouldBe Right(CONST_LONG(1L))
  }

  property("indexOf with zero offset") {
    eval(""" "qweqwe".indexOf("qw", 0) """) shouldBe Right(CONST_LONG(0L))
  }

  property("indexOf with start offset") {
    eval(""" "qweqwe".indexOf("we", 2) """) shouldBe Right(CONST_LONG(4L))
  }

  property("indexOf from end of max sized string") {
    val n = 32766
    eval(s""" "${"a" * n}z".indexOf("z", $n) """) shouldBe Right(CONST_LONG(n))
  }

  property("indexOf (not present)") {
    eval(""" "qweqwe".indexOf("ww") """) shouldBe Right(unit)
  }

  property("indexOf from empty string") {
    eval(""" "".indexOf("!") """) shouldBe Right(unit)
  }

  property("indexOf from empty string with offset") {
    eval(""" "".indexOf("!", 1) """) shouldBe Right(unit)
  }

  property("indexOf from string with Long.MaxValue offset") {
    eval(s""" "abc".indexOf("c", ${Long.MaxValue}) """) shouldBe Right(unit)
  }

  property("indexOf from string with negative offset") {
    eval(""" "abc".indexOf("a", -1) """) shouldBe Right(unit)
  }

  property("indexOf from string with negative Long.MinValue offset") {
    eval(s""" "abc".indexOf("a", ${Long.MinValue}) """) shouldBe Right(unit)
  }

  property("indexOf empty string from non-empty string") {
    eval(""" "abc".indexOf("") """) shouldBe Right(CONST_LONG(0))
  }

  property("indexOf empty string from empty string") {
    eval(""" "".indexOf("") """) shouldBe Right(CONST_LONG(0))
  }

  property("lastIndexOf") {
    eval( """ "qweqwe".lastIndexOf("we") """) shouldBe Right(CONST_LONG(4))
  }

  property("lastIndexOf with zero offset") {
    eval(""" "qweqwe".lastIndexOf("qw", 0) """) shouldBe Right(CONST_LONG(0))
  }

  property("lastIndexOf with start offset") {
    eval(""" "qweqwe".lastIndexOf("we", 4) """) shouldBe Right(CONST_LONG(4L))
  }

  property("lastIndexOf from end of max sized string") {
    val n = 32766
    eval(s""" "${"a" * n}z".lastIndexOf("z", $n) """) shouldBe Right(CONST_LONG(n))
  }

  property("lastIndexOf (not present)") {
    eval(""" "qweqwe".lastIndexOf("ww") """) shouldBe Right(unit)
  }

  property("lastIndexOf from empty string") {
    eval(""" "".lastIndexOf("!") """) shouldBe Right(unit)
  }

  property("lastIndexOf from empty string with offset") {
    eval(""" "".lastIndexOf("!", 1) """) shouldBe Right(unit)
  }

  property("lastIndexOf from string with Int.MaxValue offset") {
    eval(s""" "abc".lastIndexOf("c", ${Int.MaxValue}) """) shouldBe Right(CONST_LONG(2))
  }

  property("lastIndexOf from string with Long.MaxValue offset") {
    eval(s""" "abc".lastIndexOf("c", ${Long.MaxValue}) """) shouldBe Right(CONST_LONG(2))
  }

  property("lastIndexOf from string with negative offset") {
    eval(""" "abc".lastIndexOf("a", -1) """) shouldBe Right(unit)
  }

  property("lastIndexOf from string with negative Long.MinValue offset") {
    eval(s""" "abc".lastIndexOf("a", ${Long.MinValue}) """) shouldBe Right(unit)
  }

  property("lastIndexOf empty string from non-empty string") {
    val str = "abcde"
    eval(s""" "$str".lastIndexOf("") """) shouldBe Right(CONST_LONG(str.length))
  }

  property("lastIndexOf empty string from empty string") {
    eval(""" "".lastIndexOf("") """) shouldBe Right(CONST_LONG(0))
  }
}
