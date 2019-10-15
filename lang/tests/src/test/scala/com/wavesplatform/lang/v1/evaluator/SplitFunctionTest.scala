package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.kernel.Monoid
import com.wavesplatform.lang.Common.{AorBorC, NoShrink, addCtx, sampleTypes}
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CaseObj, EVALUATED}
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SplitFunctionTest
  extends PropSpec
    with  ScalaCheckPropertyChecks
    with  ScriptGen
    with  Matchers
    with  NoShrink {

  private val evaluator = new EvaluatorV1[Id, NoContext]()

  private def eval[T <: EVALUATED](code: String, pointInstance: Option[CaseObj] = None, pointType: FINAL = AorBorC): Either[String, T] = {
    val untyped                                                = Parser.parseExpr(code).get.value
    val lazyVal                                                = ContextfulVal.pure[NoContext](pointInstance.orNull)
    val stringToTuple = Map(("p", (pointType, lazyVal)))
    val ctx: CTX[NoContext] =
      Monoid.combineAll(Seq(
        PureContext.build(Global, V3),
        CTX[NoContext](sampleTypes, stringToTuple, Array.empty),
        addCtx
      ))
    val typed = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => evaluator.apply[T](ctx.evaluationContext, v._1))
  }

  property("split string containing separators") {
    val script =
      s"""
         |   let strs = split("str1;str2;str3;str4", ";")
         |   strs.size() == 4  &&
         |   strs[0] == "str1" &&
         |   strs[1] == "str2" &&
         |   strs[2] == "str3" &&
         |   strs[3] == "str4"
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("split around empty string") {
    val script =
      s"""
         |   let strs = split("some", "")
         |   strs.size() == 4 &&
         |   strs[0] == "s"   &&
         |   strs[1] == "o"   &&
         |   strs[2] == "m"   &&
         |   strs[3] == "e"
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("splitted string containing empty strings") {
    val script =
      s"""
         |   let strs1 = split(";;some;", ";")
         |   let result1 = strs1.size() == 4  &&
         |                 strs1[0] == ""     &&
         |                 strs1[1] == ""     &&
         |                 strs1[2] == "some" &&
         |                 strs1[3] == ""
         |
         |   let strs2 = split(";Q;Qsome;Q", ";Q")
         |   let result2 = strs2.size() == 4  &&
         |                 strs2[0] == ""     &&
         |                 strs2[1] == ""     &&
         |                 strs2[2] == "some" &&
         |                 strs2[3] == ""
         |
         |   let strs3 = split("QQ;someQ;Q;;", "Q;")
         |   let result3 = strs3.size() == 4  &&
         |                 strs3[0] == "Q"    &&
         |                 strs3[1] == "some" &&
         |                 strs3[2] == ""     &&
         |                 strs3[3] == ";"
         |
         |   let strs4 = split("q;Q;someQ;Q;;", "Q;")
         |   let result4 = strs4.size() == 4  &&
         |                 strs4[0] == "q;"   &&
         |                 strs4[1] == "some" &&
         |                 strs4[2] == ""     &&
         |                 strs4[3] == ";"
         |
         |   result1 && result2 && result3 && result4
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("split around unexisting separator") {
    val script =
      s"""
         |   let str      = "a;b;c"
         |   let splitted = split(str, ",")
         |   splitted.size() == 1 &&
         |   splitted[0] == str
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("split empty string") {
    val script =
      s"""
         |   let strs1 = split("", ",")
         |   let strs2 = split("", ",,,")
         |   let strs3 = split("", "")
         |
         |   strs1.size() == 1 &&
         |   strs2.size() == 1 &&
         |   strs3.size() == 1 &&
         |   strs1[0] == ""    &&
         |   strs2[0] == ""    &&
         |   strs3[0] == ""
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("split separator around separator") {
    val script =
      s"""
         |   let strs1 = split(",", ",")
         |   let strs2 = split(",x,", ",x,")
         |
         |   strs1.size() == 2 &&
         |   strs1[0] == ""    &&
         |   strs1[1] == ""    &&
         |   strs2.size() == 2 &&
         |   strs2[0] == ""    &&
         |   strs2[1] == ""
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("split string containing only separators") {
    val sep = ";;;"
    val count = 10
    val script =
      s"""
         |  let strs = "${sep * count}".split("$sep")
         |  strs.size() == ${count + 1} &&
         |  ${(0 to count).map(i => s"""strs[$i] == "" """).mkString(" && ")}
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("split perceives regex as plain text") {
    val strContainingRegex = "aaa1bbb2ccc"
    val regex = "[12]+"
    strContainingRegex.split(regex) shouldBe Array("aaa", "bbb", "ccc")

    val script =
      s"""
         |   let regex = "$regex"
         |
         |   let strContainingRegex = "$strContainingRegex"
         |   let splitted1 = split(strContainingRegex, regex)
         |   let result1   = splitted1.size() == 1 &&
         |                   splitted1[0] == strContainingRegex
         |
         |   let strContainingRegexText = "aaa${regex}bbb"
         |   let splitted2 = split(strContainingRegexText, regex)
         |   let result2   = splitted2.size() == 2 &&
         |                   splitted2[0] == "aaa" &&
         |                   splitted2[1] == "bbb"
         |
         |   result1 && result2
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }
}
