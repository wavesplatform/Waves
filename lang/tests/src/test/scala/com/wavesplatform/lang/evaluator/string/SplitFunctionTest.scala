package com.wavesplatform.lang.evaluator.string

import com.wavesplatform.lang.directives.values.{V3, V6}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ctx.BaseFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext

class SplitFunctionTest extends EvaluatorSpec {
  private def assertSuccess(script: String => String): Unit =
    for ((f, v) <- Seq((PureContext.splitStrFixed, V3), (PureContext.splitStr1C, V6), (PureContext.splitStr4C, V6))) {
      eval(script(f.name))(v) shouldBe Right(CONST_BOOLEAN(true))
    }

  property("split string containing separators") {
    assertSuccess(
      f => s"""
              |   let strs = $f("str1;str2;str3;str4", ";")
              |   strs.size() == 4  &&
              |   strs[0] == "str1" &&
              |   strs[1] == "str2" &&
              |   strs[2] == "str3" &&
              |   strs[3] == "str4"
            """.stripMargin
    )
  }

  property("split around empty string") {
    assertSuccess(
      f => s"""
              |   let strs = $f("some", "")
              |   strs.size() == 4 &&
              |   strs[0] == "s"   &&
              |   strs[1] == "o"   &&
              |   strs[2] == "m"   &&
              |   strs[3] == "e"
              |
            """.stripMargin
    )
  }

  property("splitted string containing empty strings") {
    assertSuccess(
      f => s"""
              |   let strs1 = $f(";;some;", ";")
              |   let result1 = strs1.size() == 4  &&
              |                 strs1[0] == ""     &&
              |                 strs1[1] == ""     &&
              |                 strs1[2] == "some" &&
              |                 strs1[3] == ""
              |
              |   let strs2 = $f(";Q;Qsome;Q", ";Q")
              |   let result2 = strs2.size() == 4  &&
              |                 strs2[0] == ""     &&
              |                 strs2[1] == ""     &&
              |                 strs2[2] == "some" &&
              |                 strs2[3] == ""
              |
              |   let strs3 = $f("QQ;someQ;Q;;", "Q;")
              |   let result3 = strs3.size() == 4  &&
              |                 strs3[0] == "Q"    &&
              |                 strs3[1] == "some" &&
              |                 strs3[2] == ""     &&
              |                 strs3[3] == ";"
              |
              |   let strs4 = $f("q;Q;someQ;Q;;", "Q;")
              |   let result4 = strs4.size() == 4  &&
              |                 strs4[0] == "q;"   &&
              |                 strs4[1] == "some" &&
              |                 strs4[2] == ""     &&
              |                 strs4[3] == ";"
              |
              |   result1 && result2 && result3 && result4
            """.stripMargin
    )
  }

  property("split around unexisting separator") {
    assertSuccess(
      f => s"""
           |   let str      = "a;b;c"
           |   let splitted = $f(str, ",")
           |   splitted.size() == 1 &&
           |   splitted[0] == str
           |
         """.stripMargin
    )
  }

  property("split empty string") {
    assertSuccess(
      f => s"""
           |   let strs1 = $f("", ",")
           |   let strs2 = $f("", ",,,")
           |   let strs3 = $f("", "")
           |
           |   strs1.size() == 1 &&
           |   strs2.size() == 1 &&
           |   strs3.size() == 1 &&
           |   strs1[0] == ""    &&
           |   strs2[0] == ""    &&
           |   strs3[0] == ""
           |
         """.stripMargin
    )
  }

  property("split separator around separator") {
    assertSuccess(
      f => s"""
           |   let strs1 = $f(",", ",")
           |   let strs2 = $f(",x,", ",x,")
           |
           |   strs1.size() == 2 &&
           |   strs1[0] == ""    &&
           |   strs1[1] == ""    &&
           |   strs2.size() == 2 &&
           |   strs2[0] == ""    &&
           |   strs2[1] == ""
           |
         """.stripMargin
    )
  }

  property("split string containing only separators") {
    val sep   = ";;;"
    val count = 10
    assertSuccess(
      f => s"""
           |  let strs = $f("${sep * count}", "$sep")
           |  strs.size() == ${count + 1} &&
           |  ${(0 to count).map(i => s"""strs[$i] == "" """).mkString(" && ")}
         """.stripMargin
    )
  }

  property("split perceives regex as plain text") {
    val strContainingRegex = "aaa1bbb2ccc"
    val regex              = "[12]+"
    strContainingRegex.split(regex) shouldBe Array("aaa", "bbb", "ccc")
    assertSuccess(
      f => s"""
           |   let regex = "$regex"
           |
           |   let strContainingRegex = "$strContainingRegex"
           |   let splitted1 = $f(strContainingRegex, regex)
           |   let result1   = splitted1.size() == 1 &&
           |                   splitted1[0] == strContainingRegex
           |  
           |   let strContainingRegexText = "aaa${regex}bbb"
           |   let splitted2 = $f(strContainingRegexText, regex)
           |   let result2   = splitted2.size() == 2 &&
           |                   splitted2[0] == "aaa" &&
           |                   splitted2[1] == "bbb"
           |
           |   result1 && result2
         """.stripMargin
    )
  }

  property("function family input limits") {
    val count = 100
    val elem  = "a" * count
    def str(f: BaseFunction[NoContext], limit: Int, c: Int) = s""" ${f.name}("${s"$elem," * (limit / c)}$elem", ",") """
    for ((f, limit) <- List((PureContext.splitStr1C, 500), (PureContext.splitStr4C, 6000))) {
      val actualSize = limit / (count + 1) * (count + 1) + count
      eval(str(f, limit, count + 1))(V6) shouldBe Left(s"Input string size = $actualSize bytes exceeds limit = $limit for ${f.name}")
      eval(str(f, limit, count * 2))(V6) shouldBe a[Right[_, _]]
    }
  }

  property("function family output limits") {
    val elem = "a"
    def str(f: BaseFunction[NoContext], n: Int) = s""" ${f.name}("${s"$elem," * (n - 1)}$elem", ",") """
    for ((f, limit) <- List((PureContext.splitStr1C, 20), (PureContext.splitStr4C, 100))) {
      eval(str(f, limit + 1))(V6) shouldBe Left(s"Output list size = ${limit + 1} exceeds limit = $limit for ${f.name}")
      eval(str(f, limit))(V6) shouldBe a[Right[_, _]]
    }
  }
}
