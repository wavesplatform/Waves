package com.wavesplatform.lang.v1

import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

class FoldTest extends EvaluatorSpec {
  implicit val startVersion: StdLibVersion = V3

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

    eval(script) shouldBe Left("List size exceeds 4")
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
    eval(script) shouldBe Left(s"Compilation failed: [List size limit in FOLD is too big, 1001 must be less or equal 1000 in $index-${index + 4}]")
  }

  property("Maximum limit") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [${"1," * 999}1]
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

  property("first argument should be list") {
    eval(
      s"""
         | func sum(a:Int, b:Int) = a + b
         | FOLD<5>(1, 9, sum)
         |
      """.stripMargin
    ) shouldBe Left("Compilation failed: FOLD first argument should be List[T], but Int found in 34-52")
  }

  property("suitable function is not found") {
    eval(
      s"""
         | func sum(a:Int, b:String) = a
         | FOLD<5>([1], 0, sum)
         |
      """.stripMargin
    ) shouldBe Left("Compilation failed: Can't find suitable function sum(a: Int, b: Int) for FOLD in 33-53")
    eval(
      s"""
         | func sum(a:String, b:Int) = a
         | FOLD<5>([1], 0, sum)
         |
      """.stripMargin
    ) shouldBe Left("Compilation failed: Can't find suitable function sum(a: Int, b: Int) for FOLD in 33-53")
    eval(
      s"""
         | func sum(a:String, b:Int) = a
         | FOLD<5>([], 0, sum)
         |
      """.stripMargin
    ) shouldBe Left("Compilation failed: Can't find suitable function sum(a: Int, b: Any) for FOLD in 33-52")
  }

  private val compiler: TestCompiler = TestCompiler(V5)
  property("unique names are used for different folds") {
    val script = compiler.compileExpression("""{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |func f1(acc: String, v: String) = {
        |    acc + v
        |}
        |
        |let src = ["a", "b", "c"]
        |
        |FOLD<3>(src, "F1", f1) != FOLD<3>(src, "F2", f1)
        |""".stripMargin).asInstanceOf[ExprScript].expr
    ScriptEstimatorV3(true).apply(Set.empty, functionCosts(V5), script).isRight shouldBe true
  }

  property("unique names are used for folds inside foldFunc") {
    val script = compiler.compileExpression("""{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |func f1(acc: String, v: String) = {
        |    acc + v
        |}
        |
        |func f2(acc: String, v: List[String]) = {
        |    acc + FOLD<3>(v, "STEP", f1)
        |}
        |
        |let src = [
        |    ["01", "v1_1", "v1_2"],
        |    ["02", "v2_1", "v2_2"],
        |    ["03", "v3_1", "v3_2"]
        |]
        |
        |FOLD<3>(src, "", f2) == "STEP01v1_1v1_2STEP02v2_1v2_2STEP03v3_1v3_2"
        |""".stripMargin).asInstanceOf[ExprScript].expr
    ScriptEstimatorV3(true).apply(Set.empty, functionCosts(V5), script).isRight shouldBe true
  }

  property("unique names are used for native fold") {
    val script = compiler.compileExpression("""{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |func f1(acc: List[String], v: Int) = cons(toString(v), acc)
        |
        |func add(acc: String, v: String) = acc + v
        |
        |FOLD<3>(FOLD<3>([1, 2, 3], [], f1), "F1", add) == "F1321"
        |""".stripMargin).asInstanceOf[ExprScript].expr
    ScriptEstimatorV3(true).apply(Set.empty, functionCosts(V5), script).isRight shouldBe true
  }

  property("multiple scopes") {
    val script = compiler.compileExpression("""{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |let foo = [1, 2, 3]
        |func add(a: Int, b: Int) = a + b
        |
        |func f(x: Int) = {
        |  let a = FOLD<3>(foo, 0, add)
        |  FOLD<3>(foo, a, add)
        |}
        |
        |f(10) > 0
        |""".stripMargin).asInstanceOf[ExprScript].expr
    ScriptEstimatorV3(true).apply(Set.empty, functionCosts(V5), script).isRight shouldBe true
  }
}
