package com.wavesplatform.lang.parser.error

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V8}
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.compiler.Terms.{BLOCK, FUNC, REF, TRUE}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.parser.Parser
import org.scalatest.Assertion

class FuncDefParseErrorTest extends ParseErrorTest {
  property("missing opening brace of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f a: Int) = a
      """.stripMargin,
      """Parse error: expected "("""",
      29,
      31,
      "f "
    )
  }

  property("missing closing brace of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a: Int = a
      """.stripMargin,
      """Parse error: expected ")"""",
      36,
      38,
      "t "
    )
  }

  property("missing '=' of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a: Int) a
      """.stripMargin,
      """Parse error: expected "="""",
      37,
      39,
      ") "
    )
  }

  property("Java style type definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(Int a) = a
      """.stripMargin,
      """Parse error: expected ":"""",
      33,
      35,
      "t "
    )
  }

  property("missing arguments types") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a, b, c) = a
      """.stripMargin,
      """Parse error: expected ":"""",
      29,
      32,
      "f(a"
    )
  }

  property("showing first of multiple errors") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a Int, b: String, c) a
      """.stripMargin,
      """Parse error: expected ":"""",
      31,
      33,
      "a "
    )
  }

  property("missing 'func' keyword") {
    assert(
      """
        | let x = 1
        | let y = 1
        | f(a: Int) = a
      """.stripMargin,
      """Parse error: expected "func" keyword""",
      24,
      28,
      "f(a:"
    )
  }

  property("'func' keyword without definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func
      """.stripMargin,
      """Parse error: expected function name""",
      24,
      28,
      "func",
      endExpr = false
    )
  }

  property("missing function name") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func (a: Int) = a
      """.stripMargin,
      """Parse error: expected function name""",
      24,
      28,
      "func"
    )
  }

  property("missing function name without space") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func(a: Int) = a
      """.stripMargin,
      """Parse error: expected function name""",
      24,
      28,
      "func"
    )
  }

  property("missing function body") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a: Int) =
      """.stripMargin,
      """Parse error: expected function body""",
      39,
      47,
      "=\n      ",
      endExpr = false
    )
  }

  property("missing closing curly brace of function body") {
    assert(
      """
        | func f(a: Int) = {
        |   true
        |
      """.stripMargin,
      """Parse error: expected "}"""",
      27,
      36,
      "e\n\n      ",
      endExpr = false
    )
  }

  property("missing result expression") {
    assert(
      """
        | func f() = {
        |   let a = 1
        | }
      """.stripMargin,
      """Parse error: expected expression""",
      27,
      30,
      "\n }"
    )
  }

  property("forbid definition without space") {
    assert(
      """
        | funcf() = 1
      """.stripMargin,
      """Parse error: expected "func" keyword""",
      2,
      9,
      "funcf()",
      onlyDApp = true
    )
  }

  property("forbid definition without space for callable") {
    assert(
      """
        | @Callable(i)
        | funccall() = []
      """.stripMargin,
      """Parse error: expected "func" keyword""",
      16,
      26,
      "funccall()",
      onlyDApp = true
    )
  }

  property("function name started from digit") {
    assert(
      """
        | func 1call() = 1
      """.stripMargin,
      """Parse error: expected character or "_" at start of the definition""",
      7,
      14,
      "1call()"
    )
  }

  property("missing type after parameter definition with ':'") {
    assert(
      """
        | func call(a:)
      """.stripMargin,
      """Parse error: illegal expression""",
      7,
      14,
      "call(a:"
    )
  }

  property("function argument started from digit") {
    assert(
      """
        | func call(1a:Int)
      """.stripMargin,
      """Parse error: expected character or "_" at start of the definition""",
      12,
      19,
      "1a:Int)"
    )
  }

  property("illegal character in function name") {
    assert(
      """
        | func c@ll()
      """.stripMargin,
      """Parse error: expected character, digit or "_" for the definition""",
      8,
      13,
      "@ll()"
    )
  }

  StdLibVersion.VersionDic.all.filter(_ >= V3).foreach { version =>
    def script(keyword: String, isDApp: Boolean = true): String = {
      val exprSuffix = if (isDApp) "" else "true"
      s"""func $keyword(a: Int) = []
         |$exprSuffix
         |""".stripMargin
    }

    def assertError(keyword: String, v: StdLibVersion): Assertion =
      assert(
        script(keyword),
        s"Compilation failed: keywords are restricted: $keyword",
        5,
        keyword.length + 5,
        keyword,
        v
      )

    Parser.keywordsBeforeV8.foreach { keyword =>
      property(s"keyword $keyword as function name is restricted (V${version.id})") {
        assertError(keyword, version)
      }
    }

    Parser.additionalV8Keywords.foreach { keyword =>
      property(s"keyword $keyword as function name is restricted only for version >= V8 (V${version.id})") {
        if (version >= V8) {
          assertError(keyword, version)
        } else {
          TestCompiler(version).compile(script(keyword)).map(_.decs) shouldBe Right(List(FUNC(keyword, List("a"), REF("nil"))))
          TestCompiler(version).compileExpressionE(script(keyword, isDApp = false)) shouldBe Right(
            ExprScriptImpl(version, false, BLOCK(FUNC(keyword, List("a"), REF("nil")), TRUE))
          )
        }
      }
    }
  }
}
