package com.wavesplatform.lang.parser.error

import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Expression, V6}
import com.wavesplatform.lang.utils
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler, TestCompiler}

class CommonParseErrorTest extends ParseErrorTest {
  property("empty script as expression") {
    TestCompiler(V6).compileExpressionE("") shouldBe Left("Parse error: expected result expression in 0-0")
  }

  property("empty script with comment as expression") {
    TestCompiler(V6).compileExpressionE("#abcd") shouldBe Left("Parse error: expected result expression in 5-5")
  }

  property("illegal line break inside comment") {
    assert(
      """
        | # comment
        | # comment
        | break
        | # comment
        | # comment
        | func(a: Int) = a
      """.stripMargin,
      """Parse error: illegal expression""",
      24,
      29,
      "break",
      onlyDApp = true
    )
  }

  property("cyrillic charset for definition") {
    assert(
      """
        | func кириллица() = []
      """.stripMargin,
      """Parse error: expected only latin charset for definitions""",
      7,
      18,
      "кириллица()"
    )
  }

  property("chinese charset for definition") {
    assert(
      """
        | let 煊镕不 = []
      """.stripMargin,
      """Parse error: expected only latin charset for definitions""",
      6,
      9,
      "煊镕不"
    )
  }

  property("alternative compile methods contains same message and error indexes") {
    ContractCompiler
      .compileWithParseResult(
        """
          | let 煊镕不 = []
        """.stripMargin,
        utils.compilerContext(DirectiveSet.contractDirectiveSet),
        V6
      ) shouldBe Left(("Parse error: expected only latin charset for definitions in 6-9", 6, 9))

    ExpressionCompiler
      .compileWithParseResult(
        """
          | let 煊镕不 = []
          | true
        """.stripMargin,
        utils.compilerContext(V6, Expression, false)
      ) shouldBe Left(("Parse error: expected only latin charset for definitions in 6-9", 6, 9))
  }
}
