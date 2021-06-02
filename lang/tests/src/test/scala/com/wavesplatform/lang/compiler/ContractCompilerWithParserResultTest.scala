package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.Imports
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.{CompilationError, ContractCompiler}
import com.wavesplatform.lang.v1.parser.Expressions
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContractCompilerWithParserResultTest extends PropSpec with PropertyChecks with Matchers with NoShrink {

  def compile(script: String, saveExprContext: Boolean = false): Either[String, (Option[DApp], Expressions.DAPP, Iterable[CompilationError])] = {

    val result = for {
      directives <- DirectiveParser(script)
      ds         <- Directive.extractDirectives(directives)
      ctx = lazyContexts(ds.copy(imports = Imports()))().compilerContext
      compResult <- ContractCompiler.compileWithParseResult(script, ctx, ds.stdLibVersion, saveExprContext)
    } yield compResult

    result
  }

  property("simple test 2") {
    val script = """
                   |{-# STDLIB_VERSION 3 #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |{-# CONTENT_TYPE DAPP #-}
                   |
                   |@Callable(inv)
                   |func default() = {
                   |  [ IntegerEntry("x", inv.payment.extract().amount) ]
                   |}
                   |
                   |""".stripMargin

    val result = compile(script)

    result shouldBe Symbol("right")
  }
}
