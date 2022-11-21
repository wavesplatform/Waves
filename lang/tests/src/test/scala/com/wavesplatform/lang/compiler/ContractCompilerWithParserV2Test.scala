package com.wavesplatform.lang.compiler

import cats.implicits.toBifunctorOps
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import com.wavesplatform.lang.utils
import com.wavesplatform.lang.v1.compiler.{CompilationError, ContractCompiler}
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.test.PropSpec

class ContractCompilerWithParserV2Test extends PropSpec {

  def compile(script: String, saveExprContext: Boolean = false): Either[String, (Option[DApp], Expressions.DAPP, Iterable[CompilationError])] = {

    val result = for {
      directives <- DirectiveParser(script)
      ds         <- Directive.extractDirectives(directives)
      ctx = utils.compilerContext(ds)
      compResult <- ContractCompiler.compileWithParseResult(script, ctx, ds.stdLibVersion, saveExprContext).leftMap(_._1)
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
