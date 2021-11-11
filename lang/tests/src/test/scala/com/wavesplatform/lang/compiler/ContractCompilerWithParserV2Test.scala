package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.{CompilationError, ContractCompiler}
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.test.PropSpec

class ContractCompilerWithParserV2Test extends PropSpec {

  def compile(script: String, lastInsertedCharPos: Int): Either[String, (Option[DApp], Expressions.DAPP, Iterable[CompilationError])] =
    for {
      directives <- DirectiveParser(script)
      ds         <- Directive.extractDirectives(directives)
      ctx = lazyContexts(ds)().compilerContext
      compResult <- ContractCompiler.compileWithParseResult(script, ctx, ds.stdLibVersion, Some(lastInsertedCharPos), saveExprContext = false)
    } yield compResult

  property("no errors") {
    val script = """
                   |{-# STDLIB_VERSION 5 #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |{-# CONTENT_TYPE DAPP #-}
                   |
                   |@Callable(inv)
                   |func default() = {
                   |  [ IntegerEntry("x", inv.caller.wavesBalance().available) ]
                   |}
                   |
                 """.stripMargin

    compile(script, 0).map(_._3) shouldBe Right(Nil)
  }

  property("absent field after dot") {
    val script = """
                   |{-# STDLIB_VERSION 5 #-}
                   |{-# CONTENT_TYPE DAPP #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   |@Callable(i)
                   |func call() = {
                   |  let asset = Issue("Asset", "", 1, 0, true, unit, 0)
                   |  let assetId = asset.calculateAssetId()
                   |  [
                   |    ScriptTransfer(i.)
                   |  ]
                   |}
                   |
                   |@Verifier(tx)
                   |func verify() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
                   |
                 """.stripMargin

    compile(script, script.indexOf("i.") + 1)
      .map(_._3.map(err => script.substring(err.start, err.end))) shouldBe Right(List("."))
  }

  property("absent parenthesis") {
    val script = """
                   |{-# STDLIB_VERSION 5 #-}
                   |{-# CONTENT_TYPE DAPP #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   |func test() = {
                   |  match getInteger(this, "key" {
                   |    case a: Int => a
                   |    case _      => throw()
                   |  }
                   |}
                   |
                 """.stripMargin

    compile(script, script.indexOf(""""key"""") + 4)
      .map(_._3.map(err => script.substring(err.start, err.end))) shouldBe Right(List("""(this, "key""""))
  }
}
