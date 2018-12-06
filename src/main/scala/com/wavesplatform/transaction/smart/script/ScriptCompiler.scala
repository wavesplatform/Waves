package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.lang.Version
import com.wavesplatform.lang.Version.{V1, _}
import com.wavesplatform.lang.directives.{Directive, DirectiveKey, DirectiveParser}
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompilerV1}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.transaction.smart.script.v1.ScriptV1.ScriptV1Impl
import com.wavesplatform.transaction.smart.script.v1.{ScriptV1, ScriptV2}
import com.wavesplatform.utils._

import scala.util.{Failure, Success, Try}

object ScriptCompiler extends ScorexLogging {

  def contract(scriptText: String): Either[String, Script] = {
    val ctx = compilerContext(V3, isAssetScript = false)
    ContractCompiler(ctx, Parser.parseContract(scriptText).get.value)
      .map(s => ScriptV2(V3, s))
  }

  def apply(scriptText: String, isAssetScript: Boolean): Either[String, (Script, Long)] = {
    val directives = DirectiveParser(scriptText)

    val scriptWithoutDirectives =
      scriptText.lines
        .filter(str => !str.contains("{-#"))
        .mkString("\n")

    for {
      ver        <- extractVersion(directives)
      expr       <- tryCompile(scriptWithoutDirectives, ver, isAssetScript, directives)
      script     <- ScriptV1.apply(ver, expr)
      complexity <- ScriptEstimator(varNames(ver), functionCosts(ver), expr)
    } yield (script, complexity)
  }

  def tryCompile(src: String, version: Version, isAssetScript: Boolean, directives: List[Directive]): Either[String, EXPR] = {
    val compiler = new ExpressionCompilerV1(compilerContext(version, isAssetScript))
    try {
      compiler.compile(src, directives)
    } catch {
      case ex: Throwable =>
        log.error("Error compiling script", ex)
        log.error(src)
        val msg = Option(ex.getMessage).getOrElse("Parsing failed: Unknown error")
        Left(msg)
    }
  }

  def estimate(script: Script, version: Version): Either[String, Long] = script match {
    case s: ScriptV1Impl => ScriptEstimator(varNames(version), functionCosts(version), s.expr)
    case s: ScriptV2     => Right(1)
    case _               => ???
  }

  private def extractVersion(directives: List[Directive]): Either[String, Version] = {
    directives
      .find(_.key == DirectiveKey.LANGUAGE_VERSION)
      .map(d =>
        Try(d.value.toInt) match {
          case Success(v) =>
            val ver = Version(v)
            Either
              .cond(
                SupportedVersions(ver),
                ver,
                "Unsupported language version"
              )
          case Failure(ex) =>
            Left("Can't parse language version")
      })
      .getOrElse(V1.asRight)
  }

}
