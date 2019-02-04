package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.lang.ScriptType.ScriptType
import com.wavesplatform.lang.StdLibVersion.StdLibVersion
import com.wavesplatform.lang.directives.{Directive, DirectiveKey, DirectiveParser}
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.{ScriptType, StdLibVersion}
import com.wavesplatform.transaction.smart.script.ContractScript._
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.smart.script.v1.ExprScript.ExprScriprImpl
import com.wavesplatform.utils._

import scala.util.{Failure, Success, Try}

object ScriptCompiler extends ScorexLogging {

  def contract(scriptText: String): Either[String, Script] = {
    val ctx = compilerContext(StdLibVersion.V3, isAssetScript = false)
    ContractCompiler(ctx, Parser.parseContract(scriptText).get.value)
      .flatMap(s => ContractScript(StdLibVersion.V3, s))
  }

  def apply(scriptText: String, isAssetScript: Boolean): Either[String, (Script, Long)] = {
    val directives = DirectiveParser(scriptText)

    val scriptWithoutDirectives =
      scriptText.linesIterator
        .filter(str => !str.contains("{-#"))
        .mkString("\n")

    for {
      ver    <- extractStdLibVersion(directives)
      tpe    <- extractScriptType(directives)
      script <- tryCompile(scriptWithoutDirectives, tpe, ver, isAssetScript)
    } yield (script, script.complexity)
  }

  def tryCompile(src: String, tpe: ScriptType, version: StdLibVersion, isAssetScript: Boolean): Either[String, Script] = {
    val ctx = compilerContext(version, isAssetScript)
    try {
      tpe match {
        case ScriptType.Expression => ExpressionCompiler.compile(src, ctx).flatMap(expr => ExprScript.apply(version, expr))
        case ScriptType.Contract   => ContractCompiler.compile(src, ctx).flatMap(expr => ContractScript.apply(version, expr))
      }
    } catch {
      case ex: Throwable =>
        log.error("Error compiling script", ex)
        log.error(src)
        val msg = Option(ex.getMessage).getOrElse("Parsing failed: Unknown error")
        Left(msg)
    }
  }

  def estimate(script: Script, version: StdLibVersion): Either[String, Long] = script match {
    case s: ExprScriprImpl     => ScriptEstimator(varNames(version), functionCosts(version), s.expr)
    case s: ContractScriptImpl => ContractScript.estimateComplexity(version, s.expr).map(_._2)
    case _                     => ???
  }

  private def extractStdLibVersion(directives: List[Directive]): Either[String, StdLibVersion] = {
    directives
      .find(_.key == DirectiveKey.STDLIB_VERSION)
      .map(d =>
        Try(d.value.toInt) match {
          case Success(v) =>
            val ver = StdLibVersion(v)
            Either
              .cond(
                StdLibVersion.SupportedVersions(ver),
                ver,
                "Unsupported language version"
              )
          case Failure(ex) =>
            Left("Can't parse language version")
      })
      .getOrElse(StdLibVersion.V2.asRight)
  }

  private def extractScriptType(directives: List[Directive]): Either[String, ScriptType] = {
    directives
      .find(_.key == DirectiveKey.SCRIPT_TYPE)
      .map(d =>
        Try(d.value) match {
          case Success(v) =>
            val ver = ScriptType.parseString(v)
            Either
              .cond(
                ScriptType.SupportedVersions(ver),
                ver,
                "Unsupported script type"
              )
          case Failure(ex) =>
            Left("Can't parse script type")
      })
      .getOrElse(ScriptType.Expression.asRight)
  }

}
