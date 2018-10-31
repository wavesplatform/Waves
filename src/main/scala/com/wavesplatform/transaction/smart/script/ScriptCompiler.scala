package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.lang.ScriptVersion
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.{Directive, DirectiveParser, DirectiveKey}
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.utils._
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import scala.util.{Failure, Success, Try}

object ScriptCompiler extends ScorexLogging {

  def apply(scriptText: String): Either[String, (Script, Long)] = {
    val directives = DirectiveParser(scriptText)

    val scriptWithoutDirectives =
      scriptText.lines
        .filter(str => !str.contains("{-#"))
        .mkString("\n")

    for {
      ver        <- extractVersion(directives)
      expr       <- tryCompile(scriptWithoutDirectives, ScriptVersion.Versions.V2, directives)
      script     <- ScriptV1(ScriptVersion.Versions.V2, expr)
      complexity <- ScriptEstimator(varNames(ScriptVersion.Versions.V2), functionCosts(ScriptVersion.Versions.V2), expr)
    } yield (script, complexity)
  }

  def tryCompile(src: String, version: ScriptVersion, directives: List[Directive]): Either[String, EXPR] = {
    val compiler = new CompilerV1(compilerContext(version))
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

  def estimate(script: Script, version: ScriptVersion): Either[String, Long] = script match {
    case Script.Expr(expr) => ScriptEstimator(varNames(version), functionCosts(version), expr)
  }

  private def extractVersion(directives: List[Directive]): Either[String, ScriptVersion] = {
    directives
      .find(_.key == DirectiveKey.LANGUAGE_VERSION)
      .map(d =>
        Try(d.value.toInt) match {
          case Success(v) =>
            ScriptVersion
              .fromInt(v)
              .fold[Either[String, ScriptVersion]](Left("Unsupported language version"))(_.asRight)
          case Failure(ex) =>
            Left("Can't parse language version")
      })
      .getOrElse(V1.asRight)
  }

}
