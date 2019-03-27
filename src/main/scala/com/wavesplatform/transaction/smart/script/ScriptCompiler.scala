package com.wavesplatform.transaction.smart.script

import com.wavesplatform.lang.ContentType.ContentType
import com.wavesplatform.lang.StdLibVersion.StdLibVersion
import com.wavesplatform.lang.directives.DirectiveParser
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler}
import com.wavesplatform.lang.{ContentType, ScriptType}
import com.wavesplatform.transaction.smart.script.ContractScript._
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.utils._

object ScriptCompiler extends ScorexLogging {

  @Deprecated
  def apply(scriptText: String, isAssetScript: Boolean): Either[String, (Script, Long)] = {
    val directives = DirectiveParser(scriptText)
    for {
      ver    <- extractStdLibVersion(directives)
      tpe    <- extractContentType(directives)
      _      <- DirectiveSet(ver, if (isAssetScript) ScriptType.Asset else ScriptType.Account, tpe)
      script <- tryCompile(scriptText, tpe, ver, isAssetScript)
    } yield (script, script.complexity)
  }

  def compile(scriptText: String): Either[String, (Script, Long)] = {
    for {
      scriptType <- extractScriptType(DirectiveParser(scriptText))
      result     <- apply(scriptText, scriptType == ScriptType.Asset)
    } yield result
  }

  private def tryCompile(src: String, cType: ContentType, version: StdLibVersion, isAssetScript: Boolean): Either[String, Script] = {
    val ctx = compilerContext(version, cType, isAssetScript)
    try {
      cType match {
        case ContentType.Expression => ExpressionCompiler.compile(src, ctx).flatMap(expr => ExprScript.apply(version, expr))
        case ContentType.DApp       => ContractCompiler.compile(src, ctx).flatMap(expr => ContractScript.apply(version, expr))
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
    case s: ExprScript         => ScriptEstimator(varNames(version, ContentType.Expression), functionCosts(version), s.expr)
    case s: ContractScriptImpl => ContractScript.estimateComplexity(version, s.expr).map(_._2)
    case _                     => ???
  }

}
