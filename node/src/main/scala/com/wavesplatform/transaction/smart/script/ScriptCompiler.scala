package com.wavesplatform.transaction.smart.script

import com.wavesplatform.lang.directives.Directive.extractValue
import com.wavesplatform.lang.directives.DirectiveKey._
import com.wavesplatform.lang.directives._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script, ScriptPreprocessor}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.utils._

object ScriptCompiler extends ScorexLogging {

  @Deprecated
  def apply(
      scriptText: String,
      isAssetScript: Boolean,
      estimator: ScriptEstimator
  ): Either[String, (Script, Long)] =
    applyAndEstimate(scriptText, isAssetScript, estimator, Script.estimate, StdLibVersion.VersionDic.default)

  def compile(
      scriptText: String,
      estimator: ScriptEstimator,
      libraries: Map[String, String] = Map(),
      defaultStdLib: => StdLibVersion = StdLibVersion.VersionDic.default
  ): Either[String, (Script, Long)] =
    compileAndEstimate(scriptText, estimator, libraries, Script.estimate, defaultStdLib)

  def compileAndEstimateCallables(
      scriptText: String,
      estimator: ScriptEstimator,
      libraries: Map[String, String] = Map(),
      defaultStdLib: => StdLibVersion = StdLibVersion.VersionDic.default
  ): Either[String, (Script, Script.ComplexityInfo)] =
    compileAndEstimate(scriptText, estimator, libraries, Script.complexityInfo, defaultStdLib)

  def compileAndEstimate[C](
      scriptText: String,
      estimator: ScriptEstimator,
      libraries: Map[String, String] = Map(),
      estimate: (Script, ScriptEstimator, Boolean) => Either[String, C],
      defaultStdLib: => StdLibVersion = StdLibVersion.VersionDic.default
  ): Either[String, (Script, C)] =
    for {
      directives  <- DirectiveParser(scriptText)
      ds          <- Directive.extractDirectives(directives, defaultStdLib)
      linkedInput <- ScriptPreprocessor(scriptText, libraries, ds.imports)
      result      <- applyAndEstimate(linkedInput, ds.scriptType == Asset, estimator, estimate, defaultStdLib)
    } yield result

  private def applyAndEstimate[C](
      scriptText: String,
      isAssetScript: Boolean,
      estimator: ScriptEstimator,
      estimate: (Script, ScriptEstimator, Boolean) => Either[String, C],
      defaultStdLib: => StdLibVersion // = StdLibVersion.VersionDic.default
  ): Either[String, (Script, C)] =
    for {
      directives <- DirectiveParser(scriptText)
      contentType = extractValue(directives, CONTENT_TYPE)
      version     = extractValue(directives, STDLIB_VERSION)(Some(defaultStdLib))
      scriptType  = if (isAssetScript) Asset else Account
      _          <- DirectiveSet(version, scriptType, contentType)
      script     <- tryCompile(scriptText, contentType, version, isAssetScript)
      complexity <- estimate(script, estimator, !isAssetScript)
    } yield (script, complexity)

  private def tryCompile(src: String, cType: ContentType, version: StdLibVersion, isAssetScript: Boolean): Either[String, Script] = {
    val ctx = compilerContext(version, cType, isAssetScript)
    try {
      cType match {
        case Expression => ExpressionCompiler.compileBoolean(src, ctx).flatMap(expr => ExprScript.apply(version, expr))
        case DApp       => ContractCompiler.compile(src, ctx, version).flatMap(expr => ContractScript.apply(version, expr))
        case Library    => ExpressionCompiler.compileDecls(src, ctx).flatMap(ExprScript(version, _))
      }
    } catch {
      case ex: Throwable =>
        log.error("Error compiling script", ex)
        log.error(src)
        val msg = Option(ex.getMessage).getOrElse("Parsing failed: Unknown error")
        Left(msg)
    }
  }
}
