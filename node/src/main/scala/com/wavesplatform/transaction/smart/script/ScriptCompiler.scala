package com.wavesplatform.transaction.smart.script

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
      estimator: ScriptEstimator,
      fixEstimateOfVerifier: Boolean = true
  ): Either[String, (Script, Long)] =
    for {
      directives   <- DirectiveParser(scriptText)
      directiveSet <- Directive.extractDirectives(directives, StdLibVersion.VersionDic.default)
      scriptType = ScriptType.isAssetScript(isAssetScript)
      result <- applyAndEstimate(scriptText, directiveSet.copy(scriptType = scriptType), estimator, Script.estimate, fixEstimateOfVerifier)
    } yield result

  def compile(
      scriptText: String,
      estimator: ScriptEstimator,
      libraries: Map[String, String] = Map(),
      defaultStdLib: => StdLibVersion = StdLibVersion.VersionDic.default,
      fixEstimateOfVerifier: Boolean = true
  ): Either[String, (Script, Long)] =
    compileAndEstimate(scriptText, estimator, libraries, Script.estimate, defaultStdLib, fixEstimateOfVerifier)

  def compileAndEstimateCallables(
      scriptText: String,
      estimator: ScriptEstimator,
      defaultStdLib: => StdLibVersion,
      fixEstimateOfVerifier: Boolean
  ): Either[String, (Script, Script.ComplexityInfo)] =
    compileAndEstimate(scriptText, estimator, Map(), Script.complexityInfo, defaultStdLib, fixEstimateOfVerifier)

  private def compileAndEstimate[C](
      scriptText: String,
      estimator: ScriptEstimator,
      libraries: Map[String, String],
      estimate: (Script, ScriptEstimator, Boolean, Boolean) => Either[String, C],
      defaultStdLib: => StdLibVersion,
      fixEstimateOfVerifier: Boolean
  ): Either[String, (Script, C)] =
    for {
      directives   <- DirectiveParser(scriptText)
      directiveSet <- Directive.extractDirectives(directives, defaultStdLib)
      linkedInput  <- ScriptPreprocessor(scriptText, libraries, directiveSet.imports)
      result       <- applyAndEstimate(linkedInput, directiveSet, estimator, estimate, fixEstimateOfVerifier)
    } yield result

  private def applyAndEstimate[C](
      scriptText: String,
      directiveSet: DirectiveSet,
      estimator: ScriptEstimator,
      estimate: (Script, ScriptEstimator, Boolean, Boolean) => Either[String, C],
      fixEstimateOfVerifier: Boolean
  ): Either[String, (Script, C)] =
    for {
      script     <- tryCompile(scriptText, directiveSet)
      complexity <- estimate(script, estimator, fixEstimateOfVerifier, directiveSet.scriptType != Asset)
    } yield (script, complexity)

  private def tryCompile(src: String, directiveSet: DirectiveSet): Either[String, Script] = {
    val ctx = compilerContext(directiveSet)
    try {
      directiveSet match {
        case DirectiveSet(v, Call, Expression, _) => ContractCompiler.compileFreeCall(src, ctx, v).flatMap(ExprScript(v, _, isFreeCall = true))
        case DirectiveSet(v, _, Expression, _)    => ExpressionCompiler.compileBoolean(src, ctx).flatMap(ExprScript(v, _))
        case DirectiveSet(v, _, DApp, _)          => ContractCompiler.compile(src, ctx, v).flatMap(ContractScript(v, _))
        case DirectiveSet(v, _, Library, _)       => ExpressionCompiler.compileDecls(src, ctx).flatMap(ExprScript(v, _))
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
