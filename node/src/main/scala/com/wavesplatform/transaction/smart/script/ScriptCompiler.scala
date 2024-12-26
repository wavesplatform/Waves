package com.wavesplatform.transaction.smart.script

import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.{API, CompileResult}

object ScriptCompiler {
  @deprecated("use ScriptCompiler.compile instead", "1.0")
  def apply(
      scriptText: String,
      isAssetScript: Boolean,
      estimator: ScriptEstimator,
      fixEstimateOfVerifier: Boolean = true
  ): Either[String, (Script, Long)] = {
    val script = if (!isAssetScript || scriptText.contains("SCRIPT_TYPE")) scriptText else s"{-# SCRIPT_TYPE ASSET #-}\n$scriptText"
    compile(script, estimator, fixEstimateOfVerifier = fixEstimateOfVerifier)
  }

  def compile(
      scriptText: String,
      estimator: ScriptEstimator,
      libraries: Map[String, String] = Map(),
      defaultStdLib: => StdLibVersion = StdLibVersion.VersionDic.default,
      fixEstimateOfVerifier: Boolean = true
  ): Either[String, (Script, Long)] =
    API.compile(scriptText, estimator, libraries = libraries, defaultStdLib = defaultStdLib).map {
      case CompileResult.Expression(v, _, complexity, expr, _, isFreeCall) => (ExprScriptImpl(v, isFreeCall, expr), complexity)
      case CompileResult.Library(v, _, complexity, expr)                   => (ExprScriptImpl(v, isFreeCall = false, expr), complexity)
      case CompileResult.DApp(v, r, _, _)                                  => (ContractScriptImpl(v, r.dApp), r.verifierComplexity)
    }
}
