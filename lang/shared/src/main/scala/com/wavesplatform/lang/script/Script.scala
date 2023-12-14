package com.wavesplatform.lang.script

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.ValidationError.ScriptParseError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.{DApp as DAppType, *}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.compiler.Decompiler
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import monix.eval.Coeval

trait Script {
  type Expr

  val stdLibVersion: StdLibVersion

  val expr: Expr

  val bytes: Coeval[ByteStr]

  val containsBlockV2: Coeval[Boolean]

  val containsArray: Boolean

  val isFreeCall: Boolean

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Script => stdLibVersion == that.stdLibVersion && expr == that.expr
    case _            => false
  }

  override def hashCode(): Int = stdLibVersion.id * 31 + bytes().hashCode()
}

object Script {

  case class ComplexityInfo(verifierComplexity: Long, callableComplexities: Map[String, Long], maxComplexity: Long)

  val checksumLength = 4

  def fromBase64String(str: String): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base64.tryDecode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base64: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes)
    } yield script

  type DirectiveMeta = List[(String, Any)]

  def decompile(s: Script): (String, DirectiveMeta) = {
    val cType: ContentType = s match {
      case _: ExprScript => Expression
      case _             => DAppType
    }
    val ctx = getDecompilerContext(s.stdLibVersion, cType)
    val (scriptText, directives) = (s: @unchecked) match {
      case e: ExprScript                   => (Decompiler(e.expr, ctx), List(s.stdLibVersion, Expression))
      case ContractScriptImpl(_, contract) => (Decompiler(contract, ctx, s.stdLibVersion), List(s.stdLibVersion, Account, DAppType))
    }
    val directivesText = directives
      .map(_.unparsed)
      .mkString(start = "", sep = "\n", end = "\n")

    val meta = directives.map(d => (d.key.text, d.value))
    (directivesText + scriptText, meta)
  }

  def complexityInfo(
      script: Script,
      estimator: ScriptEstimator,
      fixEstimateOfVerifier: Boolean,
      useContractVerifierLimit: Boolean,
      withCombinedContext: Boolean = false
  ): Either[String, ComplexityInfo] =
    (script: @unchecked) match {
      case script: ExprScript =>
        ExprScript
          .estimate(script.expr, script.stdLibVersion, script.isFreeCall, estimator, useContractVerifierLimit, withCombinedContext)
          .map { complexity =>
            val verifierComplexity = if (script.isFreeCall) 0 else complexity
            ComplexityInfo(verifierComplexity, Map(), complexity)
          }
      case ContractScriptImpl(version, contract @ DApp(_, _, _, verifierFuncOpt)) =>
        for {
          (maxComplexity, callableComplexities) <- ContractScript.estimateComplexity(
            version,
            contract,
            estimator,
            fixEstimateOfVerifier,
            useContractVerifierLimit
          )
          complexityInfo = verifierFuncOpt.fold(
            ComplexityInfo(0L, callableComplexities, maxComplexity)
          )(
            v => ComplexityInfo(callableComplexities(v.u.name), callableComplexities - v.u.name, maxComplexity)
          )
        } yield complexityInfo
    }

  def estimate(script: Script, estimator: ScriptEstimator, fixEstimateOfVerifier: Boolean, useContractVerifierLimit: Boolean): Either[String, Long] =
    complexityInfo(script, estimator, fixEstimateOfVerifier, useContractVerifierLimit)
      .map(_.maxComplexity)

  def verifierComplexity(
      script: Script,
      estimator: ScriptEstimator,
      fixEstimateOfVerifier: Boolean,
      useContractVerifierLimit: Boolean
  ): Either[String, Long] =
    complexityInfo(script, estimator, fixEstimateOfVerifier, useContractVerifierLimit)
      .map(_.verifierComplexity)
}
