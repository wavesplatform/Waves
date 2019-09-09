package com.wavesplatform.lang.script

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.ValidationError.ScriptParseError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.values.{DApp => DAppType}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.Decompiler
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import monix.eval.Coeval

trait Script {
  type Expr

  val stdLibVersion: StdLibVersion

  val expr: Expr

  val bytes: Coeval[ByteStr]

  val containsBlockV2: Coeval[Boolean]

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Script => stdLibVersion == that.stdLibVersion && expr == that.expr
    case _            => false
  }

  override def hashCode(): Int = stdLibVersion.id * 31 + expr.hashCode()
}

object Script {

  val checksumLength = 4

  def fromBase64String(str: String): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base64.tryDecode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base64: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes)
    } yield script

  type DirectiveMeta = List[(String, Any)]

  def decompile(s: Script): (String, DirectiveMeta) = {
    val ctx = defaultDecompilerContext
    val (scriptText, directives) = s match {
      case e: ExprScript                   => (Decompiler(e.expr, ctx), List(s.stdLibVersion, Expression))
      case ContractScriptImpl(_, contract) => (Decompiler(contract, ctx), List(s.stdLibVersion, Account, DAppType))
    }
    val directivesText = directives
      .map(_.unparsed)
      .mkString(start = "", sep = "\n", end = "\n")

    val meta = directives.map(d => (d.key.text, d.value))
    (directivesText + scriptText, meta)
  }

  def estimate(s: Script, estimator: ScriptEstimator): Either[String, Long] =
    complexityInfo(s, estimator).map(_._1)

  def complexityInfo(s: Script, estimator: ScriptEstimator): Either[String, (Long, Map[String, Long])] =
    s match {
      case script: ExprScript =>
        ExprScript.estimate(script.expr, script.stdLibVersion, estimator).map((_, Map()))
      case ContractScriptImpl(version, contract) =>
        ContractScript.estimateComplexity(version, contract, estimator)
    }

  def verifierComplexity(script: Script, estimator: ScriptEstimator): Either[String, Long] =
    Script.complexityInfo(script, estimator)
      .map(calcVerifierComplexity(script, _))

  private def calcVerifierComplexity(
    script:     Script,
    complexity: (Long, Map[String, Long])
  ): Long = {
    val (totalComplexity, cm) = complexity
    script match {
      case ContractScriptImpl(_, DApp(_, _, _, Some(vf))) if cm.contains(vf.u.name) => cm(vf.u.name)
      case _ => totalComplexity
    }
  }
}
