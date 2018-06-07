package scorex.transaction.smart.script.v1

import com.wavesplatform.crypto
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.{FunctionHeader, ScriptEstimator, Serde}
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import scorex.transaction.smart.script.Script

object ScriptV1 {
  private val functionCosts: Map[FunctionHeader, Long] =
    EvaluationContext.functionCosts(com.wavesplatform.utils.dummyEvaluationContext.functions.values)

  private val checksumLength = 4
  private val maxComplexity  = 20 * functionCosts(FunctionHeader.Predef(SIGVERIFY))
  private val maxSizeInBytes = 8 * 1024

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= maxSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $maxSizeInBytes bytes")

  def apply(x: EXPR, checkSize: Boolean = true): Either[String, Script] =
    for {
      scriptComplexity <- ScriptEstimator(functionCosts, x)
      _                <- Either.cond(scriptComplexity <= maxComplexity, (), s"Script is too complex: $scriptComplexity > $maxComplexity")
      s = new ScriptV1(x)
      _ <- if (checkSize) validateBytes(s.bytes().arr) else Right(())
    } yield s

  private class ScriptV1(override val expr: EXPR) extends Script {
    override type V = V1.type
    override val version: V   = V1
    override val text: String = expr.toString
    override val bytes: Coeval[ByteStr] =
      Coeval.evalOnce {
        val s = Array(version.value.toByte) ++ Serde.codec.encode(expr).require.toByteArray
        ByteStr(s ++ crypto.secureHash(s).take(ScriptV1.checksumLength))
      }
  }
}
