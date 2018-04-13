package scorex.transaction.smart.script.v1

import com.wavesplatform.crypto
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.Terms.Typed
import com.wavesplatform.lang.v1.Terms.Typed.EXPR
import com.wavesplatform.lang.v1.{CostCalculator, Serde}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scorex.transaction.smart.script.Script

final case class ScriptV1(expr: Typed.EXPR) extends Script {
  override type V = V1.type
  override val version: V = V1

  override val complexity: Coeval[Long] = Coeval.evalOnce(CostCalculator(expr))
  override val text: String             = expr.toString

  override val bytes: Coeval[ByteStr] =
    Coeval.evalOnce {
      val s = Array(version.value.toByte) ++ Serde.codec.encode(expr).require.toByteArray
      ByteStr(s ++ crypto.secureHash(s).take(ScriptV1.checksumLength))
    }
}

object ScriptV1 {
  private val checksumLength = 4

  object SV1 {
    def unapply(arg: Script): Option[EXPR] = {
      if (arg.version == V1) Some(arg.expr.asInstanceOf[EXPR])
      else None
    }
  }
}
