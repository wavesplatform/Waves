package scorex.transaction.smart.script.v1

import com.wavesplatform.crypto
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.Terms.{BOOLEAN, Typed}
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import scorex.transaction.smart.script.Script

object ScriptV1 {
  private val checksumLength = 4

  def apply(x: Typed.EXPR): Either[String, Script] = {
    def create = new Script {
      override type V = V1.type
      override val expr: Typed.EXPR = x
      override val version: V       = V1
      override val text: String     = x.toString
      override val bytes: Coeval[ByteStr] =
        Coeval.evalOnce {
          val s = Array(version.value.toByte) ++ Serde.codec.encode(x).require.toByteArray
          ByteStr(s ++ crypto.secureHash(s).take(ScriptV1.checksumLength))
        }
    }

    Either.cond(x.tpe == BOOLEAN, create, "Script should return BOOLEAN")
  }
}
