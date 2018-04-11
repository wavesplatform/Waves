package scorex.transaction.smart

import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.{ScriptExpr, v1}

object ScriptExprWriter {

  def toBytes(script: ScriptExpr): Array[Byte] = {
    script match {
      case V1(expr) => v1.Serde.codec.encode(expr).require.toByteArray
    }
  }
}
