package scorex.transaction.smart

import cats.implicits._
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.ScriptExprV1
import com.wavesplatform.lang.{ScriptExpr, ScriptVersion, v1}
import scorex.transaction.ValidationError.ScriptParseError

object ScriptExprReader {
  def fromBytes(v: ScriptVersion, bytes: Array[Byte]): Either[ScriptParseError, ScriptExpr] = {
    v match {
      case V1 =>
        v1.Serde.codec
          .decode(scodec.bits.BitVector(bytes))
          .fold(
            cause => Left(ScriptParseError(cause.toString())),
            _.map(ScriptExprV1).value.asRight
          )
    }
  }

}
