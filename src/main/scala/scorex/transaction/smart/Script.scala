package scorex.transaction.smart

import com.wavesplatform.lang.Serde
import com.wavesplatform.lang.Terms.Typed
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scodec.Attempt.{Failure, Successful}
import scodec.DecodeResult
import scorex.transaction.ValidationError.ScriptParseError

case class Script(script: Typed.EXPR) {
  val bytes: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(Serde.codec.encode(script).require.toByteArray))
  val version = 1
  val text: String = script.toString

  override def toString: String = s"Script(base58=${bytes()}, $text"
}

object Script {
  def fromBytes(arr: Array[Byte]): Either[ScriptParseError, Script] = Serde.codec.decode(scodec.bits.BitVector(arr)) match {
    case Successful(value: DecodeResult[Typed.EXPR]) => Right(Script(value.value))
    case Failure(cause) => Left(ScriptParseError(cause.toString))
  }

}
