package scorex.transaction.smart

import com.wavesplatform.lang.Serde
import com.wavesplatform.lang.Terms._
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scodec.Attempt.{Failure, Successful}
import scodec.DecodeResult
import scorex.transaction.ValidationError.ScriptParseError

case class Script(script: Expr) {
  val bytes: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(Serde.codec.encode(script).require.toByteArray))
  val version = 1
  val text: String = script.toString

  override def toString: String = s"Script($text, base58=${bytes()}"
}

object Script {
  def fromBytes(arr: Array[Byte]): Either[ScriptParseError, Script] = Serde.codec.decode(scodec.bits.BitVector(arr)) match {
    case Successful(value: DecodeResult[Expr]) => Right(Script(value.value))
    case Failure(cause) => Left(ScriptParseError(cause.toString))
  }

  val sigVerify: Script = null// Script(SIG_VERIFY(TX_FIELD(BodyBytes), TX_FIELD(Proof(0)), TX_FIELD(SenderPk)))

}
