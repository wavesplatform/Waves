package scorex.transaction.smart

import scodec.Attempt.{Failure, Successful}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scodec.DecodeResult
import scorex.transaction.ValidationError.ScriptParseError
import scorex.transaction.smart.lang.Terms._

case class Script(script: Term) {
  val bytes: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(scorex.transaction.smart.lang.Serde.codec.encode(script).require.toByteArray))
  val version = 1
  val text: String = script.toString

  override def toString: String = s"Script($text, base58=${bytes()}"
}

object Script {
  def fromBytes(arr: Array[Byte]): Either[ScriptParseError, Script] = scorex.transaction.smart.lang.Serde.codec.decode(scodec.bits.BitVector(arr)) match {
    case Successful(value: DecodeResult[Term]) => Right(Script(value.value))
    case Failure(cause) => Left(ScriptParseError(cause.toString))
  }

  val sigVerify: Script = Script(SIG_VERIFY(TX_FIELD(BodyBytes), TX_FIELD(Proof_0), TX_FIELD(SenderPk)))

}
