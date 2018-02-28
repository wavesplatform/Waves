package scorex.transaction.smart

import com.wavesplatform.crypto
import com.wavesplatform.lang.Serde
import com.wavesplatform.lang.Terms.Typed
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scodec.Attempt.{Failure, Successful}
import scodec.DecodeResult
import scorex.transaction.ValidationError.ScriptParseError
import scorex.transaction.smart.Script.checksumLength

case class Script(script: Typed.EXPR) {
  val bytes: Coeval[ByteStr] = Coeval.evalOnce {
    val s = Serde.codec.encode(script).require.toByteArray
    ByteStr(s ++ crypto.secureHash(s).take(checksumLength))
  }
  val version = 1
  val text: String = script.toString

  override def toString: String = s"Script(base58=${bytes()}, $text"
}

object Script {
  val checksumLength = 4

  def fromBytes(scriptBytes: Array[Byte]): Either[ScriptParseError, Script] = {

    val checkSum = scriptBytes.takeRight(checksumLength)
    val computedCheckSum = crypto.secureHash(scriptBytes.dropRight(checksumLength)).take(checksumLength)

    Either.cond[ScriptParseError, Unit](checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      .flatMap(_ =>
        Serde.codec.decode(scodec.bits.BitVector(scriptBytes.dropRight(checksumLength))) match {
          case Successful(value: DecodeResult[Typed.EXPR]) => Right(Script(value.value))
          case Failure(cause) => Left(ScriptParseError(cause.toString))
        })
  }
}
