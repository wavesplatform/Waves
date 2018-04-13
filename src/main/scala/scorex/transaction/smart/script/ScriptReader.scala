package scorex.transaction.smart.script

import com.wavesplatform.crypto
import com.wavesplatform.lang.ScriptVersion
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.Terms.Typed
import scodec.Attempt.{Failure, Successful}
import scodec.DecodeResult
import scorex.transaction.ValidationError.ScriptParseError

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum         = bytes.takeRight(checksumLength)
    val computedCheckSum = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val version          = bytes.head
    val scriptBytes      = bytes.drop(1).dropRight(checksumLength)

    for {
      _ <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      sv <- ScriptVersion
        .fromInt(version)
        .fold[Either[ScriptParseError, ScriptVersion]](Left(ScriptParseError(s"Invalid version: $version")))(v => Right(v))
      expr <- readExpr(sv, scriptBytes)
    } yield Script(sv)(expr)
  }

  private def readExpr[V <: ScriptVersion](v: V, bytes: Array[Byte]): Either[ScriptParseError, v.ExprT] = {
    v match {
      case V1 =>
        Serde.codec.decode(scodec.bits.BitVector(bytes)) match {
          case Successful(value: DecodeResult[Typed.EXPR]) => Right(value.value.asInstanceOf[v.ExprT])
          case Failure(cause)                              => Left(ScriptParseError(cause.toString))
        }
    }
  }

}
