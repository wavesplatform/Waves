package scorex.transaction.smart

import com.wavesplatform.crypto
import com.wavesplatform.lang.{ScriptExpr, ScriptVersion}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.ScriptParseError
import scorex.transaction.smart.Script._

case class Script(script: ScriptExpr) {

  val text: String = script.toString

  val bytes: Coeval[ByteStr] = Coeval.evalOnce {
    val s = Array(script.version.value.toByte) ++ ScriptExprWriter.toBytes(script)
    ByteStr(s ++ crypto.secureHash(s).take(checksumLength))
  }

  override def toString: String = text
}

object Script {

  val checksumLength = 4

  def fromBase58String(str: String): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base58.decode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base58: ${ex.getMessage}"))
      script <- fromBytes(bytes)
    } yield script

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
      expr <- ScriptExprReader.fromBytes(sv, scriptBytes)
    } yield Script(expr)
  }
}
