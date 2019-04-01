package com.wavesplatform.transaction.smart.script

import com.wavesplatform.crypto
import com.wavesplatform.lang.contract.ContractSerDe
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.{ContentType, StdLibVersion}
import com.wavesplatform.transaction.ValidationError.ScriptParseError
import com.wavesplatform.transaction.smart.script.v1._

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte], checkComplexity: Boolean = true): Either[ScriptParseError, Script] = {
    val checkSum          = bytes.takeRight(checksumLength)
    val computedCheckSum  = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val versionByte: Byte = bytes.head
    (for {
      a <- {
        if (versionByte == 0)
          Right((ContentType.parseId(bytes(1)), StdLibVersion.parseVersion(bytes(2)), 3))
        else if (versionByte == StdLibVersion.V1.toByte || versionByte == StdLibVersion.V2.toByte || versionByte == StdLibVersion.V3.toByte)
          Right((ContentType.Expression, StdLibVersion(versionByte.toInt), 1))
        else Left(ScriptParseError(s"Can't parse script bytes starting with [${bytes(0).toInt},${bytes(1).toInt},${bytes(2).toInt}]"))
      }
      (scriptType, stdLibVersion, offset) = a
      scriptBytes                         = bytes.drop(offset).dropRight(checksumLength)

      _ <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      s <- scriptType match {
        case ContentType.Expression =>
          for {
            _ <- if (checkComplexity) {
              ExprScript.validateBytes(scriptBytes)
            } else {
              Right(())
            }
            bytes <- Serde.deserialize(scriptBytes).map(_._1)
            s     <- ExprScript(stdLibVersion, bytes, checkSize = false, checkComplexity = checkComplexity)
          } yield s
        case ContentType.DApp =>
          for {
            bytes <- ContractSerDe.deserialize(scriptBytes)
            s     <- ContractScript(stdLibVersion, bytes)
          } yield s
      }
    } yield s).left
      .map(m => ScriptParseError(m.toString))
  }

}
