package com.wavesplatform.transaction.smart.script

import com.wavesplatform.crypto
import com.wavesplatform.lang.contract.ContractSerDe
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.{ContentType, StdLibVersion}
import com.wavesplatform.transaction.ValidationError.ScriptParseError
import com.wavesplatform.transaction.smart.script.v1._

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum          = bytes.takeRight(checksumLength)
    val computedCheckSum  = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val versionByte: Byte = bytes.head
    val (scriptType, stdLibVersion, offset) =
      if (versionByte == 0)
        (ContentType.parseId(bytes(1)), StdLibVersion.parseVersion(bytes(2)), 3)
      else if (versionByte == StdLibVersion.V1.toByte || versionByte == StdLibVersion.V2.toByte)
        (ContentType.Expression, StdLibVersion(versionByte.toInt), 1)
      else ???
    val scriptBytes = bytes.drop(offset).dropRight(checksumLength)

    (for {
      _ <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      s <- scriptType match {
        case ContentType.Expression =>
          for {
            _     <- ExprScript.validateBytes(scriptBytes)
            bytes <- Serde.deserialize(scriptBytes).map(_._1)
            s     <- ExprScript(stdLibVersion, bytes, checkSize = false)
          } yield s
        case ContentType.Contract =>
          for {
            bytes <- ContractSerDe.deserialize(scriptBytes)
            s     <- ContractScript(stdLibVersion, bytes)
          } yield s
      }
    } yield s).left
      .map(m => ScriptParseError(m.toString))
  }

}
