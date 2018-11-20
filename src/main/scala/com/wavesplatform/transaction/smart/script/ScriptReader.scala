package com.wavesplatform.transaction.smart.script

import com.wavesplatform.crypto
import com.wavesplatform.lang.Version
import com.wavesplatform.lang.Version._
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.transaction.ValidationError.ScriptParseError
import com.wavesplatform.transaction.smart.script.v1.ScriptV1

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum         = bytes.takeRight(checksumLength)
    val computedCheckSum = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val version          = bytes.head
    val scriptBytes      = bytes.drop(1).dropRight(checksumLength)

    for {
      _ <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      ver = Version(version.toInt)
      sv <- Either
        .cond(
          SupportedVersions(ver),
          ver,
          ScriptParseError(s"Invalid version: $version")
        )
      script <- ScriptV1
        .validateBytes(scriptBytes)
        .flatMap { _ =>
          Serde.deserialize(scriptBytes).flatMap(ScriptV1(sv, _, checkSize = false))
        }
        .left
        .map(ScriptParseError)
    } yield script
  }

}
