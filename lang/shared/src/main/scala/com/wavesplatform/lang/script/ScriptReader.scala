package com.wavesplatform.lang.script

import com.wavesplatform.lang.ValidationError.ScriptParseError
import com.wavesplatform.lang.contract.serialization.{ContractSerDeV1, ContractSerDeV2}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.serialization.{SerdeV1, SerdeV2}
import com.wavesplatform.common.utils.EitherExt2

object ScriptReader {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA
  private val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum         = bytes.takeRight(checksumLength)
    val computedCheckSum = Global.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)

    for {
      versionOrZeroByte <- bytes.headOption.toRight(ScriptParseError("Can't parse empty script bytes"))
      (scriptType, stdLibVersion, offset) <- {
        val contentTypes   = DirectiveDictionary[ContentType].idMap
        val stdLibVersions = DirectiveDictionary[StdLibVersion].idMap
        versionOrZeroByte match {
          case 0 =>
            if (bytes.length <= 2)
              Left(ScriptParseError(s"Illegal length of script: ${bytes.length}"))
            else if (!contentTypes.contains(bytes(1)))
              Left(ScriptParseError(s"Invalid content type of script: ${bytes(1)}"))
            else if (!stdLibVersions.contains(bytes(2)))
              Left(ScriptParseError(s"Invalid version of script: ${bytes(2)}"))
            else
              Right((contentTypes(bytes(1)), stdLibVersions(bytes(2)), 3))
          case v if !stdLibVersions.contains(v) => Left(ScriptParseError(s"Invalid version of script: $v"))
          case v if v < V6.id => Right((Expression, stdLibVersions(v.toInt), 1))
          case v =>
            if (bytes.length < 2)
              Left(ScriptParseError(s"Illegal length of script: ${bytes.length}"))
            else if (!contentTypes.contains(bytes(1)))
              Left(ScriptParseError(s"Invalid content type of script: ${bytes(1)}"))
            else
              Right((contentTypes(bytes(1)), stdLibVersions(v.toInt), 2))
        }
      }
      scriptBytes = bytes.drop(offset).dropRight(checksumLength)

      _ <- Either.cond(java.util.Arrays.equals(checkSum, computedCheckSum), (), ScriptParseError("Invalid checksum"))
      s <- (scriptType match {
        case Expression | Library =>
          val serde = if (stdLibVersion < V6) {
            SerdeV1
          } else {
            SerdeV2
          }
          for {
            bytes <- serde.deserialize(scriptBytes).map(_._1)
            s     <- ExprScript(stdLibVersion, bytes, checkSize = false)
          } yield s
        case DApp =>
          val contractSerDe = if (stdLibVersion < V6) {
            ContractSerDeV1
          } else {
            ContractSerDeV2
          }
          for {
            dapp <- contractSerDe.deserialize(scriptBytes)
            s    <- ContractScript(stdLibVersion, dapp)
          } yield s
      }).left
        .map(ScriptParseError.apply)
    } yield s
  }
}
