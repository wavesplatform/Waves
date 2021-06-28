package com.wavesplatform.lang.script
import com.wavesplatform.lang.ValidationError.ScriptParseError
import com.wavesplatform.lang.contract.ContractSerDe
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.{BaseGlobal, Serde}

object ScriptReader {
  val FreeCallHeader: Byte = -1

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA
  private val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum         = bytes.takeRight(checksumLength)
    val computedCheckSum = Global.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)

    for {
      versionByte <- bytes.headOption.toRight(ScriptParseError("Can't parse empty script bytes"))
      (scriptType, stdLibVersion, isFreeCall, offset) <- {
        val contentTypes   = DirectiveDictionary[ContentType].idMap
        val stdLibVersions = DirectiveDictionary[StdLibVersion].idMap
        versionByte match {
          case 0 =>
            if (bytes.length <= 2)
              Left(ScriptParseError(s"Illegal length of script: ${bytes.length}"))
            else if (!contentTypes.contains(bytes(1)))
              Left(ScriptParseError(s"Invalid content type of script: ${bytes(1)}"))
            else if (!stdLibVersions.contains(bytes(2)))
              Left(ScriptParseError(s"Invalid version of script: ${bytes(2)}"))
            else
              Right((contentTypes(bytes(1)), stdLibVersions(bytes(2)), false, 3))
          case FreeCallHeader =>
            Right(Expression, stdLibVersions(bytes(1)), true, 2)
          case v if !stdLibVersions.contains(v) => Left(ScriptParseError(s"Invalid version of script: $v"))
          case v                                => Right((Expression, stdLibVersions(v.toInt), false, 1))
        }
      }
      scriptBytes = bytes.drop(offset).dropRight(checksumLength)

      _ <- Either.cond(java.util.Arrays.equals(checkSum, computedCheckSum), (), ScriptParseError("Invalid checksum"))
      s <- (scriptType match {
        case Expression | Library =>
          for {
            bytes <- Serde.deserialize(scriptBytes).map(_._1)
            s     <- ExprScript(stdLibVersion, bytes, isFreeCall, checkSize = false)
          } yield s
        case DApp =>
          for {
            dapp <- ContractSerDe.deserialize(scriptBytes)
            s    <- ContractScript(stdLibVersion, dapp)
          } yield s
      }).left
        .map(ScriptParseError)
    } yield s
  }
}
