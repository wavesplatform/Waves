package com.wavesplatform.lang.script
import com.wavesplatform.lang.ValidationError.ScriptParseError
import com.wavesplatform.lang.contract.ContractSerDe
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.{BaseGlobal, ContractLimits, Serde}

object ScriptReader {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte], checkComplexity: Boolean = true): Either[ScriptParseError, Script] = {
    val checkSum          = bytes.takeRight(checksumLength)
    val computedCheckSum  = Global.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)

    (for {
      a <- {
        if (bytes.isEmpty)
          Left(ScriptParseError(s"Can't parse empty script bytes"))
        else {
          val versionByte: Byte = bytes.head
          if (versionByte == 0 && bytes.length > 2)
            Right((DirectiveDictionary[ContentType].idMap(bytes(1)), DirectiveDictionary[StdLibVersion].idMap(bytes(2)), 3))
          else if (versionByte == V1.id || versionByte == V2.id || versionByte == V3.id)
            Right((Expression, DirectiveDictionary[StdLibVersion].idMap(versionByte.toInt), 1))
          else Left(ScriptParseError(s"Can't parse script bytes starting with ${bytes.take(3).mkString("[", ",", "]")}"))
        }
      }
      (scriptType, stdLibVersion, offset) = a
      scriptBytes                         = bytes.drop(offset).dropRight(checksumLength)

      _ <- Either.cond(java.util.Arrays.equals(checkSum, computedCheckSum), (), ScriptParseError("Invalid checksum"))
      s <- scriptType match {
        case Expression =>
          for {
            _ <- if (checkComplexity) {
              ExprScript.validateBytes(scriptBytes)
            } else {
              Right(())
            }
            bytes <- Serde.deserialize(scriptBytes).map(_._1)
            s     <- ExprScript(stdLibVersion, bytes, checkSize = false, checkComplexity = checkComplexity)
          } yield s
        case DApp =>
          for {
            dapp <- ContractSerDe.deserialize(scriptBytes)
            _ <- Either
              .cond(
                dapp.meta.size <= ContractLimits.MaxContractMetaSizeInBytes,
                (),
                s"Script meta size in bytes must be not greater than ${ContractLimits.MaxContractMetaSizeInBytes}"
              )
            s <- ContractScript(stdLibVersion, dapp)
          } yield s
      }
    } yield s).left
      .map(m => ScriptParseError(m.toString))
  }

}
