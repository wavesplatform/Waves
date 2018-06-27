package com.wavesplatform.lang.v1.evaluator.ctx.impl

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.evaluator.ctx.CaseObj
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import com.wavesplatform.lang.v1.traits.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.{DataType, Environment, Recipient}
import scodec.bits.ByteVector

class EnvironmentFunctions(environment: Environment) {
  import EnvironmentFunctions._
  private val Global = com.wavesplatform.lang.hacks.Global // Hack for IDEA

  def addressFromString(str: String): Either[String, Option[ByteVector]] = {
    val base58String = if (str.startsWith(AddressPrefix)) str.drop(AddressPrefix.length) else str
    Global.base58Decode(base58String, Global.MaxAddressLength) match {
      case Left(e) => Left(e)
      case Right(addressBytes) =>
        val version = addressBytes.head
        val network = addressBytes.tail.head
        lazy val checksumCorrect = {
          val checkSum          = addressBytes.takeRight(ChecksumLength)
          val checkSumGenerated = Global.secureHash(addressBytes.dropRight(ChecksumLength)).take(ChecksumLength)
          checkSum sameElements checkSumGenerated
        }

        if (version == AddressVersion && network == environment.networkByte && addressBytes.length == AddressLength && checksumCorrect)
          Right(Some(ByteVector(addressBytes)))
        else Right(None)
    }
  }

  def getData(addressOrAlias: CaseObj, key: String, dataType: DataType): Either[String, Option[Any]] = {
    val objTypeName = addressOrAlias.caseType.name

    val recipientEi =
      if (objTypeName == Types.addressType.name) {
        addressOrAlias.fields
          .get("bytes")
          .toRight("Can't find 'bytes'")
          .map(_.asInstanceOf[ByteVector])
          .map(Address)
      } else if (objTypeName == Types.aliasType.name) {
        addressOrAlias.fields
          .get("alias")
          .toRight("Can't find alias")
          .map(_.asInstanceOf[String])
          .map(Alias)
      } else {
        Left(s"$addressOrAlias neither Address nor alias")
      }

    recipientEi.map(environment.data(_, key, dataType))
  }

  def addressFromAlias(name: String): Either[ExecutionError, Recipient.Address] = environment.resolveAlias(name)

}

object EnvironmentFunctions {
  val ChecksumLength       = 4
  val HashLength           = 20
  val AddressVersion: Byte = 1
  val AddressLength: Int   = 1 + 1 + ChecksumLength + HashLength
  val AddressPrefix        = "address:"
}
