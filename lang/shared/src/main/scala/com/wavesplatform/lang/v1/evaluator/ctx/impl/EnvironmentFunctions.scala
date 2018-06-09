package com.wavesplatform.lang.v1.evaluator.ctx.impl

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.evaluator.ctx.CaseObj
import com.wavesplatform.lang.v1.traits.{DataType, Environment, Recipient}
import scodec.bits.ByteVector

import scala.util.Try

class EnvironmentFunctions(environment: Environment) {
  import EnvironmentFunctions._
  private val Global = com.wavesplatform.lang.hacks.Global // Hack for IDEA

  def addressFromPublicKey(pk: ByteVector): ByteVector = {
    val publicKeyHash   = Global.secureHash(pk.toArray).take(HashLength)
    val withoutChecksum = AddressVersion +: environment.networkByte +: publicKeyHash
    val bytes           = withoutChecksum ++ Global.secureHash(withoutChecksum).take(ChecksumLength)
    ByteVector(bytes)
  }

  def addressFromString(str: String): Either[String, Option[ByteVector]] = {
    val base58String = if (str.startsWith(Prefix)) str.drop(Prefix.length) else str
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

  def getData(addr: CaseObj, key: String, dataType: DataType): Either[String, Any] =
    for {
      rawAddressBytes <- addr.fields.get("bytes").fold[Either[String, Any]](Left("Can't find 'bytes'"))(Right(_))
      addressBytes    <- Try(rawAddressBytes.asInstanceOf[ByteVector].toArray).toEither.left.map(_.getMessage)
    } yield environment.data(addressBytes, key, dataType)

  def addressFromAlias(name: String): Either[ExecutionError, Recipient.Address] = environment.resolveAlias(name)

}

object EnvironmentFunctions {
  private val ChecksumLength = 4
  private val HashLength     = 20
  private val AddressVersion = 1: Byte
  private val AddressLength  = 1 + 1 + ChecksumLength + HashLength
  private val Prefix         = "address:"
}
