package com.wavesplatform.lang

import com.wavesplatform.lang.ctx.{LazyVal, Obj}
import com.wavesplatform.lang.traits.{DataType, Environment}
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
    Global.base58Decode(base58String) match {
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

  def getData(addr: Obj, key: String, dataType: DataType): Either[String, Any] =
    for {
      bytes           <- addr.fields.get("bytes").fold[Either[String, LazyVal]](Left("Can't find 'bytes'"))(Right(_))
      rawAddressBytes <- bytes.value.value()
      addressBytes    <- Try(rawAddressBytes.asInstanceOf[ByteVector].toArray).toEither.left.map(_.getMessage)
      r               <- environment.data(addressBytes, key, dataType).fold[Either[String, Any]](Left("Data is empty"))(Right(_))
    } yield r

  def addressFromRecipient(fields: Map[String, LazyVal]): Either[ExecutionError, Array[Byte]] = {
    val bytes = fields("bytes").value.map(_.asInstanceOf[ByteVector]).value()
    bytes.flatMap(bv => environment.resolveAddress(bv.toArray))
  }
}

object EnvironmentFunctions {
  private val ChecksumLength = 4
  private val HashLength     = 20
  private val AddressVersion = 1: Byte
  private val AddressLength  = 1 + 1 + ChecksumLength + HashLength
  private val Prefix         = "address:"
}
