package com.wavesplatform.network.message

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.crypto
import monix.eval.Coeval

import scala.util.{Success, Try}

case class Message[Content <: AnyRef](spec: MessageSpec[Content], input: Either[Array[Byte], Content]) {
  import Message.{ChecksumLength, MagicBytes}

  lazy val dataBytes = input match {
    case Left(db) => db
    case Right(d) => spec.serializeData(d)
  }

  lazy val data: Try[Content] = input match {
    case Left(db) => spec.deserializeData(db)
    case Right(d) => Success(d)
  }

  lazy val dataLength: Int = dataBytes.length

  val bytes = Coeval.evalOnce {
    val dataWithChecksum = if (dataLength > 0) {
      val checksum = crypto.fastHash(dataBytes).take(ChecksumLength)
      Bytes.concat(checksum, dataBytes)
    } else dataBytes //empty array

    Bytes.concat(MagicBytes, Array(spec.messageCode), Ints.toByteArray(dataLength), dataWithChecksum)
  }
}

object Message {
  type MessageCode = Byte

  val ChecksumLength    = 4
  val LengthFieldLength = 4
  val MessageCodeLength = 1

  private val MagicBytes = Array(0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)
}
