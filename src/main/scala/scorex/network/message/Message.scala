package scorex.network.message

import com.google.common.primitives.{Bytes, Ints}
import scorex.crypto.hash.FastCryptographicHash._
import scorex.network.ConnectedPeer
import scorex.serialization.BytesSerializable

import scala.util.{Success, Try}

case class Message[Content](spec: MessageSpec[Content],
                            input: Either[Array[Byte], Content],
                            source: Option[ConnectedPeer]) extends BytesSerializable {

  import Message.{ChecksumLength, MAGIC}

  lazy val dataBytes = input match {
    case Left(db) => db
    case Right(d) => spec.serializeData(d)
  }

  lazy val data: Try[Content] = input match {
    case Left(db) => spec.deserializeData(db)
    case Right(d) => Success(d)
  }

  lazy val dataLength: Int = dataBytes.length

  lazy val bytes: Array[Byte] = {
    val dataWithChecksum = if (dataLength > 0) {
      val checksum = hash(dataBytes).take(ChecksumLength)
      Bytes.concat(checksum, dataBytes)
    } else dataBytes //empty array

    MAGIC ++ Array(spec.messageCode) ++ Ints.toByteArray(dataLength) ++ dataWithChecksum
  }
}


object Message {
  type MessageCode = Byte

  val MAGIC = Array(0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)

  val MagicLength = MAGIC.length

  val ChecksumLength = 4
  val LengthFieldLength = 4
  val MessageCodeLength = 1
}