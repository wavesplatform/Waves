package scorex.network.message

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints}
import scorex.crypto.Sha256._
import scorex.network.ConnectedPeer

import scala.util.{Success, Try}


trait MessageSpec[Content] {
  val messageCode: Message.MessageCode
  val messageName: String

  def deserializeData(bytes: Array[Byte]): Try[Content]

  def serializeData(data: Content): Array[Byte]

  override def toString = s"MessageSpec($messageCode: $messageName)"
}


case class Message[Content](spec: MessageSpec[Content],
                            input: Either[Array[Byte], Content],
                            source: Option[ConnectedPeer]
                           ) {

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
}


case class MessageHandler(specs: Seq[MessageSpec[_]]) {

  import Message._

  private val specsMap = {
    Map(specs.map(s => s.messageCode -> s): _*)
      .ensuring(m => m.size == specs.size, "Duplicate message codes")
  }

  //MAGIC ++ Array(spec.messageCode) ++ Ints.toByteArray(dataLength) ++ dataWithChecksum
  def parse(bytes: ByteBuffer, sourceOpt: Option[ConnectedPeer]): Try[Message[_]] = Try {
    val magic = new Array[Byte](MagicLength)
    bytes.get(magic)

    assert(magic.sameElements(Message.MAGIC), "Wrong magic bytes")

    val msgCode = bytes.get

    val length = bytes.getInt
    assert(length >= 0, "Data length is negative!")

    val msgData: Array[Byte] = length > 0 match {
      case true =>
        val data = new Array[Byte](length)
        //READ CHECKSUM
        val checksum = new Array[Byte](Message.ChecksumLength)
        bytes.get(checksum)

        //READ DATA
        bytes.get(data)

        //VALIDATE CHECKSUM
        val digest = hash(data).take(Message.ChecksumLength)

        //CHECK IF CHECKSUM MATCHES
        assert(checksum.sameElements(digest), s"Invalid data checksum length = $length")
        data

      case false => Array()
    }

    val spec = specsMap.get(msgCode).ensuring(_.isDefined, "No message handler").get

    Message(spec, Left(msgData), sourceOpt)
  }
}