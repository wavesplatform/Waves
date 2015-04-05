package scorex.network.message

import java.nio.ByteBuffer
import java.util.Arrays
import com.google.common.primitives.{Bytes, Ints}
import scorex.crypto.Crypto

import scala.util.Try

abstract class Message {
  import scorex.network.message.Message._

  val messageType: Int
  val mbId: Option[Int]

  def hasId() = mbId.isDefined

  def hash() = Crypto.sha256(toBytes())

  def toBytes() = {
    // MESSAGE TYPE
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(messageType), TYPE_LENGTH, 0)

    //ID
    val idBytes = mbId.map { id =>
      val hasIdBytes = Array(1: Byte)
      val idBytes = Bytes.ensureCapacity(Ints.toByteArray(id), ID_LENGTH, 0)
      Bytes.concat(hasIdBytes, idBytes)
    }.getOrElse(Array(0: Byte))

    Bytes.concat(MAGIC, typeBytes, idBytes, Ints.toByteArray(this.getDataLength()))
  }

  protected def getDataLength() = 0

  protected def generateChecksum(data: Array[Byte]) =
    Arrays.copyOfRange(Crypto.sha256(data), 0, CHECKSUM_LENGTH)
}


case class PingMessage(mbId: Option[Int] = None) extends Message {
  override val messageType = Message.PING_TYPE
}

case class GetPeersMessage(mbId: Option[Int] = None) extends Message {
  override val messageType = Message.GET_PEERS_TYPE
}

object Message {
  val MAGIC = Array(0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)

  val MAGIC_LENGTH = MAGIC.length

  val TYPE_LENGTH = 4
  val ID_LENGTH = 4
  val MESSAGE_LENGTH = 4
  val CHECKSUM_LENGTH = 4

  val GET_PEERS_TYPE = 1
  val PEERS_TYPE = 2
  val VERSION_TYPE = 3
  val GET_SIGNATURES_TYPE = 4
  val SIGNATURES_TYPE = 5
  val GET_BLOCK_TYPE = 6
  val BLOCK_TYPE = 7
  val TRANSACTION_TYPE = 8
  val PING_TYPE = 9

  def parse(bytes: ByteBuffer): Try[Message] = Try {
    val magic = new Array[Byte](MESSAGE_LENGTH)
    bytes.get(magic)

    if (!magic.sameElements(Message.MAGIC)) throw new Exception("wrong magic")

    val msgType = bytes.getInt

    val hasId = bytes.get

    val idOpt = if (hasId == 1) Some(bytes.getInt) else None

    val length = bytes.getInt

    val data = Array.fill(length)(0: Byte)
    if (length > 0) {
      //READ CHECKSUM
      val checksum = new Array[Byte](Message.CHECKSUM_LENGTH)
      bytes.get(checksum)

      //READ DATA
      bytes.get(data)

      //VALIDATE CHECKSUM
      val digest = Arrays.copyOfRange(Crypto.sha256(data), 0, Message.CHECKSUM_LENGTH)

      //CHECK IF CHECKSUM MATCHES
      if (!checksum.sameElements(digest)) throw new Exception("Invalid data checksum length=" + length)
    }

    //todo: id for all?
    msgType match {
      case Message.PING_TYPE => PingMessage(idOpt)
      case Message.GET_PEERS_TYPE => GetPeersMessage(idOpt)
      case Message.PEERS_TYPE => PeersMessage(data)
      case Message.VERSION_TYPE => HeightMessage(data)
      case Message.SIGNATURES_TYPE => SignaturesMessage(data)
      case Message.GET_BLOCK_TYPE => GetBlockMessage(data)
      case Message.BLOCK_TYPE => BlockMessage(data)
      case Message.TRANSACTION_TYPE => TransactionMessage(data)
    }
  }
}