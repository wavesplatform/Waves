package scorex.network.message

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints}
import scorex.crypto.Crypto

import scala.util.Try

abstract class Message {

  import scorex.network.message.Message._

  val messageType: Byte

  def hash() = Crypto.sha256(serialize())

  val dataBytes: Array[Byte]
  lazy val dataLength: Int = dataBytes.length

  def serialize(): Array[Byte] = {
    val typeBytes = Array(messageType)

    val dataWithChecksum = if (dataLength > 0) {
      val checksum = Crypto.sha256(dataBytes).take(CHECKSUM_LENGTH)
      Bytes.concat(checksum, dataBytes)
    } else dataBytes //empty array

    Bytes.concat(MAGIC, typeBytes, Ints.toByteArray(dataLength), dataWithChecksum)
  }
}


object Message {
  val MAGIC = Array(0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)

  val MAGIC_LENGTH = MAGIC.length

  val TYPE_LENGTH = 4
  val MESSAGE_LENGTH = 4
  val CHECKSUM_LENGTH = 4

  val GET_PEERS_TYPE = 1: Byte
  val PEERS_TYPE = 2: Byte
  val VERSION_TYPE = 3: Byte
  val GET_SIGNATURES_TYPE = 4: Byte
  val SIGNATURES_TYPE = 5: Byte
  val GET_BLOCK_TYPE = 6: Byte
  val BLOCK_TYPE = 7: Byte
  val TRANSACTION_TYPE = 8: Byte
  val PING_TYPE = 9: Byte

  def parse(bytes: ByteBuffer): Try[Message] = Try {
    val magic = new Array[Byte](MESSAGE_LENGTH)
    bytes.get(magic)

    if (!magic.sameElements(Message.MAGIC)) throw new Exception("wrong magic")

    val msgType = bytes.get

    val length = bytes.getInt

    val data = Array.fill(length)(0: Byte)
    if (length > 0) {
      //READ CHECKSUM
      val checksum = new Array[Byte](Message.CHECKSUM_LENGTH)
      bytes.get(checksum)

      //READ DATA
      bytes.get(data)

      //VALIDATE CHECKSUM
      val digest = Crypto.sha256(data).take(Message.CHECKSUM_LENGTH)

      //CHECK IF CHECKSUM MATCHES
      if (!checksum.sameElements(digest)) throw new Exception("Invalid data checksum length=" + length)
    }

    msgType match {
      case Message.PING_TYPE => PingMessage
      case Message.GET_PEERS_TYPE => GetPeersMessage
      case Message.PEERS_TYPE => PeersMessage(data)
      case Message.VERSION_TYPE => ScoreMessage(data)
      case Message.GET_SIGNATURES_TYPE => GetSignaturesMessage(data)
      case Message.SIGNATURES_TYPE => SignaturesMessage(data)
      case Message.GET_BLOCK_TYPE => GetBlockMessage(data)
      case Message.BLOCK_TYPE => BlockMessage(data)
      case Message.TRANSACTION_TYPE => TransactionMessage(data)
    }
  }
}

case object PingMessage extends Message {
  override val messageType = Message.PING_TYPE
  override val dataBytes = Array[Byte]()
}

case object GetPeersMessage extends Message {
  override val messageType = Message.GET_PEERS_TYPE
  override val dataBytes = Array[Byte]()
}