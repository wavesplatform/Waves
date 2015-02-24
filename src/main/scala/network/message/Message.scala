package network.message

import java.io.DataInputStream
import java.util.Arrays

import com.google.common.primitives.{Bytes, Ints}
import network.ConnectedPeer
import scorex.crypto.Crypto

trait Message {

import network.message.Message._._

  val messageType: Int
  val mbSender: Option[ConnectedPeer]
  val mbId: Option[Int]


  def hasId() = mbId.isDefined


  def getHash() = Crypto.sha256(toBytes())


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


case class PingMessage(mbSender: Option[ConnectedPeer] = None, mbId: Option[Int] = None) extends Message {
  override val messageType = Message.PING_TYPE
}

case class GetPeersMessage(mbSender: Option[ConnectedPeer] = None, mbId: Option[Int] = None) extends Message {
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

  def apply(sender: ConnectedPeer, inputStream: DataInputStream): Message = {

    val typeBytes = Array.fill(TYPE_LENGTH)(0: Byte)
    inputStream.readFully(typeBytes)
    val msgType = Ints.fromByteArray(typeBytes)

    //READ HAS ID
    val hasId = inputStream.read()

    val id = if (hasId == 1) {
      //READ ID
      val idBytes = Array.fill(ID_LENGTH)(0: Byte)
      inputStream.readFully(idBytes)
      Some(Ints.fromByteArray(idBytes))
    } else None

    //READ LENGTH
    //byte[] lengthBytes = new byte[Message.MESSAGE_LENGTH]
    //inputStream.readFully(lengthBytes)
    val length = inputStream.readInt() //Ints.fromByteArray(lengthBytes)

    //IF MESSAGE CONTAINS DATA READ DATA AND VALIDATE CHECKSUM
    val data = Array.fill(length)(0: Byte)
    if (length > 0) {
      //READ CHECKSUM
      val checksum = Array.fill(Message.CHECKSUM_LENGTH)(0: Byte)
      inputStream.readFully(checksum)

      //READ DATA
      inputStream.readFully(data)

      //VALIDATE CHECKSUM
      val digest = Arrays.copyOfRange(Crypto.sha256(data), 0, Message.CHECKSUM_LENGTH)

      //CHECK IF CHECKSUM MATCHES
      if (!checksum.sameElements(digest)) throw new Exception("Invalid data checksum length=" + length)
    }

    //todo: sender,id for all?
    msgType match {
      case Message.PING_TYPE => PingMessage(Some(sender), id)
      case Message.GET_PEERS_TYPE => GetPeersMessage(Some(sender), id)
      case Message.PEERS_TYPE => PeersMessage(data)
      case Message.VERSION_TYPE => VersionMessage(data)
      case Message.GET_SIGNATURES_TYPE => GetSignaturesMessage(data)
      case Message.SIGNATURES_TYPE => SignaturesMessage(data)
      case Message.GET_BLOCK_TYPE => GetBlockMessage(data)
      case Message.BLOCK_TYPE => BlockMessage(data)
      case Message.TRANSACTION_TYPE => TransactionMessage(data)
    }
  }
}