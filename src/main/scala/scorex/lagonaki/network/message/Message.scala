package scorex.lagonaki.network.message

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints}
import scorex.consensus.ConsensusModule
import scorex.crypto.Sha256._
import scorex.transaction.TransactionModule

import scala.util.Try

abstract class Message {

  import scorex.lagonaki.network.message.Message._

  val messageType: Byte

  //lazy val mhash = hash(bytes)

  val dataBytes: Array[Byte]
  lazy val dataLength: Int = dataBytes.length

  lazy val bytes: Array[Byte] = {
    val typeBytes = Array(messageType)

    val dataWithChecksum = if (dataLength > 0) {
      val checksum = hash(dataBytes).take(ChecksumLength)
      Bytes.concat(checksum, dataBytes)
    } else dataBytes //empty array

    Bytes.concat(MAGIC, typeBytes, Ints.toByteArray(dataLength), dataWithChecksum)
  }
}


object Message {
  val MAGIC = Array(0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)

  val MagicLength = MAGIC.length

  val TypeLength = 4
  //val MessageLength = 4
  val ChecksumLength = 4

  val GetPeersType = 1: Byte
  val PeersType = 2: Byte
  val VersionType = 3: Byte
  val GetSignaturesType = 4: Byte
  val SignaturesType = 5: Byte
  val GetBlockType = 6: Byte
  val BlockType = 7: Byte
  val TransactionType = 8: Byte
  val PingType = 9: Byte

  def parse(bytes: ByteBuffer)(implicit consensusModule: ConsensusModule[_],
                               transactionModule: TransactionModule[_]): Try[Message] = Try {
    val magic = new Array[Byte](MagicLength)
    bytes.get(magic)

    if (!magic.sameElements(Message.MAGIC)) throw new Exception("wrong magic")

    val msgType = bytes.get

    val length = bytes.getInt

    val data = Array.fill(length)(0: Byte)
    if (length > 0) {
      //READ CHECKSUM
      val checksum = new Array[Byte](Message.ChecksumLength)
      bytes.get(checksum)

      //READ DATA
      bytes.get(data)

      //VALIDATE CHECKSUM
      val digest = hash(data).take(Message.ChecksumLength)

      //CHECK IF CHECKSUM MATCHES
      if (!checksum.sameElements(digest)) throw new Exception("Invalid data checksum length=" + length)
    }

    msgType match {
      case Message.GetPeersType => GetPeersMessage
      case Message.PeersType => PeersMessage(data)
      case Message.VersionType => ScoreMessage(data)
      case Message.GetSignaturesType => GetSignaturesMessage(data)
      case Message.SignaturesType => SignaturesMessage(data)
      case Message.GetBlockType => GetBlockMessage(data)
      case Message.BlockType => BlockMessage(data)
      case Message.TransactionType => TransactionMessage(data)
    }
  }
}

