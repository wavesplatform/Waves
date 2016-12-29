package scorex.network.message

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import scorex.crypto.hash.FastCryptographicHash._
import scala.util.Try

case class MessageHandler(specs: Seq[MessageSpec[_]]) {

  import Message._

  private val specsMap = Map(specs.map(s => s.messageCode -> s): _*)
    .ensuring(m => m.size == specs.size, "Duplicate message codes")

  /**
    * Parse raw packet to Message
    * @param bytes MAGIC ++ Array(spec.messageCode) ++ Ints.toByteArray(dataLength) ++ dataWithChecksum
    * @return
    */
  def parseBytes(bytes: ByteBuffer): Try[(MessageSpec[_], Array[Byte])] = Try {
    val magic = new Array[Byte](MagicLength)
    bytes.get(magic)

    require(magic.sameElements(MAGIC), "Wrong magic bytes: " + magic.mkString)

    val msgCode = bytes.get

    val length = bytes.getInt
    require(length >= 0, s"Data length $length is negative! Message code = $msgCode")

    val checksumLength = if (length > 0) ChecksumLength else 0
    require(length == bytes.limit - MagicLength - checksumLength - MessageCodeLength - LengthFieldLength,
      s"Invalid value of length field = $length. Message code = $msgCode")

    val msgData: Array[Byte] = length > 0 match {
      case true =>
        val data = new Array[Byte](length)
        //READ CHECKSUM
        val checksum = new Array[Byte](ChecksumLength)
        bytes.get(checksum)

        //READ DATA
        bytes.get(data)

        //VALIDATE CHECKSUM
        val digest = hash(data).take(Message.ChecksumLength)

        //CHECK IF CHECKSUM MATCHES
        require(checksum.sameElements(digest), s"Invalid data checksum length = $length")
        data

      case false => Array()
    }

    val spec = specsMap.get(msgCode).ensuring(_.isDefined, "No message handler").get

    (spec, msgData)
  }
}

object MessageHandler {
  case class RawNetworkData(spec: MessageSpec[_], data: Array[Byte], remote: InetSocketAddress)
}