package scorex.network.message

import java.util.Arrays
import com.google.common.primitives.{Bytes, Ints}


case class SignaturesMessage(signatures: Seq[Array[Byte]], mbId: Option[Int] = None) extends Message {

  import scorex.network.message.SignaturesMessage._

  override val messageType = Message.SIGNATURES_TYPE

  override def toBytes() = {
    val length = signatures.size
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), DATA_LENGTH, 0)

    //WRITE SIGNATURES
    val data = signatures.foldLeft(lengthBytes) { case (bytes, header) => Bytes.concat(bytes, header)}

    //ADD CHECKSUM
    Bytes.concat(super.toBytes(), generateChecksum(data), data)
  }

  override def getDataLength() = DATA_LENGTH + (signatures.size * SIGNATURE_LENGTH)
}


object SignaturesMessage {
  private val SIGNATURE_LENGTH = 128
  private val DATA_LENGTH = 4

  def apply(data: Array[Byte]): SignaturesMessage = {
    //READ LENGTH
    val lengthBytes = Arrays.copyOfRange(data, 0, DATA_LENGTH)
    val length = Ints.fromByteArray(lengthBytes)

    //CHECK IF DATA MATCHES LENGTH
    if (data.length != DATA_LENGTH + (length * SIGNATURE_LENGTH))
      throw new Exception("Data does not match length")

    //CREATE HEADERS LIST
    val headers = (0 to length - 1).map { i =>
      val position = DATA_LENGTH + (i * SIGNATURE_LENGTH)
      Arrays.copyOfRange(data, position, position + SIGNATURE_LENGTH)
    }
    new SignaturesMessage(headers.toList)
  }
}
