package scorex.network.message

import com.google.common.primitives.{Bytes, Ints}


abstract class SignaturesSeqMessage extends Message {

  import SignaturesSeqMessage._

  val signatures: Seq[Array[Byte]]

  override def toBytes() = {
    val length = signatures.size
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), DATA_LENGTH, 0)

    //WRITE SIGNATURES
    val data = signatures.foldLeft(lengthBytes) { case (bytes, header) => Bytes.concat(bytes, header) }

    //ADD CHECKSUM
    Bytes.concat(super.toBytes(), generateChecksum(data), data)
  }

  override def getDataLength() = DATA_LENGTH + (signatures.size * SIGNATURE_LENGTH)
}

object SignaturesSeqMessage {

  val SIGNATURE_LENGTH = 128
  private val DATA_LENGTH = 4

  def parse(data: Array[Byte]) = {
    //READ LENGTH
    val lengthBytes = data.take(DATA_LENGTH)
    val length = Ints.fromByteArray(lengthBytes)

    //CHECK IF DATA MATCHES LENGTH
    if (data.length != DATA_LENGTH + (length * SIGNATURE_LENGTH))
      throw new Exception("Data does not match length")

    //CREATE HEADERS LIST
    (0 to length - 1).map { i =>
      val position = DATA_LENGTH + (i * SIGNATURE_LENGTH)
      data.slice(position, position + SIGNATURE_LENGTH)
    }.toSeq
  }
}


case class GetSignaturesMessage(override val signatures: Seq[Array[Byte]]) extends SignaturesSeqMessage {
  require(signatures.forall(_.length == SignaturesSeqMessage.SIGNATURE_LENGTH))

  override val messageType = Message.GET_SIGNATURES_TYPE

  def this(data: Array[Byte]) = this(SignaturesSeqMessage.parse(data))
}

object GetSignaturesMessage {
  def apply(data: Array[Byte]) = new GetSignaturesMessage(data)
}


case class SignaturesMessage(override val signatures: Seq[Array[Byte]]) extends SignaturesSeqMessage {
  require(signatures.forall(_.length == SignaturesSeqMessage.SIGNATURE_LENGTH))

  override val messageType = Message.SIGNATURES_TYPE

  def this(data: Array[Byte]) = this(SignaturesSeqMessage.parse(data))
}

object SignaturesMessage {
  def apply(data: Array[Byte]) = new SignaturesMessage(data)
}