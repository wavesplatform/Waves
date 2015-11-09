package scorex.lagonaki.network.message

import com.google.common.primitives.{Bytes, Ints}
import scorex.crypto.SigningFunctionsImpl.SignatureLength

abstract class SignaturesSeqMessage extends Message {

  import SignaturesSeqMessage._

  val signatures: Seq[Array[Byte]]

  override lazy val dataBytes = {
    val length = signatures.size
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), DataLength, 0)

    //WRITE SIGNATURES
    signatures.foldLeft(lengthBytes) { case (bs, header) => Bytes.concat(bs, header) }
  }
}

object SignaturesSeqMessage {

  private val DataLength = 4

  def parse(data: Array[Byte]) = {
    //READ LENGTH
    val lengthBytes = data.take(DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    //CHECK IF DATA MATCHES LENGTH
    if (data.length != DataLength + (length * SignatureLength))
      throw new Exception("Data does not match length")

    //CREATE HEADERS LIST
    (0 to length - 1).map { i =>
      val position = DataLength + (i * SignatureLength)
      data.slice(position, position + SignatureLength)
    }.toSeq
  }
}


case class GetSignaturesMessage(override val signatures: Seq[Array[Byte]]) extends SignaturesSeqMessage {
  require(signatures.forall(_.length == SignatureLength))

  override val messageType = Message.GetSignaturesType

  def this(data: Array[Byte]) = this(SignaturesSeqMessage.parse(data))
}

object GetSignaturesMessage {
  def apply(data: Array[Byte]) = new GetSignaturesMessage(data)
}


case class SignaturesMessage(override val signatures: Seq[Array[Byte]]) extends SignaturesSeqMessage {
  require(signatures.forall(_.length == SignatureLength))

  override val messageType = Message.SignaturesType

  def this(data: Array[Byte]) = this(SignaturesSeqMessage.parse(data))
}

object SignaturesMessage {
  def apply(data: Array[Byte]) = new SignaturesMessage(data)
}