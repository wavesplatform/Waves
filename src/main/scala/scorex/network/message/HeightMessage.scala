package scorex.network.message

import com.google.common.primitives.{Bytes, Ints}

//todo: longest chain rule doesn't make sense for proof-of-stake at all and probably dangerous!
//todo: so should be replaced with cumulative difficulty (aka maxvalid function)
case class HeightMessage(height: Int) extends Message {

  override val messageType = Message.VERSION_TYPE

  override def toBytes() = {
    val heightBytes = Bytes.ensureCapacity(Ints.toByteArray(height), HeightMessage.HEIGHT_LENGTH, 0)
    Bytes.concat(super.toBytes(), generateChecksum(heightBytes), heightBytes)
  }

  override def getDataLength() = HeightMessage.HEIGHT_LENGTH
}


object HeightMessage {
  private val HEIGHT_LENGTH = 4

  def apply(data: Array[Byte]): HeightMessage = {
    //CHECK IF DATA MATCHES LENGTH
    require(data.length == HEIGHT_LENGTH, "Data does not match length")
    val height = Ints.fromByteArray(data)
    new HeightMessage(height)
  }
}
