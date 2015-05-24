package scorex.network.message

import com.google.common.primitives.{Bytes, Ints}

case class ScoreMessage(height: Int, score: BigInt) extends Message {
  lazy val scoreBytes = score.toByteArray

  override val messageType = Message.VERSION_TYPE

  override def toBytes() = {

    val bb = java.nio.ByteBuffer.allocate(4 + scoreBytes.length)
    bb.putInt(height)
    bb.put(scoreBytes)
    val bytes = bb.array()

    Bytes.concat(super.toBytes(), generateChecksum(bytes), bytes)
  }

  override def getDataLength() = 4 + scoreBytes.length
}


object ScoreMessage {
  def apply(data: Array[Byte]): ScoreMessage = {
    val heightBytes = data.take(4)
    val height = Ints.fromByteArray(heightBytes)
    val score = BigInt(data.takeRight(data.length - 4))
    ScoreMessage(height, score)
  }
}
