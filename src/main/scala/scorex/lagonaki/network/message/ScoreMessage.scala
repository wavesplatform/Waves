package scorex.lagonaki.network.message

import com.google.common.primitives.Ints

case class ScoreMessage(height: Int, score: BigInt) extends Message {
  lazy val scoreBytes = score.toByteArray

  override val messageType = Message.VersionType

  override lazy val dataBytes = {
    val bb = java.nio.ByteBuffer.allocate(4 + scoreBytes.length)
    bb.putInt(height)
    bb.put(scoreBytes)
    bb.array()
  }
}


object ScoreMessage {
  def apply(data: Array[Byte]): ScoreMessage = {
    val heightBytes = data.take(4)
    val height = Ints.fromByteArray(heightBytes)
    val score = BigInt(data.takeRight(data.length - 4))
    ScoreMessage(height, score)
  }
}
