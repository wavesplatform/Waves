package scorex.network.message

import com.google.common.primitives.Bytes

case class ScoreMessage(score: BigInt) extends Message {

  private lazy val scoreBytes = score.toByteArray

  override val messageType = Message.VERSION_TYPE

  override def toBytes() =
    Bytes.concat(super.toBytes(), generateChecksum(scoreBytes), scoreBytes)

  override def getDataLength() = scoreBytes.length
}


object ScoreMessage {
  def apply(data: Array[Byte]): ScoreMessage = new ScoreMessage(BigInt(data))
}
