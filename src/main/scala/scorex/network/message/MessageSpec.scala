package scorex.network.message

import scala.util.Try

trait MessageSpec[Content <: AnyRef] {
  val messageCode: Message.MessageCode
  val messageName: String

  def maxLength: Int

  def deserializeData(bytes: Array[Byte]): Try[Content]

  def serializeData(data: Content): Array[Byte]

  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}
