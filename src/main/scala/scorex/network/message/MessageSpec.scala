package scorex.network.message

import scala.reflect.ClassTag
import scala.util.Try

abstract class MessageSpec[Content <: AnyRef](implicit contentCt: ClassTag[Content]) {
  val contentClass: Class[_] = contentCt.runtimeClass
  val messageCode: Message.MessageCode
  val messageName: String

  def maxLength: Int

  def deserializeData(bytes: Array[Byte]): Try[Content]

  def serializeData(data: Content): Array[Byte]

  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}
