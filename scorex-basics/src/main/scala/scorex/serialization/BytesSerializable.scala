package scorex.serialization

import com.google.common.primitives.Shorts

trait BytesSerializable extends Serializable {

  def bytes: Array[Byte]

  protected def arrayWithSize(b: Array[Byte]): Array[Byte] = Shorts.toByteArray(b.length.toShort) ++ b
}
