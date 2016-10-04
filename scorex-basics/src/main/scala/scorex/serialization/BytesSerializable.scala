package scorex.serialization

import com.google.common.primitives.Ints

trait BytesSerializable extends Serializable {

  def bytes: Array[Byte]

  protected def arrayWithSize(b: Array[Byte]): Array[Byte] = Ints.toByteArray(b.length) ++ b
}
