package scorex.serialization

import com.google.common.primitives.Shorts
import monix.eval.Coeval

trait BytesSerializable {

  val bytes: Coeval[Array[Byte]]
}

object BytesSerializable {
  def arrayWithSize(b: Array[Byte]): Array[Byte] = Shorts.toByteArray(b.length.toShort) ++ b
}