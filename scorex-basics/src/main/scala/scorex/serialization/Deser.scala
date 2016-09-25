package scorex.serialization

import com.google.common.primitives.Ints
import scorex.crypto.EllipticCurveImpl._

import scala.util.Try

/**
  * Interface for objects, that can deserialize bytes to instance of T
  */
trait Deser[T] {

  def parseBytes(bytes: Array[Byte]): Try[T]

  protected def parseArraySize(bytes: Array[Byte], position: Int): (Array[Byte], Int) = {
    val length = Ints.fromByteArray(bytes.slice(position, position + 4))
    (bytes.slice(position + 4, position + 4 + length), position + 4 + length)
  }

  protected def parseOption(bytes: Array[Byte], position: Int, length: Int): (Option[Array[Byte]], Int) = {
    if (bytes.slice(position, position + 1).head == (1: Byte)) {
      val b = bytes.slice(position + 1, position + 1 + length)
      (Some(b), position + 1 + length)
    } else (None, position + 1)
  }

}
