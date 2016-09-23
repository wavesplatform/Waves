package scorex.serialization

import com.google.common.primitives.Ints

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

}
