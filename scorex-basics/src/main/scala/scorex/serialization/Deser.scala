package scorex.serialization

import scala.util.Try

/**
  * Interface for objects, that can deserialize bytes to instance of T
  */
trait Deser[T] {

  def parseBytes(bytes: Array[Byte]): Try[T]

}
