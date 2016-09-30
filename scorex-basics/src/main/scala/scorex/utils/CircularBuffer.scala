package scorex.utils

import scala.collection.mutable.ArrayBuffer

class CircularBuffer[A](val maxSize: Int) {
  private var curIndex = 0
  private[utils] var buffer = new ArrayBuffer[A](maxSize / 10)

  def +=(elem: A): this.type = {
    if (buffer.size < maxSize) {
      curIndex = 0
      buffer += elem
      this
    } else {
      buffer(curIndex) = elem
      curIndex = (curIndex + 1) % maxSize
      this
    }
  }

  def remove(elem: A): Option[A] = {
    val n = buffer.indexOf(elem)
    if (n >= 0) Some(buffer.remove(n)) else None
  }

  def drop(p: A => Boolean): Seq[A] = {
    val removed = buffer.filter(p)
    buffer = buffer.filterNot(p)
    removed
  }

  def apply(idx: Int): A = {
    if (idx >= buffer.size) throw new IndexOutOfBoundsException(idx.toString)
    buffer(idx)
  }

  def size(): Int = buffer.size

  def nonEmpty: Boolean = size != 0

}
