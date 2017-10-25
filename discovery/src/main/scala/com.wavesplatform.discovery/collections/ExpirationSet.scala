package com.wavesplatform.discovery.collections

class ExpirationSet[T](expirationTimeMilis: Long) extends scala.collection.mutable.Set[T]{
  private var inner = Map.empty[T, Long]
  private def freshInner = {
    val time = System.currentTimeMillis()
    inner = inner.filter({ case (k, v) =>
      time - v > expirationTimeMilis })

    inner
  }

  override def +=(elem: T): ExpirationSet.this.type = {
    inner += ((elem, System.currentTimeMillis()))
    this
  }

  override def -=(elem: T): ExpirationSet.this.type = {
    inner = inner.-(elem)
    this
  }

  override def contains(elem: T): Boolean = freshInner.contains(elem)

  override def iterator: Iterator[T] = freshInner.keys.iterator
}
