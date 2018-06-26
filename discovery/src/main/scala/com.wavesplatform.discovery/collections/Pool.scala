package com.wavesplatform.discovery.collections

class Pool[T] {
  private val queue = scala.collection.mutable.Queue.empty[T]
  private val items = scala.collection.mutable.Set.empty[T]

  def add(item: T): Unit = {
    if (!items.contains(item)) {
      items.add(item)
      queue.enqueue(item)
    }
  }

  def next(): Option[T] = {
    if (queue.nonEmpty) {
      val item = queue.dequeue()
      queue.enqueue(item)
      Some(item)
    } else None
  }

  def remove(item: T): Unit = {
    items.remove(item)
    queue.dequeueFirst(i => i == item)
  }
}
