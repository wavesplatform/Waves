package com.wavesplatform.discovery.collections

import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._

import com.google.common.cache.CacheBuilder

class ExpirationSet[T <: Object](expirationTimeMilis: Long) extends scala.collection.mutable.Set[T] {
  private val emptyItem = new Object()

  private var inner = CacheBuilder
    .newBuilder()
    .expireAfterWrite(expirationTimeMilis, TimeUnit.MILLISECONDS)
    .build[T, Object]()

  override def +=(elem: T): ExpirationSet.this.type = {
    inner.put(elem, emptyItem)
    this
  }

  override def -=(elem: T): ExpirationSet.this.type = {
    inner.invalidate(elem)
    this
  }

  override def contains(elem: T): Boolean = inner.asMap().containsKey(elem)

  override def iterator: Iterator[T] = inner.asMap().keySet().iterator().asScala
}
