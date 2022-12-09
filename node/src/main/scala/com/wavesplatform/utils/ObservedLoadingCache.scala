package com.wavesplatform.utils

import com.github.benmanes.caffeine.cache.{LoadingCache, Policy}
import com.github.benmanes.caffeine.cache.stats.CacheStats
import monix.reactive.Observer

import java.{lang, util}
import java.util.concurrent.ConcurrentMap
import java.util.function
import scala.reflect.ClassTag

class ObservedLoadingCache[K, V](val delegate: LoadingCache[K, V], changed: Observer[K])(implicit ct: ClassTag[K]) extends LoadingCache[K, V] {

  override def get(key: K): V = delegate.get(key)

  override def get(key: K, mappingFunction: function.Function[? >: K, ? <: V]): V = delegate.get(key, mappingFunction)

  override def getAll(keys: lang.Iterable[? <: K]): util.Map[K, V] = delegate.getAll(keys)

  override def getAllPresent(keys: lang.Iterable[?]): util.Map[K, V] = delegate.getAllPresent(keys)

  override def getIfPresent(key: Any): V = delegate.getIfPresent(key)

  override def refresh(key: K): Unit = {
    delegate.refresh(key)
    changed.onNext(key)
  }

  override def put(key: K, value: V): Unit = {
    delegate.put(key, value)
    changed.onNext(key)
  }

  override def putAll(m: java.util.Map[? <: K, ? <: V]): Unit = {
    delegate.putAll(m)
    m.keySet().forEach(k => changed.onNext(k))
  }

  override def invalidate(key: Any): Unit = {
    delegate.invalidate(key)
    onNext(key)
  }

  override def invalidateAll(keys: java.lang.Iterable[?]): Unit = {
    delegate.invalidateAll(keys)
    keys.forEach(onNext)
  }

  override def asMap(): ConcurrentMap[K, V] = delegate.asMap()

  override def estimatedSize(): Long = delegate.estimatedSize()

  override def stats(): CacheStats = delegate.stats()

  override def cleanUp(): Unit = delegate.cleanUp()

  override def policy(): Policy[K, V] = delegate.policy()

  override def invalidateAll(): Unit = delegate.invalidateAll()

  private def onNext(key: Any): Unit = key match {
    case k: K => changed.onNext(k)
    case _    =>
  }
}
