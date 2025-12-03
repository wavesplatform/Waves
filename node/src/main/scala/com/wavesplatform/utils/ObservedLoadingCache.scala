package com.wavesplatform.utils

import com.google.common.cache.{ForwardingLoadingCache, LoadingCache}
import monix.reactive.Observer

import scala.reflect.ClassTag

class ObservedLoadingCache[K, V](override val delegate: LoadingCache[K, V], changed: Observer[K])(implicit ct: ClassTag[K])
    extends ForwardingLoadingCache[K, V] {

  override def refresh(key: K): Unit = {
    super.refresh(key)
    changed.onNext(key)
  }

  override def put(key: K, value: V): Unit = {
    super.put(key, value)
    changed.onNext(key)
  }

  override def putAll(m: java.util.Map[? <: K, ? <: V]): Unit = {
    super.putAll(m)
    m.keySet().forEach(k => changed.onNext(k))
  }

  override def invalidate(key: Any): Unit = {
    super.invalidate(key)
    onNext(key)
  }

  override def invalidateAll(keys: java.lang.Iterable[?]): Unit = {
    super.invalidateAll(keys)
    keys.forEach(onNext)
  }

  private def onNext(key: Any): Unit = key match {
    case k: K => changed.onNext(k)
    case _    =>
  }
}
