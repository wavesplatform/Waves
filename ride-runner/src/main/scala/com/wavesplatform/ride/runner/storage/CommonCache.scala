package com.wavesplatform.ride.runner.storage

import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.ride.runner.stats.KamonCaffeineStats

trait TypedCache {
  // TODO T in TypedCache?
  def get[T <: CacheKey](key: T): RemoteData[T#ValueT]
  def getOrLoad[T <: CacheKey](key: T)(load: T => RemoteData[T#ValueT]): RemoteData[T#ValueT]
  def set[T <: CacheKey, V](key: T, value: RemoteData[V])(implicit ev: V =:= T#ValueT): Unit
  def remove[T <: CacheKey](key: T): Unit
}

class CommonCache extends TypedCache {
  private val backend = Caffeine
    .newBuilder()
    .softValues()
    .maximumSize(
      30000
    ) // TODO weigher: https://gitlab.wvservices.com/waves/node/-/blob/rocksdb/node/src/main/scala/com/wavesplatform/database/Caches.scala
    .recordStats(() => new KamonCaffeineStats("Common"))
    .build[CacheKey, RemoteData[Any]]()

  override def get[T <: CacheKey](key: T): RemoteData[T#ValueT] =
    Option(backend.getIfPresent(key)).getOrElse(RemoteData.Unknown).asInstanceOf[RemoteData[T#ValueT]]

  override def getOrLoad[T <: CacheKey](key: T)(load: T => RemoteData[T#ValueT]): RemoteData[T#ValueT] =
    backend.get(key, _ => load(key)).asInstanceOf[RemoteData[T#ValueT]]

  override def set[T <: CacheKey, V](key: T, value: RemoteData[V])(implicit ev: V =:= T#ValueT): Unit = backend.put(key, value)

  override def remove[T <: CacheKey](key: T): Unit = backend.invalidate(key)
}
