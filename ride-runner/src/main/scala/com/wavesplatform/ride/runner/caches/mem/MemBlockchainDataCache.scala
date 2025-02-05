package com.wavesplatform.ride.runner.caches.mem

import com.github.benmanes.caffeine.cache.Caffeine
import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.caches.mem.MemBlockchainDataCache.Settings
import com.wavesplatform.ride.runner.stats.KamonCaffeineStats

class MemBlockchainDataCache(settings: Settings) {
  private val backend = Caffeine
    .newBuilder()
    .softValues()
    .maximumWeight(settings.size.toBytes)
    .recordStats(() => new KamonCaffeineStats("Common"))
    .weigher { (key: MemCacheKey, value: RemoteData[Any]) =>
      val v = value.mayBeValue.map(_.asInstanceOf[key.ValueT])
      key.keyWeight + v.fold(0) { v =>
        MemCacheWeights.OfCachedRemoteDataOverhead + key.valueWeight(v)
      }
    }
    .build[MemCacheKey, RemoteData[Any]]()

  def get[T <: MemCacheKey](key: T): RemoteData[T#ValueT] =
    Option(backend.getIfPresent(key)).getOrElse(RemoteData.Unknown).asInstanceOf[RemoteData[T#ValueT]]

  def getOrLoad[T <: MemCacheKey](key: T)(load: T => RemoteData[T#ValueT]): RemoteData[T#ValueT] =
    backend.get(key, _ => load(key)).asInstanceOf[RemoteData[T#ValueT]]

  def set[T <: MemCacheKey, V](key: T, value: RemoteData[V])(implicit ev: V =:= T#ValueT): Unit = backend.put(key, value)

  def updateIfExists[T <: MemCacheKey, V](key: T, newValue: RemoteData[V])(implicit ev: V =:= T#ValueT): Unit =
    Option(backend.policy().getEntryIfPresentQuietly(key)).foreach { _ =>
      backend.put(key, newValue)
    }

  def remove[T <: MemCacheKey](key: T): Unit = backend.invalidate(key)
}

object MemBlockchainDataCache {
  case class Settings(size: ConfigMemorySize)
}
