package com.wavesplatform.ride.runner.caches.mem

import com.github.benmanes.caffeine.cache.Caffeine
import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.caches.mem.MemBlockchainDataCache.Settings
import com.wavesplatform.ride.runner.stats.KamonCaffeineStats
import org.github.jamm.MemoryMeter
import pureconfig.ConfigReader

class MemBlockchainDataCache(settings: Settings) {
  private val mmeter = MemoryMeter.builder().build()
  private val backend = Caffeine
    .newBuilder()
    .softValues()
    .maximumWeight(settings.size.toBytes)
    .recordStats(() => new KamonCaffeineStats("Common"))
    .weigher { (key: MemCacheKey[?], value: RemoteData[Any]) =>
      (mmeter.measure(key) + mmeter.measure(value)).toInt
    }
    .build[MemCacheKey[?], RemoteData[Any]]()

  def get[V, K <: MemCacheKey[V]](key: K): RemoteData[V] =
    Option(backend.getIfPresent(key)).getOrElse(RemoteData.Unknown).asInstanceOf[RemoteData[V]]

  def getOrLoad[V, K <: MemCacheKey[V]](key: K)(load: K => RemoteData[V]): RemoteData[V] =
    backend.get(key, _ => load(key)).asInstanceOf[RemoteData[V]]

  def set[V, K <: MemCacheKey[V]](key: K, value: RemoteData[V]): Unit = backend.put(key, value)

  def updateIfExists[V, K <: MemCacheKey[V]](key: K, newValue: RemoteData[V]): Unit =
    Option(backend.policy().getEntryIfPresentQuietly(key)).foreach { _ =>
      backend.put(key, newValue)
    }

  def remove[K <: MemCacheKey[?]](key: K): Unit = backend.invalidate(key)
}

object MemBlockchainDataCache {
  case class Settings(size: ConfigMemorySize) derives ConfigReader
}
