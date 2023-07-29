package com.wavesplatform.ride.runner.storage

import com.github.benmanes.caffeine.cache.Caffeine
import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.ride.runner.stats.KamonCaffeineStats
import com.wavesplatform.ride.runner.storage.BlockchainDataCache.Settings

// TODO InMem cache?
class BlockchainDataCache(settings: Settings) {
  private val backend = Caffeine
    .newBuilder()
    .softValues()
    .maximumWeight(settings.size.toBytes)
    .recordStats(() => new KamonCaffeineStats("Common"))
    .weigher { (key: CacheKey, value: RemoteData[Any]) =>
      val v = value.mayBeValue.map(_.asInstanceOf[key.ValueT])
      key.keyWeight + v.fold(0) { v =>
        CacheWeights.OfCachedRemoteDataOverhead + key.valueWeight(v)
      }
    }
    .build[CacheKey, RemoteData[Any]]()

  def get[T <: CacheKey](key: T): RemoteData[T#ValueT] =
    Option(backend.getIfPresent(key)).getOrElse(RemoteData.Unknown).asInstanceOf[RemoteData[T#ValueT]]

  def getOrLoad[T <: CacheKey](key: T)(load: T => RemoteData[T#ValueT]): RemoteData[T#ValueT] =
    backend.get(key, _ => load(key)).asInstanceOf[RemoteData[T#ValueT]]

  def set[T <: CacheKey, V](key: T, value: RemoteData[V])(implicit ev: V =:= T#ValueT): Unit = backend.put(key, value)

  def remove[T <: CacheKey](key: T): Unit = backend.invalidate(key)
}

object BlockchainDataCache {
  case class Settings(size: ConfigMemorySize)
}
