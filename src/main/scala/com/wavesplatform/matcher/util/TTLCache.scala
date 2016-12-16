package com.wavesplatform.matcher.util


import com.google.common.cache.{CacheBuilder, Cache => GuavaCache}
import scala.concurrent.duration._

object TTLCache {

  def apply[K <: AnyRef, V <: AnyRef](implicit duration: Duration) = {
    val ttlCache: GuavaCache[K, V] =
      CacheBuilder
        .newBuilder()
        .expireAfterWrite(duration.length, duration.unit)
        .build()
    new Cache(ttlCache)
  }
}
