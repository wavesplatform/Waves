package com.wavesplatform.riderunner.storage

import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.stats.KamonCaffeineStatsCounter
import kamon.instrumentation.caffeine.KamonStatsCounter

import java.util.concurrent.ConcurrentHashMap

// TODO start with simple code with a lot of duplication
//class KeyValueStorage[KeyT, ValueT](name: String) {
//  protected val keyIndexes = new ConcurrentHashMap[KeyT, Int]()
//
//  protected val values = Caffeine
//    .newBuilder()
//    .recordStats(() => new KamonCaffeineStatsCounter(s"$name"))
//    .softValues()
//    .build[KeyT, ValueT]()
//
//  def get(key: KeyT): Option[ValueT] = ???
//  def set(key: KeyT)
//}
