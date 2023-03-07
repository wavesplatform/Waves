package com.wavesplatform.riderunner.storage

import com.wavesplatform.riderunner.storage.StorageContext.{ReadOnly, ReadWrite}
import com.wavesplatform.riderunner.storage.persistent.CacheKey

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

class KeyIndexStorage[KeyT] private (
    cacheKey: CacheKey[(KeyT, DbKeyIndex), Unit],
    lastIndex: AtomicInteger,
    indexes: ConcurrentHashMap[KeyT, DbKeyIndex]
) {
  def getIndexOf(key: KeyT): Option[DbKeyIndex] = Option(indexes.get(key))

  def getOrMkIndexOf(key: KeyT)(implicit ctx: ReadWrite): DbKeyIndex = getIndexOf(key).getOrElse {
    val nextIndex = DbKeyIndex(lastIndex.incrementAndGet())
    Option(indexes.putIfAbsent(key, nextIndex)).getOrElse {
      ctx.db.put(cacheKey.mkKey((key, nextIndex)), ())
      nextIndex
    }
  }
}

object KeyIndexStorage {
  def apply[KeyT](cacheKey: CacheKey[(KeyT, DbKeyIndex), Unit])(implicit ctx: ReadOnly): KeyIndexStorage[KeyT] = {
    var lastIndex = -1
    val indexes   = new ConcurrentHashMap[KeyT, DbKeyIndex]()
    ctx.db.iterateOverPrefix(cacheKey.prefixBytes) { dbEntry =>
      val (key, index) = cacheKey.parseKey(dbEntry.getKey)
      lastIndex = math.max(lastIndex, index)
      indexes.put(key, index)
    }

    new KeyIndexStorage[KeyT](cacheKey, new AtomicInteger(lastIndex), indexes)
  }
}
