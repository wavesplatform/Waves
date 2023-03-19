package com.wavesplatform.ride.runner.storage

import com.wavesplatform.ride.runner.storage.persistent.PersistentStorageContext.{ReadOnly, ReadWrite}
import com.wavesplatform.ride.runner.storage.persistent.CacheKey

import java.lang
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

  def mkSet[KeyT](historyKey: CacheKey[KeyT, Seq[Int]])(implicit ctx: ReadOnly): ConcurrentHashMap.KeySetView[KeyT, lang.Boolean] = {
    val r = ConcurrentHashMap.newKeySet[KeyT]()
    ctx.db.iterateOverPrefix(historyKey.prefixBytes) { dbEntry =>
      val key = historyKey.parseKey(dbEntry.getKey)
      r.add(key)
    }
    r
  }

  def mkList[KeyT](historyKey: CacheKey[KeyT, Seq[Int]])(implicit ctx: ReadOnly): List[KeyT] = {
    var r = List.empty[KeyT]
    ctx.db.iterateOverPrefix(historyKey.prefixBytes) { dbEntry =>
      val key = historyKey.parseKey(dbEntry.getKey)
      r = key :: r
    }
    r
  }
}
