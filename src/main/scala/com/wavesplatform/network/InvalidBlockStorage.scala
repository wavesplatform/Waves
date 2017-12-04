package com.wavesplatform.network

import com.google.common.cache.CacheBuilder
import com.wavesplatform.state2.ByteStr

import scala.concurrent.duration.FiniteDuration
import InvalidBlockStorageImpl._

trait InvalidBlockStorage {
  def add(blockId: ByteStr): Unit

  def contains(blockId: ByteStr): Boolean
}

class InMemoryInvalidBlockStorage extends InvalidBlockStorage {

  var s: Set[ByteStr] = Set.empty[ByteStr]

  override def add(blockId: ByteStr): Unit = s += blockId

  override def contains(blockId: ByteStr): Boolean = s.contains(blockId)

}

class InvalidBlockStorageImpl(settings: InvalidBlockStorageSettings) extends InvalidBlockStorage {
  private val dummy = new Object
  private val cache = CacheBuilder
    .newBuilder()
    .expireAfterWrite(settings.timeout.length, settings.timeout.unit)
    .build[ByteStr, Object]()

  override def add(blockId: ByteStr): Unit = cache.put(blockId, dummy)

  override def contains(blockId: ByteStr): Boolean = cache.getIfPresent(blockId) != null
}

object InvalidBlockStorageImpl {

  case class InvalidBlockStorageSettings(maxSize: Int,
                                         timeout: FiniteDuration)

}
