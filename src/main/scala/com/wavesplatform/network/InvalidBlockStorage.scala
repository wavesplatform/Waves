package com.wavesplatform.network

import com.google.common.cache.CacheBuilder
import com.wavesplatform.state2.ByteStr

import scala.concurrent.duration.FiniteDuration
import InvalidBlockStorageImpl._

trait InvalidBlockStorage {
  def add(blockId: ByteStr): Unit

  def contains(blockId: ByteStr): Boolean
}

object InvalidBlockStorage {

  object Empty extends InvalidBlockStorage {
    override def add(blockId: ByteStr): Unit = {}

    override def contains(blockId: ByteStr): Boolean = false
  }

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
