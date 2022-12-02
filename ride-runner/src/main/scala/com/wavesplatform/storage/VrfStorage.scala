package com.wavesplatform.storage

import com.github.benmanes.caffeine.cache.{CacheLoader, Caffeine}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.grpc.BlockchainApi
import com.wavesplatform.storage.persistent.VrfPersistentCache
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

class VrfStorage(blockchainApi: BlockchainApi, persistentCache: VrfPersistentCache, currHeight: => Int) extends ScorexLogging {
  protected val values = Caffeine.newBuilder().maximumSize(100).build[Int, Option[ByteStr]] {
    new CacheLoader[Int, Option[ByteStr]] {
      override def load(height: Int): Option[ByteStr] = {
        val cached = persistentCache.get(height)
        if (cached.loaded) cached.mayBeValue
        else
          blockchainApi.getVrf(height).tap { x =>
            persistentCache.set(height, x)
            log.trace(s"Set VRF at $height: $x")
          }
      }
    }
  }

  def get(height: Int): Option[ByteStr] =
    if (height > currHeight) throw new RuntimeException(s"Can't receive a block VRF with height=$height > current height=$currHeight")
    else values.get(height)

  def removeFrom(height: Int): Unit = {
    persistentCache.removeFrom(height)
    values.invalidate(height)
  }
}
