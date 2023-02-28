package com.wavesplatform.riderunner.storage

import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite
import com.wavesplatform.riderunner.storage.persistent.VrfPersistentCache
import com.wavesplatform.utils.ScorexLogging
import kamon.instrumentation.caffeine.KamonStatsCounter

import scala.util.chaining.scalaUtilChainingOps

class VrfStorage(settings: ExactWithHeightStorage.Settings, blockchainApi: BlockchainApi, persistentCache: VrfPersistentCache, currHeight: => Int)
    extends ScorexLogging {
  protected val values = Caffeine
    .newBuilder()
    .softValues()
    .maximumSize(settings.maxEntries)
    .recordStats(() => new KamonStatsCounter("VrfStorage"))
    .build[Int, Option[ByteStr]]()

  def get(atHeight: Int)(implicit ctx: ReadWrite): Option[ByteStr] =
    if (atHeight > currHeight) throw new RuntimeException(s"Can't receive a block VRF with height=$atHeight > current height=$currHeight")
    else load(atHeight).tap(values.put(atHeight, _))

  private def load(atHeight: Int)(implicit ctx: ReadWrite): Option[ByteStr] = {
    val cached = persistentCache.get(atHeight)
    if (cached.loaded) cached.mayBeValue
    else
      blockchainApi.getVrf(atHeight).tap { x =>
        persistentCache.set(atHeight, x)
        log.trace(s"Set VRF at $atHeight: $x")
      }
  }

  def remove(heights: Range)(implicit ctx: ReadWrite): Unit = {
    persistentCache.removeFrom(heights.start)
    heights.foreach(values.invalidate)
  }
}
