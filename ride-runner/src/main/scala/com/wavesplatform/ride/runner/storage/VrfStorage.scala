package com.wavesplatform.ride.runner.storage

import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.stats.KamonCaffeineStats
import com.wavesplatform.ride.runner.storage.StorageContext.ReadWrite
import com.wavesplatform.ride.runner.storage.persistent.VrfPersistentCache
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

// TODO #15 Metrics, better work with VRF?
class VrfStorage(settings: ExactWithHeightStorage.Settings, blockchainApi: BlockchainApi, persistentCache: VrfPersistentCache, currHeight: => Int)
    extends ScorexLogging {
  protected val values = Caffeine
    .newBuilder()
    .softValues()
    .maximumSize(settings.maxEntries)
    .recordStats(() => new KamonCaffeineStats("VrfStorage"))
    .build[Int, Option[ByteStr]]()

  def get(atHeight: Int)(implicit ctx: ReadWrite): Option[ByteStr] =
    if (atHeight > currHeight) throw new RuntimeException(s"Can't receive a block VRF with height=$atHeight > current height=$currHeight")
    else if (atHeight < 1) None
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
