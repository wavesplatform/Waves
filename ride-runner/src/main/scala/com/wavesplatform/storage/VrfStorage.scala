package com.wavesplatform.storage

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.storage.persistent.VrfPersistentCache
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

class VrfStorage(blockchainApi: BlockchainGrpcApi, persistentCache: VrfPersistentCache, currHeight: => Int) extends ScorexLogging {
  // TODO LRU cache

  // TODO if height > current
  def get(height: Int): Option[ByteStr] =
    if (height > currHeight) throw new RuntimeException(s"Can't receive a block VRF with height=$height > current height=$currHeight")
    else {
      val cached = persistentCache.get(height)
      if (cached.loaded) cached.mayBeValue
      else
        blockchainApi.getVrf(height).tap { x =>
          set(height, x)
          log.debug(s"Set VRF at $height: $x")
        }
    }

  def set(height: Int, vrf: Option[ByteStr]): Unit = persistentCache.set(height, vrf)

  def removeFrom(height: Int): Unit = persistentCache.removeFrom(height)
}
