package com.wavesplatform.utils

import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.database.extensions.impl.LevelDBApiExtensions
import com.wavesplatform.state.extensions.ApiExtensions
import com.wavesplatform.state.extensions.impl.CompositeApiExtensions
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl}

object Implicits {
  implicit def blockchainToApiExtensions(b: Blockchain): ApiExtensions = b match {
    case ldb: LevelDBWriter => new LevelDBApiExtensions(ldb)
    case cb: BlockchainUpdaterImpl => new CompositeApiExtensions(cb, new LevelDBApiExtensions(cb.stableBlockchain), () => cb.bestLiquidDiff)
    case _ =>
  }
}
