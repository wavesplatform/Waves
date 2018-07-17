package com

import com.wavesplatform.block.Block
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.NG
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.utils.ScorexLogging

package object wavesplatform extends ScorexLogging {
  def checkGenesis(settings: WavesSettings, blockchainUpdater: BlockchainUpdater with NG): Unit = if (blockchainUpdater.isEmpty) {
    Block.genesis(settings.blockchainSettings.genesisSettings).flatMap(blockchainUpdater.processBlock).left.foreach { value =>
      log.error(value.toString)
      com.wavesplatform.utils.forceStopApplication()
    }
    log.info(s"Genesis block ${blockchainUpdater.blockHeaderAndSize(1).get._1} has been added to the state")
  }
}
