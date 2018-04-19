package com

import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.NG
import com.wavesplatform.utils.forceStopApplication
import scorex.block.Block
import scorex.transaction.BlockchainUpdater
import scorex.utils.ScorexLogging

package object wavesplatform extends ScorexLogging {
  def checkGenesis(settings: WavesSettings, blockchainUpdater: BlockchainUpdater with NG): Unit = if (blockchainUpdater.isEmpty) {
    Block.genesis(settings.blockchainSettings.genesisSettings).flatMap(blockchainUpdater.processBlock).left.foreach { value =>
      log.error(value.toString)
      forceStopApplication()
    }
    log.info("Genesis block has been added to the state")
  }
}
