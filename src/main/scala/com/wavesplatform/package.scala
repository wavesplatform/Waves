package com

import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.utils.forceStopApplication
import scorex.block.Block
import scorex.transaction.{BlockchainUpdater, History}
import scorex.utils.ScorexLogging

package object wavesplatform extends ScorexLogging {
  def checkGenesis(history: History, settings: WavesSettings, blockchainUpdater: BlockchainUpdater): Unit = if (history.isEmpty) {
    Block.genesis(settings.blockchainSettings.genesisSettings).flatMap(blockchainUpdater.processBlock).left.foreach { value =>
      log.error(value.toString)
      forceStopApplication()
    }
    log.info(s"Genesis block ${history.blockHeaderAndSize(1).get._1} has been added to the state")
  }
}
