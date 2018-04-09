package com

import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils.forceStopApplication
import scorex.block.Block
import scorex.transaction.BlockchainUpdater
import scorex.utils.ScorexLogging

package object wavesplatform extends ScorexLogging {
  def checkGenesis(blockchain: Blockchain, settings: WavesSettings, blockchainUpdater: BlockchainUpdater): Unit = if (blockchain.isEmpty) {
    Block.genesis(settings.blockchainSettings.genesisSettings).flatMap(blockchainUpdater.processBlock).left.foreach { value =>
      log.error(value.toString)
      forceStopApplication()
    }
    log.info("Genesis block has been added to the state")
  }
}
