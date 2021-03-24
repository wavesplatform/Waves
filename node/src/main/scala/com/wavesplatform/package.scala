package com

import java.time.Instant

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.ScorexLogging

package object wavesplatform extends ScorexLogging {
  private def checkOrAppend(block: Block, blockchainUpdater: Blockchain with BlockchainUpdater): Either[ValidationError, Unit] = {
    if (blockchainUpdater.isEmpty) {
      blockchainUpdater.processBlock(block, block.header.generationSignature).map { _ =>
        val genesisHeader = blockchainUpdater.blockHeader(1).get
        log.info(s"Genesis block ${genesisHeader.id()} (generated at ${Instant.ofEpochMilli(genesisHeader.header.timestamp)}) has been added to the state")
      }
    } else {
      val existingGenesisBlockId: Option[ByteStr] = blockchainUpdater.blockHeader(1).map(_.id())
      Either.cond(existingGenesisBlockId.fold(false)(_ == block.id()),
                  (),
                  GenericError("Mismatched genesis blocks in configuration and blockchain"))
    }
  }

  def checkGenesis(settings: WavesSettings, blockchainUpdater: Blockchain with BlockchainUpdater): Unit = {
    Block
      .genesis(settings.blockchainSettings.genesisSettings)
      .flatMap { genesis =>
        log.trace(s"Genesis block json: ${genesis.json()}")
        checkOrAppend(genesis, blockchainUpdater)
      }
      .left
      .foreach { e =>
        log.error("INCORRECT NODE CONFIGURATION!!! NODE STOPPED BECAUSE OF THE FOLLOWING ERROR:")
        log.error(e.toString)
        com.wavesplatform.utils.forceStopApplication()
      }
  }
}
