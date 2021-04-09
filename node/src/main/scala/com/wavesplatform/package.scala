package com

import java.time.Instant

import com.wavesplatform.block.Block
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.ScorexLogging

package object wavesplatform extends ScorexLogging {
  private def checkOrAppend(block: Block, blockchainUpdater: Blockchain with BlockchainUpdater, miner: Miner): Either[ValidationError, Unit] =
    if (blockchainUpdater.isEmpty) {
      blockchainUpdater.processBlock(block, block.header.generationSignature).map { _ =>
        val genesisHeader = blockchainUpdater.blockHeader(1).get
        log.info(s"Genesis block ${genesisHeader.id()} (generated at ${Instant.ofEpochMilli(genesisHeader.header.timestamp)}) has been added to the state")
      }
    } else blockchainUpdater.blockHeader(1).map(_.id()) match {
      case Some(id) if id == block.id() =>
        miner.scheduleMining()
        Right(())
      case _ =>
        Left(GenericError("Mismatched genesis blocks in configuration and blockchain"))
    }

  def checkGenesis(settings: WavesSettings, blockchainUpdater: Blockchain with BlockchainUpdater, miner: Miner): Unit = {
    Block
      .genesis(settings.blockchainSettings.genesisSettings)
      .flatMap { genesis =>
        log.trace(s"Genesis block json: ${genesis.json()}")
        checkOrAppend(genesis, blockchainUpdater, miner)
      }
      .left
      .foreach { e =>
        log.error("INCORRECT NODE CONFIGURATION!!! NODE STOPPED BECAUSE OF THE FOLLOWING ERROR:")
        log.error(e.toString)
        com.wavesplatform.utils.forceStopApplication()
      }
  }
}
