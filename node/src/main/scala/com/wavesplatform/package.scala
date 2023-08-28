package com

import java.time.Instant

import com.typesafe.scalalogging.Logger
import com.wavesplatform.block.Block
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.transaction.TxValidationError.GenericError
import org.slf4j.LoggerFactory

package object wavesplatform {
  private lazy val logger: Logger =
    Logger(LoggerFactory.getLogger(getClass.getName))
  private def checkOrAppend(block: Block, blockchainUpdater: Blockchain & BlockchainUpdater, miner: Miner): Either[ValidationError, Unit] =
    if (blockchainUpdater.isEmpty) {
      blockchainUpdater.processBlock(block, block.header.generationSignature, None).map { _ =>
        val genesisHeader = blockchainUpdater.blockHeader(1).get
        logger.info(
          s"Genesis block ${genesisHeader.id()} (generated at ${Instant.ofEpochMilli(genesisHeader.header.timestamp)}) has been added to the state"
        )
      }
    } else
      blockchainUpdater.blockHeader(1).map(_.id()) match {
        case Some(id) if id == block.id() =>
          miner.scheduleMining()
          Right(())
        case _ =>
          Left(GenericError("Mismatched genesis blocks in configuration and blockchain"))
      }

  def checkGenesis(settings: WavesSettings, blockchainUpdater: Blockchain & BlockchainUpdater, miner: Miner): Unit = {
    Block
      .genesis(
        settings.blockchainSettings.genesisSettings,
        blockchainUpdater.isFeatureActivated(BlockchainFeatures.RideV6),
        blockchainUpdater.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot)
      )
      .flatMap { genesis =>
        logger.trace(s"Genesis block json: ${genesis.json()}")
        checkOrAppend(genesis, blockchainUpdater, miner)
      }
      .left
      .foreach { e =>
        logger.error("INCORRECT NODE CONFIGURATION!!! NODE STOPPED BECAUSE OF THE FOLLOWING ERROR:")
        logger.error(e.toString)
        com.wavesplatform.utils.forceStopApplication()
      }
  }
}
