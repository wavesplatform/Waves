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
    Logger(LoggerFactory.getLogger(this.getClass.getName))
  private def configuredGenesis(settings: WavesSettings, blockchain: Blockchain): Either[ValidationError, Block] =
    Block.genesis(
      settings.blockchainSettings.genesisSettings,
      blockchain.isFeatureActivated(BlockchainFeatures.RideV6),
      blockchain.isFeatureActivated(BlockchainFeatures.LightNode)
    )

  private def matchesGenesis(genesis: Block, blockchain: Blockchain): Either[ValidationError, Unit] =
    blockchain.blockHeader(1).map(_.id()) match {
      case Some(id) if id == genesis.id() => Right(())
      case Some(_)                        => Left(GenericError("Mismatched genesis blocks in configuration and blockchain"))
      case None                           => Left(GenericError("Blockchain is empty"))
    }

  /** Read-only counterpart of [[checkGenesis]] for tools that must not write to the state. */
  def verifyGenesis(settings: WavesSettings, blockchain: Blockchain): Either[ValidationError, Unit] =
    configuredGenesis(settings, blockchain).flatMap(matchesGenesis(_, blockchain))

  private def checkOrAppend(block: Block, blockchainUpdater: Blockchain & BlockchainUpdater, miner: Miner): Either[ValidationError, Unit] =
    if (blockchainUpdater.isEmpty) {
      blockchainUpdater.processBlock(block, block.header.generationSignature, snapshot = None, generatorSet = Seq.empty).map { _ =>
        val genesisHeader = blockchainUpdater.blockHeader(1).get
        logger.info(
          s"Genesis block ${genesisHeader.id()} (generated at ${Instant.ofEpochMilli(genesisHeader.header.timestamp)}) has been added to the state"
        )
      }
    } else matchesGenesis(block, blockchainUpdater).map(_ => miner.scheduleMining())

  def checkGenesis(settings: WavesSettings, blockchainUpdater: Blockchain & BlockchainUpdater, miner: Miner): Unit = {
    configuredGenesis(settings, blockchainUpdater)
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
