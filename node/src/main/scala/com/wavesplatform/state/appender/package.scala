package com.wavesplatform.state

import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.settings.SynchronizationSettings
import com.wavesplatform.transaction.TxValidationError.{BlockFromFuture, GenericError}
import com.wavesplatform.transaction._
import com.wavesplatform.utils
import com.wavesplatform.utils.{BaseTargetReachedMaximum, ScorexLogging, Time}
import com.wavesplatform.utx.UtxPoolImpl
import kamon.Kamon

import scala.util.{Left, Right}

package object appender extends ScorexLogging {

  val MaxTimeDrift: Long = 100 // millis

  private[appender] def appendBlock(
      blockchainUpdater: BlockchainUpdater with Blockchain,
      utxStorage: UtxPoolImpl,
      time: Time,
      syncSettings: SynchronizationSettings,
      verify: Boolean = true
  )(block: Block): Either[ValidationError, Option[Int]] =
    for {
      hitSource  <- validateConsensusData(blockchainUpdater, time.correctedTime(), block, syncSettings, verify)
      baseHeight <- appendBlock(blockchainUpdater, utxStorage, syncSettings, verify)(block, hitSource)
    } yield baseHeight

  private def appendBlock(
      blockchainUpdater: BlockchainUpdater with Blockchain,
      utxStorage: UtxPoolImpl,
      syncSettings: SynchronizationSettings,
      verify: Boolean
  )(
      block: Block,
      hitSource: ByteStr
  ): Either[ValidationError, Option[Int]] =
    metrics.appendBlock.measureSuccessful(blockchainUpdater.processBlock(block, hitSource, verify)).map { discDiffs =>
      for (maxBaseTarget <- syncSettings.maxBaseTargetOpt
           if blockchainUpdater.isFeatureActivated(BlockchainFeatures.FairPoS) &&
             blockchainUpdater.lastBlockHeader.exists(_.header.baseTarget > maxBaseTarget)) {}
      metrics.utxRemoveAll.measure(utxStorage.removeAll(block.transactionData))
      metrics.utxDiscardedPut.measure(utxStorage.addAndCleanupPriority(discDiffs))
      Some(blockchainUpdater.height)
    }

  private def validateConsensusData(
      blockchain: Blockchain,
      currentTs: Long,
      block: Block,
      settings: SynchronizationSettings,
      verify: Boolean
  ): Either[ValidationError, ByteStr] =
    metrics.blockConsensusValidation
      .measureSuccessful {
        import block.header.{timestamp => blockTime}
        val pos = PoSSelector(blockchain)

        for {
          height <- blockchain
            .heightOf(block.header.reference)
            .toRight(GenericError(s"Could not determine height of ${block.header.reference}"))
          parent <- blockchain.parentHeader(block.header).toRight(GenericError(s"Could not load header of ${block.header.reference}"))
          grandParent = blockchain.parentHeader(parent, 2)
          _ <- validateBlockVersion(height, block, blockchain)
          _ <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime))
          hitSource <- if (!verify) Right(block.header.generationSignature)
          else
            for {
              _  <- pos.validateBaseTarget(height, block, parent, grandParent)
              _  <- validateBlockDelay(blockchain, block, parent)
              hs <- pos.validateGenerationSignature(block)
            } yield hs
          _ <- validateBaseTargetLimit(blockchain, settings, block.header.baseTarget)
        } yield hitSource
      }
      .left
      .map {
        case GenericError(x) => GenericError(s"Block ${block.id()} is invalid. $x")
        case x               => x
      }

  private def validateBaseTargetLimit(blockchain: Blockchain, settings: SynchronizationSettings, actualBaseTarget: Long) =
    settings.maxBaseTargetOpt match {
      case Some(maxBaseTarget) if blockchain.isFeatureActivated(BlockchainFeatures.FairPoS) && actualBaseTarget > maxBaseTarget =>
        log.error(
          s"""Base target reached maximum value (settings: synchronization.max-base-target=${settings.maxBaseTargetOpt.get}). Anti-fork protection.
                   |FOR THIS REASON THE NODE WAS STOPPED AUTOMATICALLY""".stripMargin
        )
        utils.forceStopApplication(BaseTargetReachedMaximum)
        Left(GenericError(s"Base target $actualBaseTarget exceeds max base target $maxBaseTarget"))
      case _ => Right(())
    }

  private def validateBlockDelay(blockchain: Blockchain, block: Block, parent: BlockHeader): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(
        !blockchain.hasAccountScript(block.header.generator.toAddress),
        (),
        GenericError(s"Account(${block.header.generator.toAddress}) is scripted are therefore not allowed to forge blocks")
      )
      effectiveBalance = blockchain.generatingBalance(block.header.generator.toAddress, Some(block.header.reference))
      _ <- Either.cond(
        blockchain.isEffectiveBalanceValid(blockchain.height, block.header.timestamp, effectiveBalance),
        (),
        GenericError(s"generator's ${block.header.generator.toAddress} effective balance $effectiveBalance is less that required for generation")
      )
      _ <- PoSSelector(blockchain)
        .validateBlockDelay(blockchain.height, block.header, parent, effectiveBalance)
        .orElse(checkExceptions(blockchain.height, block))
    } yield ()

  // Invalid blocks, that are already in blockchain
  private val exceptions = List(
    812608 -> ByteStr.decodeBase58("2GNCYVy7k3kEPXzz12saMtRDeXFKr8cymVsG8Yxx3sZZ75eHj9csfXnGHuuJe7XawbcwjKdifUrV1uMq4ZNCWPf1").get,
    813207 -> ByteStr.decodeBase58("5uZoDnRKeWZV9Thu2nvJVZ5dBvPB7k2gvpzFD618FMXCbBVBMN2rRyvKBZBhAGnGdgeh2LXEeSr9bJqruJxngsE7").get
  )

  private def checkExceptions(height: Int, block: Block): Either[ValidationError, Unit] =
    Either
      .cond(
        exceptions.contains((height, block.id())),
        (),
        GenericError(s"Block time ${block.header.timestamp} less than expected")
      )

  private def validateBlockVersion(parentHeight: Int, block: Block, blockchain: Blockchain): Either[ValidationError, Unit] =
    Either.cond(
      blockchain.blockVersionAt(parentHeight + 1) == block.header.version,
      (),
      GenericError(s"Block version should be equal to ${blockchain.blockVersionAt(parentHeight + 1)}")
    )

  private[this] object metrics {
    val blockConsensusValidation = Kamon.timer("block-appender.block-consensus-validation").withoutTags()
    val appendBlock              = Kamon.timer("block-appender.blockchain-append-block").withoutTags()
    val utxRemoveAll             = Kamon.timer("block-appender.utx-remove-all").withoutTags()
    val utxDiscardedPut          = Kamon.timer("block-appender.utx-discarded-put").withoutTags()
  }
}
