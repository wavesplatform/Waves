package com.wavesplatform.state

import cats.syntax.either.*
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.*
import com.wavesplatform.mining.Miner
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, BlockFromFuture, GenericError}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxForAppender
import kamon.Kamon

package object appender {

  val MaxTimeDrift: Long = 100 // millis

  // Invalid blocks, that are already in blockchain
  private val exceptions = List(
    812608 -> ByteStr.decodeBase58("2GNCYVy7k3kEPXzz12saMtRDeXFKr8cymVsG8Yxx3sZZ75eHj9csfXnGHuuJe7XawbcwjKdifUrV1uMq4ZNCWPf1").get,
    813207 -> ByteStr.decodeBase58("5uZoDnRKeWZV9Thu2nvJVZ5dBvPB7k2gvpzFD618FMXCbBVBMN2rRyvKBZBhAGnGdgeh2LXEeSr9bJqruJxngsE7").get
  )

  private[appender] def appendKeyBlock(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      utx: UtxForAppender,
      pos: PoSSelector,
      time: Time,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block): Either[ValidationError, Option[Int]] =
    for {
      hitSource <- if (verify) validateBlock(blockchainUpdater, pos, time)(block) else pos.validateGenerationSignature(block)
      newHeight <-
        metrics.appendBlock
          .measureSuccessful(blockchainUpdater.processBlock(block, hitSource, None, verify, txSignParCheck))
          .map { discardedDiffs =>
            utx.setPrioritySnapshots(discardedDiffs)
            Some(blockchainUpdater.height)
          }

    } yield newHeight

  private[appender] def appendExtensionBlock(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      pos: PoSSelector,
      time: Time,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block): Either[ValidationError, Option[Int]] = {
    if (block.header.challengedHeader.nonEmpty) {
      processBlockWithChallenge(blockchainUpdater, pos, time, verify, txSignParCheck)(block).map(_._2)
    } else {
      for {
        hitSource <- if (verify) validateBlock(blockchainUpdater, pos, time)(block) else pos.validateGenerationSignature(block)
        _         <- metrics.appendBlock.measureSuccessful(blockchainUpdater.processBlock(block, hitSource, None, verify, txSignParCheck))
      } yield Some(blockchainUpdater.height)
    }
  }

  private[appender] def appendChallengeBlock(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      utx: UtxForAppender,
      pos: PoSSelector,
      time: Time,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block): Either[ValidationError, Option[Int]] =
    processBlockWithChallenge(blockchainUpdater, pos, time, verify, txSignParCheck)(block).map { case (discardedDiffs, newHeight) =>
      utx.setPrioritySnapshots(discardedDiffs)
      newHeight
    }

  private def processBlockWithChallenge(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      pos: PoSSelector,
      time: Time,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block): Either[ValidationError, (Seq[StateSnapshot], Option[Int])] = {
    val challengedBlock = block.toOriginal
    for {
      challengedHitSource <-
        if (verify) validateBlock(blockchainUpdater, pos, time)(challengedBlock) else pos.validateGenerationSignature(challengedBlock)
      hitSource <- if (verify) validateBlock(blockchainUpdater, pos, time)(block) else pos.validateGenerationSignature(block)
      discardedSnapshots <-
        metrics.appendBlock
          .measureSuccessful(blockchainUpdater.processBlock(block, hitSource, Some(challengedHitSource), verify, txSignParCheck))
    } yield discardedSnapshots -> Some(blockchainUpdater.height)
  }

  private def validateBlock(blockchainUpdater: Blockchain, pos: PoSSelector, time: Time)(block: Block) =
    for {
      _ <- Miner.isAllowedForMining(block.sender.toAddress, blockchainUpdater).leftMap(BlockAppendError(_, block))
      hitSource <- blockConsensusValidation(blockchainUpdater, pos, time.correctedTime(), block) { (height, parent) =>
        val balance = blockchainUpdater.generatingBalance(block.sender.toAddress, Some(parent))
        Either.cond(
          blockchainUpdater.isEffectiveBalanceValid(height, block, balance),
          balance + block.header.challengedHeader.map(ch => blockchainUpdater.generatingBalance(ch.generator.toAddress, Some(parent))).getOrElse(0L),
          s"generator's effective balance $balance is less that required for generation"
        )
      }
      _ <- validateStateHash(block, blockchainUpdater)
      _ <- validateChallengedHeader(block, blockchainUpdater)
    } yield hitSource

  private def blockConsensusValidation(blockchain: Blockchain, pos: PoSSelector, currentTs: Long, block: Block)(
      genBalance: (Int, BlockId) => Either[String, Long]
  ): Either[ValidationError, ByteStr] =
    metrics.blockConsensusValidation
      .measureSuccessful {

        val blockTime = block.header.timestamp

        for {
          height <- blockchain
            .heightOf(block.header.reference)
            .toRight(GenericError(s"height: history does not contain parent ${block.header.reference}"))
          parent <- blockchain.parentHeader(block.header).toRight(GenericError(s"parent: history does not contain parent ${block.header.reference}"))
          grandParent = blockchain.parentHeader(parent, 2)
          effectiveBalance <- genBalance(height, block.header.reference).left.map(GenericError(_))
          _                <- validateBlockVersion(height, block, blockchain)
          _                <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime))
          _                <- pos.validateBaseTarget(height, block, parent, grandParent)
          hitSource        <- pos.validateGenerationSignature(block)
          _ <- pos
            .validateBlockDelay(height, block.header, parent, effectiveBalance)
            .orElse(checkExceptions(height, block))
        } yield hitSource
      }
      .left
      .map {
        case GenericError(x) => GenericError(s"Block $block is invalid: $x")
        case x               => x
      }

  private def checkExceptions(height: Int, block: Block): Either[ValidationError, Unit] = {
    Either
      .cond(
        exceptions.contains((height, block.id())),
        (),
        GenericError(s"Block time ${block.header.timestamp} less than expected")
      )
  }

  private def validateBlockVersion(parentHeight: Int, block: Block, blockchain: Blockchain): Either[ValidationError, Unit] = {
    Either.cond(
      blockchain.blockVersionAt(parentHeight + 1) == block.header.version,
      (),
      GenericError(s"Block version should be equal to ${blockchain.blockVersionAt(parentHeight + 1)}")
    )
  }

  private def validateChallengedHeader(block: Block, blockchain: Blockchain): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(
        block.header.challengedHeader.isEmpty || blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot, blockchain.height + 1),
        (),
        BlockAppendError("Challenged header is not supported yet", block)
      )
      _ <- Either.cond(
        !block.header.challengedHeader.map(_.generator).contains(block.header.generator),
        (),
        BlockAppendError("Challenged block generator and challenging block generator should not be equal", block)
      )
    } yield ()

  private def validateStateHash(block: Block, blockchain: Blockchain): Either[ValidationError, Unit] =
    Either.cond(
      block.header.stateHash.isEmpty || blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot, blockchain.height + 1),
      (),
      BlockAppendError("Block state hash is not supported yet", block)
    )

  private[this] object metrics {
    val blockConsensusValidation = Kamon.timer("block-appender.block-consensus-validation").withoutTags()
    val appendBlock              = Kamon.timer("block-appender.blockchain-append-block").withoutTags()
  }

}
