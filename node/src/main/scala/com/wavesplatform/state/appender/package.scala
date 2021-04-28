package com.wavesplatform.state

import scala.util.{Left, Right}

import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.network._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, BlockFromFuture, GenericError}
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPoolImpl
import io.netty.channel.Channel
import kamon.Kamon
import monix.eval.Task

package object appender extends ScorexLogging {

  val MaxTimeDrift: Long = 100 // millis

  // Invalid blocks, that are already in blockchain
  private val exceptions = List(
    812608 -> ByteStr.decodeBase58("2GNCYVy7k3kEPXzz12saMtRDeXFKr8cymVsG8Yxx3sZZ75eHj9csfXnGHuuJe7XawbcwjKdifUrV1uMq4ZNCWPf1").get,
    813207 -> ByteStr.decodeBase58("5uZoDnRKeWZV9Thu2nvJVZ5dBvPB7k2gvpzFD618FMXCbBVBMN2rRyvKBZBhAGnGdgeh2LXEeSr9bJqruJxngsE7").get
  )

  private[appender] def processAndBlacklistOnFailure[A, B](
      ch: Channel,
      peerDatabase: PeerDatabase,
      start: => String,
      success: => String,
      errorPrefix: String
  )(f: => Task[Either[B, Option[BigInt]]]): Task[Either[B, Option[BigInt]]] = {
    log.debug(start)
    f map {
      case Right(maybeNewScore) =>
        log.debug(success)
        Right(maybeNewScore)
      case Left(ve) =>
        log.warn(s"$errorPrefix: $ve")
        peerDatabase.blacklistAndClose(ch, s"$errorPrefix: $ve")
        Left(ve)
    }
  }

  private[appender] def appendKeyBlock(
      blockchainUpdater: BlockchainUpdater with Blockchain,
      utx: UtxPoolImpl,
      pos: PoSSelector,
      time: Time,
      verify: Boolean
  )(block: Block): Either[ValidationError, Option[Int]] =
    for {
      hitSource <- if (verify) validateBlock(blockchainUpdater, pos, time)(block) else pos.validateGenerationSignature(block)
      newHeight <- utx.priorityPool.lockedWrite {
        metrics.appendBlock
          .measureSuccessful(blockchainUpdater.processBlock(block, hitSource, verify))
          .map { discardedDiffs =>
            utx.removeAll(block.transactionData)
            utx.setPriorityDiffs(discardedDiffs)
            utx.runCleanup()
            Some(blockchainUpdater.height)
          }
      }
    } yield newHeight

  private[appender] def appendExtensionBlock(
      blockchainUpdater: BlockchainUpdater with Blockchain,
      pos: PoSSelector,
      time: Time,
      verify: Boolean
  )(block: Block): Either[ValidationError, Option[Int]] =
    for {
      hitSource <- if (verify) validateBlock(blockchainUpdater, pos, time)(block) else pos.validateGenerationSignature(block)
      _         <- metrics.appendBlock.measureSuccessful(blockchainUpdater.processBlock(block, hitSource, verify))
    } yield Some(blockchainUpdater.height)

  private def validateBlock(blockchainUpdater: Blockchain, pos: PoSSelector, time: Time)(block: Block) =
    for {
      _ <- Either.cond(
        !blockchainUpdater.hasAccountScript(block.sender.toAddress),
        (),
        BlockAppendError(s"Account(${block.sender.toAddress}) is scripted are therefore not allowed to forge blocks", block)
      )
      hitSource <- blockConsensusValidation(blockchainUpdater, pos, time.correctedTime(), block) { (height, parent) =>
        val balance = blockchainUpdater.generatingBalance(block.sender.toAddress, Some(parent))
        Either.cond(
          blockchainUpdater.isEffectiveBalanceValid(height, block, balance),
          balance,
          s"generator's effective balance $balance is less that required for generation"
        )
      }
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
          _                <- pos.validateBlockDelay(height, block.header, parent, effectiveBalance).orElse(checkExceptions(height, block))
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

  private[this] object metrics {
    val blockConsensusValidation = Kamon.timer("block-appender.block-consensus-validation").withoutTags()
    val appendBlock              = Kamon.timer("block-appender.blockchain-append-block").withoutTags()
  }

}
