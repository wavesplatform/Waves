package com.wavesplatform.state

import cats.implicits._
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.mining._
import com.wavesplatform.network._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, BlockFromFuture, GenericError}
import com.wavesplatform.transaction._
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPoolImpl
import io.netty.channel.Channel
import kamon.Kamon
import monix.eval.Task

import scala.util.{Left, Right}

package object appender extends ScorexLogging {

  private val MaxTimeDrift: Long = 100 // millis

  // Invalid blocks, that are already in blockchain
  private val exceptions = List(
    812608 -> ByteStr.decodeBase58("2GNCYVy7k3kEPXzz12saMtRDeXFKr8cymVsG8Yxx3sZZ75eHj9csfXnGHuuJe7XawbcwjKdifUrV1uMq4ZNCWPf1").get,
    813207 -> ByteStr.decodeBase58("5uZoDnRKeWZV9Thu2nvJVZ5dBvPB7k2gvpzFD618FMXCbBVBMN2rRyvKBZBhAGnGdgeh2LXEeSr9bJqruJxngsE7").get
  )

  private[appender] def processAndBlacklistOnFailure[A, B](
      ch: Channel,
      peerDatabase: PeerDatabase,
      miner: Miner,
      start: => String,
      success: => String,
      errorPrefix: String)(f: => Task[Either[B, Option[BigInt]]]): Task[Either[B, Option[BigInt]]] = {

    log.debug(start)
    f map {
      case Right(maybeNewScore) =>
        log.debug(success)
        maybeNewScore.foreach(_ => miner.scheduleMining())
        Right(maybeNewScore)
      case Left(ve) =>
        log.warn(s"$errorPrefix: $ve")
        peerDatabase.blacklistAndClose(ch, s"$errorPrefix: $ve")
        Left(ve)
    }
  }

  private[appender] def appendBlock(blockchainUpdater: BlockchainUpdater with Blockchain,
                                    utxStorage: UtxPoolImpl,
                                    pos: PoSSelector,
                                    time: Time,
                                    verify: Boolean)(block: Block): Either[ValidationError, Option[Int]] = {
    val append: Block => Either[ValidationError, Option[Int]] =
      if (verify) validateAndAppendBlock(blockchainUpdater, utxStorage, pos, time)
      else appendBlock(blockchainUpdater, utxStorage, verify = false)
    append(block)
  }

  private[appender] def validateAndAppendBlock(blockchainUpdater: BlockchainUpdater with Blockchain,
                                               utxStorage: UtxPoolImpl,
                                               pos: PoSSelector,
                                               time: Time)(block: Block): Either[ValidationError, Option[Int]] =
    for {
      _ <- Either.cond(
        !blockchainUpdater.hasScript(block.sender),
        (),
        BlockAppendError(s"Account(${block.sender.toAddress}) is scripted are therefore not allowed to forge blocks", block)
      )
      _ <- blockConsensusValidation(blockchainUpdater, pos, time.correctedTime(), block) { (height, parent) =>
        val balance = blockchainUpdater.generatingBalance(block.sender, parent)
        Either.cond(
          blockchainUpdater.isEffectiveBalanceValid(height, block, balance),
          balance,
          s"generator's effective balance $balance is less that required for generation"
        )
      }
      baseHeight <- appendBlock(blockchainUpdater, utxStorage, verify = true)(block)
    } yield baseHeight

  private def appendBlock(blockchainUpdater: BlockchainUpdater with Blockchain, utxStorage: UtxPoolImpl, verify: Boolean)(
      block: Block): Either[ValidationError, Option[Int]] =
    metrics.appendBlock.measureSuccessful(blockchainUpdater.processBlock(block, verify)).map { maybeDiscardedTxs =>
      metrics.utxRemoveAll.measure(utxStorage.removeAll(block.transactionData))
      maybeDiscardedTxs.map { discarded =>
        metrics.utxDiscardedPut.measure(utxStorage.addAndCleanup(discarded))
        blockchainUpdater.height
      }
    }

  private def blockConsensusValidation(blockchain: Blockchain, pos: PoSSelector, currentTs: Long, block: Block)(
      genBalance: (Int, BlockId) => Either[String, Long]): Either[ValidationError, Unit] =
    metrics.blockConsensusValidation
      .measureSuccessful {

        val blockTime = block.timestamp

        for {
          height <- blockchain.heightOf(block.reference).toRight(GenericError(s"height: history does not contain parent ${block.reference}"))
          parent <- blockchain.parentHeader(block).toRight(GenericError(s"parent: history does not contain parent ${block.reference}"))
          grandParent = blockchain.parentHeader(parent, 2)
          effectiveBalance <- genBalance(height, block.reference).left.map(GenericError(_))
          _                <- validateBlockVersion(height, block, blockchain.settings.functionalitySettings)
          _                <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime))
          _                <- pos.validateBaseTarget(height, block, parent, grandParent)
          _                <- pos.validateGeneratorSignature(height, block)
          _                <- pos.validateBlockDelay(height, block, parent, effectiveBalance).orElse(checkExceptions(height, block))
        } yield ()
      }
      .left
      .map {
        case GenericError(x) => GenericError(s"Block $block is invalid: $x")
        case x               => x
      }

  private def checkExceptions(height: Int, block: Block): Either[ValidationError, Unit] = {
    Either
      .cond(
        exceptions.contains((height, block.uniqueId)),
        (),
        GenericError(s"Block time ${block.timestamp} less than expected")
      )
  }

  private def validateBlockVersion(height: Int, block: Block, fs: FunctionalitySettings): Either[ValidationError, Unit] = {
    val version3Height = fs.blockVersion3AfterHeight
    Either.cond(
      height > version3Height
        || block.version == Block.GenesisBlockVersion
        || block.version == Block.PlainBlockVersion,
      (),
      GenericError(s"Block Version 3 can only appear at height greater than $version3Height")
    )
  }

  private[this] object metrics {
    val blockConsensusValidation = Kamon.timer("block-appender.block-consensus-validation")
    val appendBlock              = Kamon.timer("block-appender.blockchain-append-block")
    val utxRemoveAll             = Kamon.timer("block-appender.utx-remove-all")
    val utxDiscardedPut          = Kamon.timer("block-appender.utx-discarded-put")
  }
}
