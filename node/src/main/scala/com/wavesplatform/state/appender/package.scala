package com.wavesplatform.state

import cats.implicits._
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.mining._
import com.wavesplatform.network._
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
      errorPrefix: String
  )(f: => Task[Either[B, Option[BigInt]]]): Task[Either[B, Option[BigInt]]] = {
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

  private[appender] def appendBlock(
      blockchainUpdater: BlockchainUpdater with Blockchain,
      utxStorage: UtxPoolImpl,
      pos: PoSSelector,
      time: Time,
      verify: Boolean
  )(block: Block): Either[ValidationError, Option[Int]] = {
    if (verify)
      validateAndAppendBlock(blockchainUpdater, utxStorage, pos, time)(block)
    else
      pos
        .validateGenerationSignature(block)
        .flatMap(hitSource => appendBlock(blockchainUpdater, utxStorage, verify = false)(block, hitSource))
  }

  private[appender] def validateAndAppendBlock(
      blockchainUpdater: BlockchainUpdater with Blockchain,
      utxStorage: UtxPoolImpl,
      pos: PoSSelector,
      time: Time
  )(block: Block): Either[ValidationError, Option[Int]] =
    for {
      _ <- Either.cond(
        !blockchainUpdater.hasScript(block.sender),
        (),
        BlockAppendError(s"Account(${block.sender.toAddress}) is scripted are therefore not allowed to forge blocks", block)
      )
      hitSource <- blockConsensusValidation(blockchainUpdater, pos, time.correctedTime(), block) { (height, parent) =>
        val balance = blockchainUpdater.generatingBalance(block.sender, Some(parent))
        Either.cond(
          blockchainUpdater.isEffectiveBalanceValid(height, block, balance),
          balance,
          s"generator's effective balance $balance is less that required for generation"
        )
      }
      baseHeight <- appendBlock(blockchainUpdater, utxStorage, verify = true)(block, hitSource)
    } yield baseHeight

  private def appendBlock(blockchainUpdater: BlockchainUpdater with Blockchain, utxStorage: UtxPoolImpl, verify: Boolean)(
      block: Block
  ,
      hitSource: ByteStr
  ): Either[ValidationError, Option[Int]] =
    metrics.appendBlock.measureSuccessful(blockchainUpdater.processBlock(block, hitSource, verify)).map { maybeDiscardedTxs =>
      metrics.utxRemoveAll.measure(utxStorage.removeAll(block.transactionData))
      maybeDiscardedTxs.map { discarded =>
        metrics.utxDiscardedPut.measure(utxStorage.addAndCleanup(discarded))
        blockchainUpdater.height
      }
    }

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
        exceptions.contains((height, block.uniqueId)),
        (),
        GenericError(s"Block time ${block.header.timestamp} less than expected")
      )
  }

  private def validateBlockVersion(height: Int, block: Block, blockchain: Blockchain): Either[ValidationError, Unit] = {
    val version3Height  = blockchain.settings.functionalitySettings.blockVersion3AfterHeight
    val versionAtHeight = blockchain.blockVersionAt(height)
    for {
      _ <- Either.cond(
        height > version3Height
          || block.header.version == Block.GenesisBlockVersion
          || block.header.version == Block.PlainBlockVersion,
        (),
        GenericError(s"Block Version 3 can only appear at height greater than $version3Height")
      )
      _ <- Either.cond(
        versionAtHeight <= block.header.version,
        (),
        GenericError(s"Block Version ${block.header.version} is lower than $versionAtHeight")
      )
    } yield ()
  }

  private[this] object metrics {
    val blockConsensusValidation = Kamon.timer("block-appender.block-consensus-validation")
    val appendBlock              = Kamon.timer("block-appender.blockchain-append-block")
    val utxRemoveAll             = Kamon.timer("block-appender.utx-remove-all")
    val utxDiscardedPut          = Kamon.timer("block-appender.utx-discarded-put")
  }

}
