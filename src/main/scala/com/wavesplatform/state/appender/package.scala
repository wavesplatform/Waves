package com.wavesplatform.state

import com.wavesplatform.consensus.{GeneratingBalanceProvider, PoSSelector}
import com.wavesplatform.mining._
import com.wavesplatform.network._
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.{BlockAppendError, BlockFromFuture, GenericError}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.util.{Left, Right}

package object appender extends ScorexLogging {

  private val MaxTimeDrift: Long = 100 // millis

  private[appender] def processAndBlacklistOnFailure[A, B](
      ch: Channel,
      peerDatabase: PeerDatabase,
      miner: Miner,
      allChannels: ChannelGroup,
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

  private[appender] def appendBlock(checkpoint: CheckpointService,
                                    blockchainUpdater: BlockchainUpdater with Blockchain,
                                    utxStorage: UtxPool,
                                    pos: PoSSelector,
                                    time: Time,
                                    settings: WavesSettings)(block: Block): Either[ValidationError, Option[Int]] =
    for {
      _ <- Either.cond(
        checkpoint.isBlockValid(block.signerData.signature, blockchainUpdater.height + 1),
        (),
        BlockAppendError(s"Block $block at height ${blockchainUpdater.height + 1} is not valid w.r.t. checkpoint", block)
      )
      _ <- Either.cond(
        blockchainUpdater.accountScript(block.sender).isEmpty,
        (),
        BlockAppendError(s"Account(${block.sender.toAddress}) is scripted are therefore not allowed to forge blocks", block)
      )
      _ <- blockConsensusValidation(blockchainUpdater, settings, pos, time.correctedTime(), block) { height =>
        val balance = GeneratingBalanceProvider.balance(blockchainUpdater, settings.blockchainSettings.functionalitySettings, height, block.sender)
        Either.cond(
          GeneratingBalanceProvider.isEffectiveBalanceValid(blockchainUpdater,
                                                            settings.blockchainSettings.functionalitySettings,
                                                            height,
                                                            block,
                                                            balance),
          balance,
          s"generator's effective balance $balance is less that required for generation"
        )
      }
      baseHeight = blockchainUpdater.height
      maybeDiscardedTxs <- blockchainUpdater.processBlock(block)
    } yield {
      utxStorage.removeAll(block.transactionData)
      utxStorage.batched { ops =>
        maybeDiscardedTxs.toSeq.flatten.foreach(ops.putIfNew)
      }
      maybeDiscardedTxs.map(_ => baseHeight)
    }

  private def blockConsensusValidation(blockchain: Blockchain, settings: WavesSettings, pos: PoSSelector, currentTs: Long, block: Block)(
      genBalance: Int => Either[String, Long]): Either[ValidationError, Unit] = {

    val blockTime = block.timestamp

    for {
      height <- blockchain.heightOf(block.reference).toRight(GenericError(s"height: history does not contain parent ${block.reference}"))
      parent <- blockchain.parent(block).toRight(GenericError(s"parent: history does not contain parent ${block.reference}"))
      grandParent = blockchain.parent(parent, 2)
      effectiveBalance <- genBalance(height).left.map(GenericError(_))
      _                <- validateBlockVersion(height, block, settings.blockchainSettings.functionalitySettings)
      _                <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime))
      _                <- validateTransactionSorting(height, block, settings.blockchainSettings.functionalitySettings)
      _                <- pos.validateBaseTarget(height, block, parent, grandParent)
      _                <- pos.validateGeneratorSignature(height, block)
      _                <- pos.validateBlockDelay(height, block, parent, effectiveBalance)
    } yield ()
  }.left.map {
    case GenericError(x) => GenericError(s"Block $block is invalid: $x")
    case x               => x
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

  private def validateTransactionSorting(height: Int, block: Block, settings: FunctionalitySettings): Either[ValidationError, Unit] = {
    val blockTime = block.timestamp
    for {
      _ <- Either.cond(
        blockTime < settings.requireSortedTransactionsAfter
          || height > settings.dontRequireSortedTransactionsAfter
          || block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData,
        (),
        GenericError("transactions are not sorted")
      )
    } yield ()
  }

}
