package com.wavesplatform.state.appender

import cats.data.OptionT
import cats.syntax.all.*
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.{BlockStats, Metrics}
import com.wavesplatform.network.{ExtensionBlocks, InvalidBlockStorage, PeerDatabase, formatBlocks, id}
import com.wavesplatform.state.*
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Applied
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import monix.eval.Task
import monix.execution.Scheduler
import org.influxdb.dto.Point

import scala.annotation.tailrec
import scala.util.chaining.*

object ExtensionAppender extends ScorexLogging {
  private case class AppendData(newBlocks: Seq[Block], lastCommonBlockId: BlockId, lastCommonHeight: Int)

  def apply(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      utxStorage: UtxPool,
      pos: PoSSelector,
      time: Time,
      invalidBlocks: InvalidBlockStorage,
      peerDatabase: PeerDatabase,
      scheduler: Scheduler
  )(ch: Channel, extension: ExtensionBlocks): Task[Either[ValidationError, Option[BigInt]]] = {
    type Result[A] = Either[ValidationError, A]
    def appendExtension(): OptionT[Result, BigInt] =
      if (extension.remoteScore <= blockchainUpdater.score) {
        log.trace(s"Ignoring extension $extension because declared remote was not greater than local score ${blockchainUpdater.score}")
        OptionT.none[Result, BigInt]
      } else
        for {
          _          <- OptionT.liftF(validateSignatures())
          appendData <- OptionT.fromOption[Result](dropCommonPrefix())
          _          <- OptionT.liftF(processNewBlocks(appendData))
        } yield blockchainUpdater.score

    def validateSignatures(): Either[ValidationError, ExtensionBlocks] =
      extension.blocks
        .collectFirst { case b if !b.signatureValid() => GenericError(s"Block $b has invalid signature") }
        .toLeft(extension)

    def dropCommonPrefix(): Option[AppendData] = {
      @tailrec def loop(last: AppendData): Option[AppendData] = last.newBlocks match {
        case Nil =>
          log.debug("No new blocks found in extension")
          None

        case b +: rest =>
          blockchainUpdater.heightOf(b.id()) match {
            case None    => last.some
            case Some(h) => loop(AppendData(rest, b.id(), h))
          }
      }

      loop(
        AppendData(
          extension.blocks,
          blockchainUpdater.lastBlockId.getOrElse(throw new RuntimeException("Empty blockchain")),
          blockchainUpdater.height
        )
      )
    }

    def processNewBlocks(appendData: AppendData): Either[ValidationError, Unit] = {
      val originalForkHeight = blockchainUpdater.height
      for {
        discardedBlocks <-
          if (appendData.lastCommonHeight < originalForkHeight) blockchainUpdater.removeAfter(appendData.lastCommonBlockId)
          else Right(Seq.empty)
        _ = precheckSignatures(appendData)
        _ <- applyFork(appendData).tap {
          case Left(_) => restoreDiscardedBlocks(appendData.lastCommonBlockId, discardedBlocks)
          case Right(_) =>
            val depth = originalForkHeight - appendData.lastCommonHeight
            if (depth > 0)
              Metrics.write(
                Point
                  .measurement("rollback")
                  .addField("depth", depth)
                  .addField("txs", discardedBlocks.size)
              )

            val newTxs   = appendData.newBlocks.flatMap(_.transactionData)
            val newTxIds = newTxs.view.map(_.id()).toSet
            utxStorage.removeIds(newTxIds)

            val discardedTxs = discardedBlocks.flatMap(_._1.transactionData)
            utxStorage.addAndScheduleCleanup(discardedTxs.filterNot(tx => newTxIds.contains(tx.id()))) // In a case of re-appending issues
        }
      } yield ()
    }

    def precheckSignatures(appendData: AppendData): Unit =
      appendData.newBlocks.zipWithIndex.foreach { case (block, idx) =>
        val rideV6Activated = blockchainUpdater.isFeatureActivated(BlockchainFeatures.RideV6, appendData.lastCommonHeight + idx + 1)
        ParSignatureChecker.checkTxSignatures(block.transactionData, rideV6Activated)
      }

    def applyFork(appendData: AppendData): Either[ValidationError, Unit] = appendData.newBlocks.view
      .map { b =>
        val s = extension.snapshots.get(b.id())
        val r = appendExtensionBlock(blockchainUpdater, pos, time, verify = true, txSignParCheck = false)(b, s).map {
          case (_: Applied, height) => BlockStats.applied(b, BlockStats.Source.Ext, height)
          case _                    =>
        }
        b -> r
      }
      .zipWithIndex
      .collectFirst { case ((b, Left(e)), i) => (i, b, e) }
      .fold(Either.unit[ValidationError]) { case (i, declinedBlock, e) =>
        e match {
          case _: TxValidationError.BlockFromFuture =>
          case _                                    => invalidBlocks.add(declinedBlock.id(), e)
        }

        appendData.newBlocks.view
          .dropWhile(_ != declinedBlock)
          .foreach(BlockStats.declined(_, BlockStats.Source.Ext))

        log.warn(
          if (i == 0) s"Can't process fork starting with ${appendData.lastCommonBlockId}, error appending block $declinedBlock: $e"
          else s"Processed only ${i + 1} of ${appendData.newBlocks.size} blocks from extension, error appending next block $declinedBlock: $e"
        )

        Left(e)
      }

    def restoreDiscardedBlocks(lastCommonBlockId: ByteStr, blocks: DiscardedBlocks): Unit = {
      blockchainUpdater.removeAfter(lastCommonBlockId).explicitGet()
      blocks.foreach { x =>
        blockchainUpdater.processBlock(x.block, x.hitSource, x.snapshot, x.generatorSet).explicitGet()
      }
    }

    val formattedBlocksStr = formatBlocks(extension.blocks)
    log.debug(s"${id(ch)} Attempting to append extension $formattedBlocksStr")
    Task(appendExtension().value).executeOn(scheduler).map {
      case Right(maybeNewScore) =>
        log.debug(s"${id(ch)} Successfully appended extension $formattedBlocksStr")
        Right(maybeNewScore)
      case Left(ve) =>
        val errorMessage = s"${id(ch)} Error appending extension $formattedBlocksStr: $ve"
        log.warn(errorMessage)
        peerDatabase.blacklistAndClose(ch, errorMessage)
        Left(ve)
    }
  }
}
