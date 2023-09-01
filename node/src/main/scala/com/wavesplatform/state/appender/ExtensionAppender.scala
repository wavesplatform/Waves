package com.wavesplatform.state.appender

import com.wavesplatform.common.utils.EitherExt2
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
import com.wavesplatform.utx.UtxPoolImpl
import io.netty.channel.Channel
import monix.eval.Task
import monix.execution.Scheduler
import org.influxdb.dto.Point

import scala.util.{Left, Right}

object ExtensionAppender extends ScorexLogging {

  def apply(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      utxStorage: UtxPoolImpl,
      pos: PoSSelector,
      time: Time,
      invalidBlocks: InvalidBlockStorage,
      peerDatabase: PeerDatabase,
      scheduler: Scheduler
  )(ch: Channel, extensionBlocks: ExtensionBlocks): Task[Either[ValidationError, Option[BigInt]]] = {
    def appendExtension(extension: ExtensionBlocks): Either[ValidationError, Option[BigInt]] =
      if (extension.remoteScore <= blockchainUpdater.score) {
        log.trace(s"Ignoring extension $extension because declared remote was not greater than local score ${blockchainUpdater.score}")
        Right(None)
      } else {
        extension.blocks
          .collectFirst { case b if !b.signatureValid() => GenericError(s"Block $b has invalid signature") }
          .toLeft(extension)
          .flatMap { extensionWithValidSignatures =>
            val newBlocks = extensionWithValidSignatures.blocks.dropWhile(blockchainUpdater.contains)

            newBlocks.headOption.map(_.header.reference) match {
              case Some(lastCommonBlockId) =>
                val initialHeight = blockchainUpdater.height

                val droppedBlocksEi = for {
                  commonBlockHeight <- blockchainUpdater.heightOf(lastCommonBlockId).toRight(GenericError("Fork contains no common parent"))
                  droppedBlocks <- {
                    if (commonBlockHeight < initialHeight)
                      blockchainUpdater.removeAfter(lastCommonBlockId)
                    else Right(Seq.empty)
                  }
                } yield (commonBlockHeight, droppedBlocks)

                droppedBlocksEi.flatMap { case (commonBlockHeight, droppedBlocks) =>
                  newBlocks.zipWithIndex.foreach { case (block, idx) =>
                    val rideV6Activated = blockchainUpdater.isFeatureActivated(BlockchainFeatures.RideV6, commonBlockHeight + idx + 1)
                    ParSignatureChecker.checkTxSignatures(block.transactionData, rideV6Activated)
                  }

                  val forkApplicationResultEi = {
                    newBlocks.view
                      .map { b =>
                        b -> appendExtensionBlock(blockchainUpdater, pos, time, verify = true, txSignParCheck = false)(
                          b,
                          extension.snapshots.get(b.id())
                        )
                          .map {
                            case (Applied(_, _), height) => BlockStats.applied(b, BlockStats.Source.Ext, height)
                            case _                       =>
                          }
                      }
                      .zipWithIndex
                      .collectFirst { case ((b, Left(e)), i) => (i, b, e) }
                      .fold[Either[ValidationError, Unit]](Right(())) { case (i, declinedBlock, e) =>
                        e match {
                          case _: TxValidationError.BlockFromFuture =>
                          case _                                    => invalidBlocks.add(declinedBlock.id(), e)
                        }

                        newBlocks.view
                          .dropWhile(_ != declinedBlock)
                          .foreach(BlockStats.declined(_, BlockStats.Source.Ext))

                        if (i == 0) log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block $declinedBlock: $e")
                        else
                          log.warn(
                            s"Processed only ${i + 1} of ${newBlocks.size} blocks from extension, error appending next block $declinedBlock: $e"
                          )

                        Left(e)
                      }
                  }

                  forkApplicationResultEi match {
                    case Left(e) =>
                      blockchainUpdater.removeAfter(lastCommonBlockId).explicitGet()
                      droppedBlocks.foreach { case (b, gp, sn) => blockchainUpdater.processBlock(b, gp, sn).explicitGet() }
                      Left(e)

                    case Right(_) =>
                      val depth = initialHeight - commonBlockHeight
                      if (depth > 0) {
                        Metrics.write(
                          Point
                            .measurement("rollback")
                            .addField("depth", initialHeight - commonBlockHeight)
                            .addField("txs", droppedBlocks.size)
                        )
                      }

                      val newTransactions = newBlocks.view.flatMap(_.transactionData).toSet
                      utxStorage.removeAll(newTransactions)
                      utxStorage.addAndScheduleCleanup(droppedBlocks.flatMap(_._1.transactionData).filterNot(newTransactions))
                      Right(Some(blockchainUpdater.score))
                  }
                }

              case None =>
                log.debug("No new blocks found in extension")
                Right(None)
            }
          }
      }

    log.debug(s"${id(ch)} Attempting to append extension ${formatBlocks(extensionBlocks.blocks)}")
    Task(appendExtension(extensionBlocks)).executeOn(scheduler).map {
      case Right(maybeNewScore) =>
        log.debug(s"${id(ch)} Successfully appended extension ${formatBlocks(extensionBlocks.blocks)}")
        Right(maybeNewScore)
      case Left(ve) =>
        val errorMessage = s"${id(ch)} Error appending extension ${formatBlocks(extensionBlocks.blocks)}: $ve"
        log.warn(errorMessage)
        peerDatabase.blacklistAndClose(ch, errorMessage)
        Left(ve)
    }
  }
}
