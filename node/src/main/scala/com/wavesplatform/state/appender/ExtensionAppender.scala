package com.wavesplatform.state.appender

import scala.util.{Left, Right}

import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.{BlockStats, Metrics}
import com.wavesplatform.network.{formatBlocks, id, InvalidBlockStorage, PeerDatabase}
import com.wavesplatform.state._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPoolImpl
import io.netty.channel.Channel
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import org.influxdb.dto.Point

object ExtensionAppender extends ScorexLogging {

  def apply(
      blockchainUpdater: BlockchainUpdater with Blockchain,
      utxStorage: UtxPoolImpl,
      pos: PoSSelector,
      time: Time,
      invalidBlocks: InvalidBlockStorage,
      peerDatabase: PeerDatabase,
      scheduler: Scheduler
  )(ch: Channel, extensionBlocks: Seq[Block]): Task[Either[ValidationError, Option[BigInt]]] = {
    def processBlocks(blocks: Seq[Block]): Task[Either[ValidationError, Option[BigInt]]] =
      Task(
        blocks
          .collectFirst { case b if !b.signatureValid() => GenericError(s"Block $b has invalid signature") }
          .toLeft(blocks)
          .flatMap { newBlocks =>
            val extension = newBlocks.dropWhile(blockchainUpdater.contains)

            extension.headOption.map(_.header.reference) match {
              case Some(lastCommonBlockId) =>
                val forkApplicationResultEi = Coeval {
                  extension.view
                    .map { b =>
                      b -> validateAndAppendBlock(blockchainUpdater, utxStorage, pos, time)(b)
                        .map {
                          _.foreach(bh => BlockStats.applied(b, BlockStats.Source.Ext, bh))
                        }
                    }
                    .zipWithIndex
                    .collectFirst { case ((b, Left(e)), i) => (i, b, e) }
                    .fold[Either[ValidationError, Unit]](Right(())) {
                      case (i, declinedBlock, e) =>
                        e match {
                          case _: TxValidationError.BlockFromFuture =>
                          case _                                    => invalidBlocks.add(declinedBlock.id(), e)
                        }

                        extension.view
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

                val initialHeight = blockchainUpdater.height

                val droppedBlocksEi = for {
                  commonBlockHeight <- blockchainUpdater.heightOf(lastCommonBlockId).toRight(GenericError("Fork contains no common parent"))
                  droppedBlocks <- {
                    if (commonBlockHeight < initialHeight)
                      blockchainUpdater.removeAfter(lastCommonBlockId)
                    else Right(Seq.empty)
                  }
                } yield (commonBlockHeight, droppedBlocks)

                droppedBlocksEi.flatMap {
                  case (commonBlockHeight, droppedBlocks) =>
                    forkApplicationResultEi() match {
                      case Left(e) =>
                        val doAppend = appendBlockAndUpdateUTX(blockchainUpdater, utxStorage, verify = true) _

                        blockchainUpdater.removeAfter(lastCommonBlockId).explicitGet()
                        droppedBlocks.foreach { case (block, hitSource) => doAppend(block, hitSource).explicitGet() }
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

                        val newTransactions = {
                          val all  = droppedBlocks.flatMap { case (block, _) => block.transactionData }
                          val seen = extension.flatMap(_.transactionData).toSet
                          all.filterNot(seen)
                        }
                        utxStorage.addAndCleanup(newTransactions)
                        Right(Some(blockchainUpdater.score))
                    }
                }

              case None =>
                log.debug("No new blocks found in extension")
                Right(None)
            }
          }
      ).executeOn(scheduler)

    extensionBlocks.foreach(BlockStats.received(_, BlockStats.Source.Ext, ch))
    processAndBlacklistOnFailure(
      ch,
      peerDatabase,
      s"${id(ch)} Attempting to append extension ${formatBlocks(extensionBlocks)}",
      s"${id(ch)} Successfully appended extension ${formatBlocks(extensionBlocks)}",
      s"${id(ch)} Error appending extension ${formatBlocks(extensionBlocks)}"
    )(processBlocks(extensionBlocks))
  }
}
