package com.wavesplatform.state2.appender

import com.wavesplatform.UtxPool
import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.metrics.{BlockStats, Instrumented, Metrics}
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{InvalidBlockStorage, PeerDatabase, formatBlocks, id}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import org.influxdb.dto.Point
import scorex.block.Block
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.util.{Left, Right}

object ExtensionAppender extends ScorexLogging with Instrumented {

  def apply(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
            stateReader: SnapshotStateReader, utxStorage: UtxPool, time: Time, settings: WavesSettings,
            featureProvider: FeatureProvider, invalidBlocks: InvalidBlockStorage,
            peerDatabase: PeerDatabase, miner: Miner, allChannels: ChannelGroup, scheduler: Scheduler
           )(ch: Channel, extensionBlocks: Seq[Block]): Task[Either[ValidationError, Option[BigInt]]] = {
    def p(blocks: Seq[Block]): Task[Either[ValidationError, Option[BigInt]]] = Task(Signed.validateOrdered(blocks).flatMap { newBlocks =>
      {
        val extension = newBlocks.dropWhile(history.contains)

        extension.headOption.map(_.reference) match {
          case Some(lastCommonBlockId) =>
            def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean =
              extension.zipWithIndex.forall(p => checkpoint.isBlockValid(p._1.signerData.signature, lastCommonHeight + 1 + p._2))

            val forkApplicationResultEi = Coeval {
              extension.view
                .map { b =>
                  b -> appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings, featureProvider)(b).right.map {
                    _.foreach(bh => BlockStats.applied(b, BlockStats.Source.Ext, bh))
                  }
                }
                .zipWithIndex
                .collectFirst { case ((b, Left(e)), i) => (i, b, e) }
                .fold[Either[ValidationError, Unit]](Right(())) {
                case (i, declinedBlock, e) =>
                  e match {
                    case _: ValidationError.BlockFromFuture =>
                    case _ => invalidBlocks.add(declinedBlock.uniqueId, e)
                  }

                  extension.view
                    .dropWhile(_ != declinedBlock)
                    .foreach(BlockStats.declined(_, BlockStats.Source.Ext))

                  if (i == 0) log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block $declinedBlock: $e")
                  else log.warn(s"Processed only ${i + 1} of ${newBlocks.size} blocks from extension, error appending next block $declinedBlock: $e")

                  Left(e)
              }
            }

            val initalHeight = history.height

            val droppedBlocksEi = for {
              commonBlockHeight <- history.heightOf(lastCommonBlockId).toRight(GenericError("Fork contains no common parent"))
              _ <- Either.cond(isForkValidWithCheckpoint(commonBlockHeight), (), GenericError("Fork contains block that doesn't match checkpoint, declining fork"))
              droppedBlocks <- blockchainUpdater.removeAfter(lastCommonBlockId)
            } yield (commonBlockHeight, droppedBlocks)

            droppedBlocksEi.flatMap {
              case (commonBlockHeight, droppedBlocks) =>
                forkApplicationResultEi() match {
                  case Left(e) =>
                    blockchainUpdater.removeAfter(lastCommonBlockId).explicitGet()
                    droppedBlocks.foreach(blockchainUpdater.processBlock(_).explicitGet())
                    Left(e)

                  case Right(_) =>
                    val depth = initalHeight - commonBlockHeight
                    if (depth > 0) {
                      Metrics.write(
                        Point
                          .measurement("rollback")
                          .addField("depth", initalHeight - commonBlockHeight)
                          .addField("txs", droppedBlocks.size)
                      )
                    }
                    droppedBlocks.flatMap(_.transactionData).foreach(utxStorage.putIfNew)
                    Right(Some(history.score))
                }
            }

          case None =>
            log.debug("No new blocks found in extension")
            Right(None)
        }
      }
    }).executeOn(scheduler)

    extensionBlocks.foreach(BlockStats.received(_, BlockStats.Source.Ext, ch))
    processAndBlacklistOnFailure(ch, peerDatabase, miner, allChannels,
      s"${id(ch)} Attempting to append extension ${formatBlocks(extensionBlocks)}",
      s"${id(ch)} Successfully appended extension ${formatBlocks(extensionBlocks)}",
      s"${id(ch)} Error appending extension ${formatBlocks(extensionBlocks)}"
    )(p(extensionBlocks))
  }
}
