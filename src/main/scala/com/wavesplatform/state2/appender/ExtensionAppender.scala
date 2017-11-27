package com.wavesplatform.state2.appender

import com.wavesplatform.UtxPool
import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.metrics.{BlockStats, Instrumented, Metrics}
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{InvalidBlockStorage, PeerDatabase, formatBlocks, id}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.{Coeval, Task}
import org.influxdb.dto.Point
import scorex.block.Block
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.util.{Left, Right}

object ExtensionAppender extends ScorexLogging with Instrumented {

  def apply(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                  stateReader: StateReader, utxStorage: UtxPool, time: Time, settings: WavesSettings,
                  featureProvider: FeatureProvider, invalidBlocks: InvalidBlockStorage,
                  peerDatabase: PeerDatabase, miner: Miner, allChannels: ChannelGroup
                 )(ch: Channel, extensionBlocks: Seq[Block]): Task[Unit] = {
    def p(blocks: Seq[Block]): Task[Either[ValidationError, Option[BigInt]]] = Task(Signed.validateOrdered(blocks).flatMap { newBlocks =>
      history.write { implicit l =>
        val extension = newBlocks.dropWhile(history.contains)

        extension.headOption.map(_.reference) match {
          case Some(lastCommonBlockId) =>

            def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean =
              extension.zipWithIndex.forall(p => checkpoint.isBlockValid(p._1.signerData.signature, lastCommonHeight + 1 + p._2))

            val forkApplicationResultEi = Coeval.evalOnce {
              val firstDeclined = extension.view
                .map { b =>
                  b -> appendBlock(checkpoint, history, blockchainUpdater, stateReader(), utxStorage, time, settings.blockchainSettings, featureProvider)(b).right.map {
                    _.foreach(bh => BlockStats.applied(b, BlockStats.Source.Ext, bh))
                  }
                }
                .zipWithIndex
                .collectFirst { case ((b, Left(e)), i) => (i, b, e) }

              firstDeclined.foreach {
                case (_, declinedBlock, _) =>
                  invalidBlocks.add(declinedBlock.uniqueId)
                  extension.view
                    .dropWhile(_ != declinedBlock)
                    .foreach(BlockStats.declined(_, BlockStats.Source.Ext))
              }

              firstDeclined
                .foldLeft[Either[ValidationError, BigInt]](Right(history.score())) {
                case (_, (i, b, e)) if i == 0 =>
                  log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block $b: $e")
                  Left(e)

                case (r, (i, b, e)) =>
                  log.debug(s"Processed $i of ${newBlocks.size} blocks from extension")
                  log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block $b: $e")
                  r
              }
            }

            val initalHeight = history.height()

            val droppedBlocksEi = (for {
              commonBlockHeight <- history.heightOf(lastCommonBlockId).toRight(GenericError("Fork contains no common parent"))
              _ <- Either.cond(isForkValidWithCheckpoint(commonBlockHeight), (), GenericError("Fork contains block that doesn't match checkpoint, declining fork"))
              droppedBlocks <- blockchainUpdater.removeAfter(lastCommonBlockId)
            } yield (commonBlockHeight, droppedBlocks)).left.map((_, Seq.empty[Block]))

            (for {
              commonHeightAndDroppedBlocks <- droppedBlocksEi
              (commonBlockHeight, droppedBlocks) = commonHeightAndDroppedBlocks
              score <- forkApplicationResultEi().left.map((_, droppedBlocks))
            } yield (commonBlockHeight, droppedBlocks, score))
              .right.map { case ((commonBlockHeight, droppedBlocks, score)) =>
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
              Some(score)
            }.left.map { case ((err, droppedBlocks)) =>
              droppedBlocks.foreach(blockchainUpdater.processBlock(_).explicitGet())
              err
            }
          case None =>
            log.debug("No new blocks found in extension")
            Right(None)
        }
      }
    }).executeOn(scheduler)

    extensionBlocks.foreach(BlockStats.received(_, BlockStats.Source.Ext, ch))
    processAndBlacklistOnFailure(ch, peerDatabase,
      s"${id(ch)} Attempting to append extension ${formatBlocks(extensionBlocks)}",
      s"${id(ch)} Successfully appended extension ${formatBlocks(extensionBlocks)}",
      s"${id(ch)} Error appending extension ${formatBlocks(extensionBlocks)}",
      p(extensionBlocks),
      scheduleMiningAndBroadcastScore(miner, allChannels))
  }
}
