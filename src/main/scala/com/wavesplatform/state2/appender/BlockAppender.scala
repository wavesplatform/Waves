package com.wavesplatform.state2.appender

import cats.data.EitherT
import com.wavesplatform.UtxPool
import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.metrics._
import com.wavesplatform.mining.Miner
import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.{BlockAppendError, InvalidSignature}
import scorex.transaction.{BlockchainUpdater, CheckpointService, History, ValidationError}
import scorex.utils.{ScorexLogging, Time}

import scala.util.Right

object BlockAppender extends ScorexLogging with Instrumented {

  def apply(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, time: Time,
            stateReader: SnapshotStateReader, utxStorage: UtxPool, settings: WavesSettings,
            featureProvider: FeatureProvider, scheduler: Scheduler)(newBlock: Block): Task[Either[ValidationError, Option[BlockchainScore]]] = Task {
    measureSuccessful(blockProcessingTimeStats, {
      if (history.contains(newBlock)) Right(None)
      else for {
        _ <- Either.cond(history.heightOf(newBlock.reference).exists(_ >= history.height - 1), (), BlockAppendError("Irrelevant block", newBlock))
        maybeBaseHeight <- appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings, featureProvider)(newBlock)
      } yield maybeBaseHeight map (_ => history.score)
    })
  }.executeOn(scheduler)

  def apply(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, time: Time,
            stateReader: SnapshotStateReader, utxStorage: UtxPool, settings: WavesSettings,
            featureProvider: FeatureProvider, allChannels: ChannelGroup, peerDatabase: PeerDatabase, miner: Miner,
            scheduler: Scheduler)
           (ch: Channel, newBlock: Block): Task[Unit] = {
    BlockStats.received(newBlock, BlockStats.Source.Broadcast, ch)
    blockReceivingLag.safeRecord(System.currentTimeMillis() - newBlock.timestamp)
    (for {
      _ <- EitherT(Task.now(newBlock.signaturesValid()))
      validApplication <- EitherT(apply(checkpoint, history, blockchainUpdater, time, stateReader, utxStorage, settings,
        featureProvider, scheduler)(newBlock))
    } yield validApplication).value.map {
      case Right(None) =>
        log.trace(s"${id(ch)} $newBlock already appended")
      case Right(Some(_)) =>
        BlockStats.applied(newBlock, BlockStats.Source.Broadcast, history.height)
        log.debug(s"${id(ch)} Appended $newBlock")
        if (newBlock.transactionData.isEmpty)
          allChannels.broadcast(BlockForged(newBlock), Some(ch))
        miner.scheduleMining()
      case Left(is: InvalidSignature) =>
        peerDatabase.blacklistAndClose(ch, s"Could not append $newBlock: $is")
      case Left(ve) =>
        BlockStats.declined(newBlock, BlockStats.Source.Broadcast)
        log.debug(s"${id(ch)} Could not append $newBlock: $ve")
    }
  }

  private val blockReceivingLag = Kamon.metrics.histogram("block-receiving-lag")
  private val blockProcessingTimeStats = Kamon.metrics.histogram("single-block-processing-time")

}
