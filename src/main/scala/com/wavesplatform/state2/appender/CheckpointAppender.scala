package com.wavesplatform.state2.appender

import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint, PeerDatabase, id}
import com.wavesplatform.state2.ByteStr
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import scorex.transaction.{BlockchainUpdater, CheckpointService, History, ValidationError}
import scorex.utils.ScorexLogging

object CheckpointAppender extends ScorexLogging {
  def apply(checkpointService: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
            peerDatabase: PeerDatabase, miner: Miner, allChannels: ChannelGroup, scheduler: Scheduler
           )(maybeChannel: Option[Channel], c: Checkpoint): Task[Either[ValidationError, Option[BigInt]]] = {
    val t = Task(checkpointService.set(c).map { _ =>
      log.info(s"Processing checkpoint $c")
      makeBlockchainCompliantWith(history, blockchainUpdater)(c)
      history.score
    }).executeOn(scheduler).map(_.map(Some(_)))
    maybeChannel match {
      case None => t
      case Some(ch) => processAndBlacklistOnFailure(ch, peerDatabase, miner, allChannels,
        s"${id(ch)} Attempting to process checkpoint",
        s"${id(ch)} Successfully processed checkpoint",
        s"${id(ch)} Error processing checkpoint")(t)
    }
  }

  private def makeBlockchainCompliantWith(history: History, blockchainUpdater: BlockchainUpdater)(checkpoint: Checkpoint): Unit = {
    val existingItems = checkpoint.items.filter {
      checkpoint => history.blockAt(checkpoint.height).isDefined
    }

    val fork = existingItems.takeWhile {
      case BlockCheckpoint(h, sig) =>
        val block = history.blockAt(h).get
        block.signerData.signature != ByteStr(sig)
    }

    if (fork.nonEmpty) {
      val genesisBlockHeight = 1
      val hh = existingItems.map(_.height) :+ genesisBlockHeight
      history.blockAt(hh(fork.size)).foreach {
        lastValidBlock =>
          log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block $lastValidBlock]")
          blockBlockForkStats.increment()
          blockForkHeightStats.record(fork.size)
          blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  private val blockBlockForkStats = Kamon.metrics.counter("block-fork")
  private val blockForkHeightStats = Kamon.metrics.histogram("block-fork-height")
}
