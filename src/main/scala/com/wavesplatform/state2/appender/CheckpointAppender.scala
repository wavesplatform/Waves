package com.wavesplatform.state2.appender

import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint, PeerDatabase, id}
import com.wavesplatform.state2.ByteStr
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import scorex.transaction.{BlockchainUpdater, CheckpointService, History, ValidationError}
import scorex.utils.ScorexLogging

object CheckpointAppender extends ScorexLogging {

  def apply(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater
           )(newCheckpoint: Checkpoint): Task[Either[ValidationError, BigInt]] =
    Task(checkpoint.set(newCheckpoint).map { _ =>
      log.info(s"Processing checkpoint $checkpoint")
      makeBlockchainCompliantWith(history, blockchainUpdater)(newCheckpoint)
      history.score()
    }).executeOn(scheduler)


  def apply(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
            peerDatabase: PeerDatabase, miner: Miner, allChannels: ChannelGroup
           )(ch: Channel, c: Checkpoint): Task[Either[ValidationError, Option[BigInt]]] =
    processAndBlacklistOnFailure(ch, peerDatabase, miner, allChannels,
      s"${id(ch)} Attempting to process checkpoint",
      s"${id(ch)} Successfully processed checkpoint",
      s"${id(ch)} Error processing checkpoint"
    )(apply(checkpoint, history, blockchainUpdater)(c).map(_.map(Some(_))))


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
