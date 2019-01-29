package com.wavesplatform.state.appender

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint, PeerDatabase, id}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import com.wavesplatform.transaction.{BlockchainUpdater, CheckpointService, ValidationError}

object CheckpointAppender extends ScorexLogging {
  def apply(checkpointService: CheckpointService,
            blockchain: Blockchain,
            blockchainUpdater: BlockchainUpdater,
            peerDatabase: PeerDatabase,
            miner: Miner,
            allChannels: ChannelGroup,
            scheduler: Scheduler)(maybeChannel: Option[Channel], c: Checkpoint): Task[Either[ValidationError, Option[BigInt]]] = {
    val t = Task(checkpointService.set(c).map { _ =>
      log.info(s"Processing checkpoint $c")
      makeBlockchainCompliantWith(blockchain, blockchainUpdater)(c)
      blockchain.score
    }).executeOn(scheduler).map(_.map(Some(_)))
    maybeChannel match {
      case None => t
      case Some(ch) =>
        processAndBlacklistOnFailure(
          ch,
          peerDatabase,
          miner,
          allChannels,
          s"${id(ch)} Attempting to process checkpoint",
          s"${id(ch)} Successfully processed checkpoint",
          s"${id(ch)} Error processing checkpoint"
        )(t)
    }
  }

  private def makeBlockchainCompliantWith(blockchain: Blockchain, blockchainUpdater: BlockchainUpdater)(checkpoint: Checkpoint): Unit = {
    val existingItems = checkpoint.items.filter { checkpoint =>
      blockchain.blockAt(checkpoint.height).isDefined
    }

    val fork = existingItems.takeWhile {
      case BlockCheckpoint(h, sig) =>
        val block = blockchain.blockAt(h).get
        block.signerData.signature != ByteStr(sig)
    }

    if (fork.nonEmpty) {
      val genesisBlockHeight = 1
      val hh                 = existingItems.map(_.height) :+ genesisBlockHeight
      blockchain.blockAt(hh(fork.size)).foreach { lastValidBlock =>
        log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block $lastValidBlock]")
        blockBlockForkStats.increment()
        blockForkHeightStats.record(fork.size)
        blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  private val blockBlockForkStats  = Kamon.counter("block-fork")
  private val blockForkHeightStats = Kamon.histogram("block-fork-height")
}
