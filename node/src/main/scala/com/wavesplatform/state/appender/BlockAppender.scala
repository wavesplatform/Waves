package com.wavesplatform.state.appender

import cats.data.EitherT
import com.wavesplatform.block.Block
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.mining.Miner
import com.wavesplatform.network._
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, InvalidSignature}
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPoolImpl
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler

import scala.util.Right

object BlockAppender extends ScorexLogging {
  def apply(blockchainUpdater: BlockchainUpdater with Blockchain,
            time: Time,
            utxStorage: UtxPoolImpl,
            pos: PoSSelector,
            scheduler: Scheduler,
            verify: Boolean = true)(newBlock: Block): Task[Either[ValidationError, Option[BigInt]]] =
    Task {
      metrics.blockProcessingTimeStats.measureSuccessful {
        if (blockchainUpdater.isLastBlockId(newBlock.header.reference))
          appendBlock(blockchainUpdater, utxStorage, pos, time, verify)(newBlock).map(_ => Some(blockchainUpdater.score))
        else if (blockchainUpdater.contains(newBlock.header.uniqueId) || blockchainUpdater.isLastBlockId(newBlock.header.uniqueId))
          Right(None)
        else
          Left(BlockAppendError("Block is not a child of the last block", newBlock))
      }
    }.executeOn(scheduler)

  def apply(blockchainUpdater: BlockchainUpdater with Blockchain,
            time: Time,
            utxStorage: UtxPoolImpl,
            pos: PoSSelector,
            allChannels: ChannelGroup,
            peerDatabase: PeerDatabase,
            miner: Miner,
            scheduler: Scheduler)(ch: Channel, newBlock: Block): Task[Unit] = {
    BlockStats.received(newBlock, BlockStats.Source.Broadcast, ch)
    metrics.blockReceivingLag.safeRecord(System.currentTimeMillis() - newBlock.header.timestamp)

    (for {
      _                <- EitherT(Task(metrics.blockSignaturesValidation.measureSuccessful(newBlock.signaturesValid())))
      validApplication <- EitherT(apply(blockchainUpdater, time, utxStorage, pos, scheduler)(newBlock))
    } yield validApplication).value.map {
      case Right(None) => // block already appended
      case Right(Some(_)) =>
        BlockStats.applied(newBlock, BlockStats.Source.Broadcast, blockchainUpdater.height)
        log.debug(s"${id(ch)} Appended $newBlock")
        if (newBlock.transactionData.isEmpty)
          allChannels.broadcast(BlockForged(newBlock), Some(ch))
        miner.scheduleMining()
      case Left(is: InvalidSignature) =>
        peerDatabase.blacklistAndClose(ch, s"Could not append $newBlock: $is")
      case Left(ve) =>
        BlockStats.declined(newBlock, BlockStats.Source.Broadcast)
        log.debug(s"${id(ch)} Could not append $newBlock generated by ${newBlock.header.signerData.generator}: $ve")
    }
  }

  private[this] object metrics {
    val blockSignaturesValidation = Kamon.timer("block-appender.block-signatures-validation")
    val blockReceivingLag         = Kamon.histogram("block-appender.receiving-lag")
    val blockProcessingTimeStats  = Kamon.timer("block-appender.processing-time")
  }
}
