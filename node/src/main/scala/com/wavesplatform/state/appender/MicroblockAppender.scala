package com.wavesplatform.state.appender

import cats.syntax.either.*
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{MicroBlock, MicroBlockSnapshot}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.*
import com.wavesplatform.mining.BlockChallenger
import com.wavesplatform.network.*
import com.wavesplatform.network.MicroBlockSynchronizer.MicroblockData
import com.wavesplatform.protobuf.PBSnapshots
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.transaction.TxValidationError.{InvalidSignature, InvalidStateHash}
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler

import scala.util.{Left, Right}

object MicroblockAppender extends ScorexLogging {
  private val microblockProcessingTimeStats = Kamon.timer("microblock-appender.processing-time").withoutTags()

  def apply(blockchainUpdater: BlockchainUpdater & Blockchain, utxStorage: UtxPool, verify: Boolean)(
      microBlock: MicroBlock,
      snapshot: Option[MicroBlockSnapshot]
  ): Either[ValidationError, BlockId] =
    microblockProcessingTimeStats.measureSuccessful {
      blockchainUpdater
        .processMicroBlock(microBlock, snapshot, verify)
        .map { totalBlockId =>
          if (microBlock.transactionData.nonEmpty) {
            utxStorage.removeAll(microBlock.transactionData)
            log.trace(
              s"Removing txs of ${microBlock.stringRepr(totalBlockId)} ${microBlock.transactionData.map(_.id()).mkString("(", ", ", ")")} from UTX pool"
            )
          }

          utxStorage.scheduleCleanup()
          totalBlockId
        }
    }

  def apply(blockchainUpdater: BlockchainUpdater & Blockchain, utxStorage: UtxPool, scheduler: Scheduler, verify: Boolean = true)(
      microBlock: MicroBlock,
      snapshot: Option[MicroBlockSnapshot]
  ): Task[Either[ValidationError, BlockId]] =
    Task(apply(blockchainUpdater, utxStorage, verify)(microBlock, snapshot)).executeOn(scheduler)

  def apply2(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      utxStorage: UtxPool,
      allChannels: ChannelGroup,
      peerDatabase: PeerDatabase,
      blockChallenger: Option[BlockChallenger],
  )(md: MicroblockData): Unit = {
    val microblockTotalResBlockSig = md.microBlock.totalResBlockSig

    val blockIdAfterApplyingMicroblock = for {
      _ <- Either.raiseUnless(md.microBlock.signatureValid())(InvalidSignature(md.microBlock))
      microBlockSnapshot = md.snapshot
        .map(s =>
          md.microBlock.transactionData.zip(s.snapshotResponse.snapshots).map { case (tx, pbs) =>
            PBSnapshots.fromProtobuf(pbs, tx.id(), Height(blockchainUpdater.height))
          }
        )
        .map(ss => MicroBlockSnapshot(microblockTotalResBlockSig, ss))
      blockId <- apply(blockchainUpdater, utxStorage, verify = true)(md.microBlock, microBlockSnapshot)
    } yield blockId

    blockIdAfterApplyingMicroblock match {
      case Right(blockId) =>
        allChannels.broadcast(md.inv, except = md.owners())
        BlockStats.applied(md.microBlock, blockId)
      case Left(is: InvalidSignature) =>
        md.source.foreach(source => peerDatabase.blacklistAndClose(source, s"Could not append microblock ${md.inv.totalBlockId}: $is"))
      case Left(ish: InvalidStateHash) =>
        (md.source ++ md.snapshot.map(_.snapshotSource)).foreach { ch =>
          peerDatabase.blacklistAndClose(
            ch,
            s"Could not append microblock ${md.inv.totalBlockId}: $ish"
          )
        }

        BlockStats.declined(md.inv.totalBlockId)

        blockChallenger.foreach(_.challengeMicroblock(md))

      case Left(ve) =>
        BlockStats.declined(md.inv.totalBlockId)
        log.debug(s"${md.source.fold("")(src => id(src) + " ")}Could not append microblock ${md.inv.totalBlockId}: $ve")
    }

  }

  def apply(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      utxStorage: UtxPool,
      allChannels: ChannelGroup,
      peerDatabase: PeerDatabase,
      blockChallenger: Option[BlockChallenger],
      scheduler: Scheduler
  )(md: MicroblockData.Remote): Task[Unit] =
    Task(apply2(blockchainUpdater, utxStorage, allChannels, peerDatabase, blockChallenger)(md)).executeOn(scheduler)
}
