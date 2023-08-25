package com.wavesplatform.state.appender

import java.time.Instant
import cats.data.EitherT
import com.wavesplatform.block.Block
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.*
import com.wavesplatform.mining.BlockChallenger
import com.wavesplatform.network.*
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, GenericError, InvalidSignature, InvalidStateHash}
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxForAppender
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import kamon.trace.Span
import monix.eval.Task
import monix.execution.Scheduler

object BlockAppender extends ScorexLogging {
  def apply(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      time: Time,
      utxStorage: UtxForAppender,
      pos: PoSSelector,
      scheduler: Scheduler,
      verify: Boolean = true,
      txSignParCheck: Boolean = true
  )(newBlock: Block): Task[Either[ValidationError, Option[BigInt]]] =
    Task {
      if (
        blockchainUpdater
          .isLastBlockId(newBlock.header.reference) || blockchainUpdater.lastBlockHeader.exists(_.header.reference == newBlock.header.reference)
      ) {
        if (newBlock.header.challengedHeader.isDefined) {
          appendChallengeBlock(blockchainUpdater, utxStorage, pos, time, verify, txSignParCheck)(newBlock).map(_ => Some(blockchainUpdater.score))
        } else {
          appendKeyBlock(blockchainUpdater, utxStorage, pos, time, verify, txSignParCheck)(newBlock).map(_ => Some(blockchainUpdater.score))
        }
      } else if (blockchainUpdater.contains(newBlock.id()) || blockchainUpdater.isLastBlockId(newBlock.id()))
        Right(None)
      else
        Left(BlockAppendError("Block is not a child of the last block or its parent", newBlock))
    }.executeOn(scheduler)

  def apply(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      time: Time,
      utxStorage: UtxForAppender,
      pos: PoSSelector,
      allChannels: ChannelGroup,
      peerDatabase: PeerDatabase,
      blockChallenger: BlockChallenger,
      scheduler: Scheduler
  )(ch: Channel, newBlock: Block, snapshot: Option[BlockSnapshot]): Task[Unit] = {
    import metrics.*
    implicit val implicitTime: Time = time

    val span = createApplySpan(newBlock)
    span.markNtp("block.received")

    val append =
      (for {
        _ <- EitherT(Task(Either.cond(newBlock.signatureValid(), (), GenericError("Invalid block signature"))))
        _ = span.markNtp("block.signatures-validated")
        validApplication <- EitherT(apply(blockchainUpdater, time, utxStorage, pos, scheduler)(newBlock))
      } yield validApplication).value

    val handle = append.asyncBoundary.flatMap {
      case Right(None) => Task.unit // block already appended
      case Right(Some(_)) =>
        Task {
          log.debug(s"${id(ch)} Appended $newBlock")

          span.markNtp("block.applied")
          span.finishNtp()
          BlockStats.applied(newBlock, BlockStats.Source.Broadcast, blockchainUpdater.height)
          if (newBlock.transactionData.isEmpty || newBlock.header.challengedHeader.isDefined) {
            allChannels.broadcast(BlockForged(newBlock), Some(ch)) // Key block or challenging block
          }
        }
      case Left(is: InvalidSignature) =>
        Task(peerDatabase.blacklistAndClose(ch, s"Could not append $newBlock: $is"))
      case Left(ish: InvalidStateHash) =>
        peerDatabase.blacklistAndClose(ch, s"Could not append $newBlock: $ish")

        span.markNtp("block.declined")
        span.fail(ish.toString)
        span.finishNtp()
        BlockStats.declined(newBlock, BlockStats.Source.Broadcast)

        blockChallenger.challengeBlock(newBlock, ch)

      case Left(ve) =>
        Task {
          log.debug(s"${id(ch)} Could not append $newBlock: $ve")

          span.markNtp("block.declined")
          span.fail(ve.toString)
          span.finishNtp()

          BlockStats.declined(newBlock, BlockStats.Source.Broadcast)
        }
    }

    handle
      .onErrorHandle(e => log.warn("Error happened after block appending", e))
  }

  // noinspection TypeAnnotation,ScalaStyle
  private[this] object metrics {
    def createApplySpan(block: Block) = {
      Kamon
        .spanBuilder("block-appender")
        .tag("id", BlockStats.id(block.id()))
        .tag("parent-id", BlockStats.id(block.header.reference))
        .start(Instant.ofEpochMilli(block.header.timestamp))
    }

    implicit class SpanExt(private val span: Span) extends AnyVal {
      def markNtp(name: String)(implicit time: Time): Span =
        span.mark(name, ntpTime)

      def finishNtp()(implicit time: Time): Unit =
        span.finish(ntpTime)

      private[this] def ntpTime(implicit time: Time) =
        Instant.ofEpochMilli(time.correctedTime())
    }
  }
}
