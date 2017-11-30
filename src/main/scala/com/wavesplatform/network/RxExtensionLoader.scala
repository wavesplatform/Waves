package com.wavesplatform.network

import com.wavesplatform.network.RxScoreObserver.{BestChannel, ChannelClosedAndSyncWith, SyncWith}
import io.netty.channel._
import monix.eval.{Coeval, Task}
import monix.execution.schedulers.SchedulerService
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.transaction.History.BlockchainScore
import scorex.transaction.{NgHistory, ValidationError}
import scorex.utils.ScorexLogging

import scala.concurrent.duration._

object RxExtensionLoader extends ScorexLogging {

  def apply(maxRollback: Int, syncTimeOut: FiniteDuration,
            history: NgHistory,
            peerDatabase: PeerDatabase,
            invalidBlocks: InvalidBlockStorage,
            blocks: Observable[(Channel, Block)],
            signatures: Observable[(Channel, Signatures)],
            syncWithChannelClosed: Observable[ChannelClosedAndSyncWith]
           )(extensionApplier: (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BlockchainScore]]]): Observable[(Channel, Block)] = {

    implicit val scheduler: SchedulerService = Scheduler.singleThread("rx-extension-loader")

    val simpleBlocks = ConcurrentSubject.publish[(Channel, Block)]
    var s: State = State(LoaderState.Idle, ApplierState.Idle)
    val lastSyncWith: Coeval[Option[SyncWith]] = lastObserved(syncWithChannelClosed.map(_.syncWith))

    def blacklistOnTimeout(ch: Channel, reason: String): CancelableFuture[Unit] = Task {
      peerDatabase.blacklistAndClose(ch, reason)
    }.delayExecution(syncTimeOut)
      .runAsync(scheduler)

    def syncNext(state: State, syncWith: SyncWith): State =
      syncWith match {
        case None =>
          log.trace("Last bestChannel is None, state is up to date")
          state.copy(loaderState = LoaderState.Idle)
        case Some(best) => sync(state, best)
      }

    def sync(state: State, best: BestChannel): State = {
      val maybeKnownSigs = state.applierState match {
        case ApplierState.Idle => Some((history.lastBlockIds(maxRollback), false))
        case ApplierState.Applying(ext) => Some((history.lastBlockIds(maxRollback - ext.blocks.size) ++ ext.blocks.map(_.uniqueId), true))
        case _ => None
      }
      maybeKnownSigs match {
        case Some((knownSigs, optimistic)) =>
          val ch = best.channel
          log.debug(s"${id(ch)} Requesting extension signatures ${if (optimistic) "optimistically" else ""}, last ${knownSigs.length} are ${formatSignatures(knownSigs)}")
          Task(ch.writeAndFlush(GetSignatures(knownSigs))).runAsync
          state.copy(loaderState = LoaderState.ExpectingSignatures(ch, knownSigs, blacklistOnTimeout(ch, s"Timeout loading extension")))
        case None =>
          log.trace(s"Holding on requesting next sigs, $state")
          state
      }
    }


    def onNewSyncWithChannelClosed(state: State, cc: ChannelClosedAndSyncWith): State = {
      cc match {
        case ChannelClosedAndSyncWith(_, None) =>
          state.loaderState match {
            case wp: LoaderState.WithPeer =>
              wp.timeout.cancel()
              state.copy(loaderState = LoaderState.Idle)
            case _ => state
          }
        case ChannelClosedAndSyncWith(None, Some(bestChannel)) =>
          log.trace(s"New SyncWith: $bestChannel, currentState = $state")
          if (state.loaderState == LoaderState.Idle)
            sync(state, bestChannel)
          else state
        case ChannelClosedAndSyncWith(Some(closedChannel), Some(bestChannel)) =>
          state.loaderState match {
            case wp: LoaderState.WithPeer if closedChannel == wp.channel =>
              wp.timeout.cancel()
              sync(state.copy(loaderState = LoaderState.Idle), bestChannel)
            case LoaderState.Idle =>
              sync(state, bestChannel)
            case _ => state
          }
      }
    }

    def onNewSignatures(state: State, ch: Channel, sigs: Signatures): State = {
      state.loaderState match {
        case LoaderState.ExpectingSignatures(c, known, timeout) if c == ch =>
          timeout.cancel()
          val (_, unknown) = sigs.signatures.span(id => known.contains(id))
          sigs.signatures.find(invalidBlocks.contains) match {
            case Some(invalidBlock) =>
              peerDatabase.blacklistAndClose(ch, s"Signatures contain invalid block(s): $invalidBlock")
              syncNext(state.copy(loaderState = LoaderState.Idle), lastSyncWith().flatten)
            case None =>
              if (unknown.isEmpty) {
                log.trace(s"${id(ch)} Received empty extension signatures list, sync with node complete")
                state.copy(loaderState = LoaderState.Idle)
              } else {
                log.trace(s"${id(ch)} Requesting all required blocks(size=${unknown.size})")
                Task(unknown.foreach(s => ch.writeAndFlush(GetBlock(s)))).runAsync
                state.copy(loaderState = LoaderState.ExpectingBlocks(ch, unknown, unknown.toSet, Set.empty, blacklistOnTimeout(ch, "Timeout loading first requested block")))
              }
          }
        case _ =>
          log.trace(s"${id(ch)} Received unexpected signatures ${formatSignatures(sigs.signatures)}, ignoring at $state")
          state
      }
    }

    def onBlock(state: State, ch: Channel, block: Block): State = {
      state.loaderState match {
        case LoaderState.ExpectingBlocks(c, requested, expected, recieved, timeout) if c == ch && expected.contains(block.uniqueId) =>
          timeout.cancel()
          if (expected == Set(block.uniqueId)) {
            val blockById = (recieved + block).map(b => b.uniqueId -> b).toMap
            val ext = ExtensionBlocks(requested.map(blockById))
            log.debug(s"${id(ch)} $ext successfully received")
            extensionLoadingFinished(state.copy(loaderState = LoaderState.Idle), ext, ch)
          } else {
            state.copy(loaderState = LoaderState.ExpectingBlocks(c, requested, expected - block.uniqueId, recieved + block,
              blacklistOnTimeout(ch, s"Timeout loading one of requested blocks, non-received: ${
                val totalleft = expected.size - 1
                if (totalleft == 1) "one=" + requested.last.trim
                else "total=" + totalleft.toString
              }")))
          }
        case _ =>
          simpleBlocks.onNext((ch, block))
          state

      }
    }

    def extensionLoadingFinished(state: State, extension: ExtensionBlocks, ch: Channel): State = {
      state.applierState match {
        case ApplierState.Idle =>
          val newState = state.copy(applierState = ApplierState.Applying(extension))
          applyExtension(extension, ch)
          newState
        case ApplierState.Applying(applying) =>
          log.trace(s"Caching recieved $extension until $applying is executed")
          state.copy(applierState = ApplierState.Buffered(ch, extension, applying))
        case ApplierState.Buffered(_, _, _) =>
          log.warn(s"Overflow, discarding $extension")
          state
      }
    }

    def applyExtension(extensionBlocks: ExtensionBlocks, ch: Channel): Unit = {
      extensionApplier(ch, extensionBlocks).flatMap { ar =>
        Task {
          s = onExetensionApplied(s, extensionBlocks, ch, ar)
        }.executeOn(scheduler)
      }.runAsync(scheduler)
    }

    def onExetensionApplied(state: State, extension: ExtensionBlocks, ch: Channel, applicationResult: Either[ValidationError, Option[BlockchainScore]]): State = {
      state.applierState match {
        case ApplierState.Idle =>
          log.warn(s"Applying $extension finished with $applicationResult, but applierState is Idle")
          state
        case ApplierState.Applying(_) =>
          log.trace(s"Applying $extension finished with $applicationResult, no cached")
          syncNext(state.copy(applierState = ApplierState.Idle), lastSyncWith().flatten)
        case ApplierState.Buffered(nextChannel, nextExtension, _) => applicationResult match {
          case Left(_) =>
            log.debug(s"Falied to apply $extension, discarding cached as well")
            syncNext(state.copy(applierState = ApplierState.Idle), lastSyncWith().flatten)
          case Right(_) =>
            log.trace(s"Successfully applied $extension, staring to apply cached")
            val newState = syncNext(state.copy(applierState = ApplierState.Applying(nextExtension)), lastSyncWith().flatten)
            applyExtension(nextExtension, nextChannel)
            newState
        }
      }
    }

    Observable.merge(
      signatures.map { case ((ch, sigs)) => onNewSignatures(s, ch, sigs) },
      blocks.map { case ((ch, block)) => onBlock(s, ch, block) },
      syncWithChannelClosed.map(ch => onNewSyncWithChannelClosed(s, ch)))
      .map(s = _)
      .logErr
      .subscribe()

    simpleBlocks
  }

  sealed trait LoaderState

  object LoaderState {

    sealed trait WithPeer extends LoaderState {
      def channel: Channel

      def timeout: CancelableFuture[Unit]
    }

    case object Idle extends LoaderState

    case class ExpectingSignatures(channel: Channel, known: Seq[BlockId], timeout: CancelableFuture[Unit]) extends WithPeer {
      override def toString: String = s"ExpectingSignatures(channel=${id(channel)})"
    }

    case class ExpectingBlocks(channel: Channel, allBlocks: Seq[BlockId],
                               expected: Set[BlockId],
                               received: Set[Block],
                               timeout: CancelableFuture[Unit]) extends WithPeer {
      override def toString: String = s"ExpectingBlocks(channel=${id(channel)}, totalBlocks=${allBlocks.size}, " +
        s"received=${received.size}, expected=${if (expected.size == 1) expected.head.trim else expected.size})"
    }

  }

  case class ExtensionBlocks(blocks: Seq[Block]) {
    override def toString: String = s"ExtensionBlocks(${formatSignatures(blocks.map(_.uniqueId))}"
  }

  case class State(loaderState: LoaderState, applierState: ApplierState)

  sealed trait ApplierState

  case object ApplierState {

    case object Idle extends ApplierState

    case class Applying(applying: ExtensionBlocks) extends ApplierState

    case class Buffered(nextChannel: Channel, nextExtension: ExtensionBlocks, applying: ExtensionBlocks) extends ApplierState {
      override def toString: String = s"Buffered(nextChannel: ${id(nextChannel)}, nextExtension: $nextExtension, applying: $applying)"
    }

  }

}