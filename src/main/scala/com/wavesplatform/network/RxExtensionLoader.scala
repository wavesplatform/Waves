package com.wavesplatform.network

import com.wavesplatform.network.RxExtensionLoader.ApplierState.{Applying, Buffered, Idle}
import com.wavesplatform.network.RxScoreObserver.SyncWith
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
            syncWith: Observable[SyncWith],
            blocks: Observable[(Channel, Block)],
            signatures: Observable[(Channel, Signatures)],
            channelClosed: Observable[Channel]
           )(extensionApplier: (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BlockchainScore]]]): Observable[(Channel, Block)] = {

    implicit val scheduler: SchedulerService = Scheduler.singleThread("rx-extension-loader")

    val simpleBlocks = ConcurrentSubject.publish[(Channel, Block)]
    var s: State = State(LoaderState.Idle, ApplierState.Idle)
    val lastBestChannel: Coeval[Option[SyncWith]] = lastSeen(syncWith)

    def blacklistOnTimeout(ch: Channel, reason: String): CancelableFuture[Unit] = Task {
      peerDatabase.blacklistAndClose(ch, reason)
    }.delayExecution(syncTimeOut)
      .runAsync(scheduler)

    def requestNext(state: State): State = {
      lastBestChannel().get match {
        case None =>
          log.trace("Last bestChannel is None, state is up to date")
          state.copy(loaderState = LoaderState.Idle)
        case Some(best) =>
          val maybeKnownSigs = state.applierState match {
            case ApplierState.Idle => Some(history.lastBlockIds(maxRollback))
            case ApplierState.Applying(ext) => Some(history.lastBlockIds(maxRollback - ext.blocks.size) ++ ext.blocks.map(_.uniqueId))
            case _ => None
          }
          maybeKnownSigs match {
            case Some(knownSigs) =>
              val ch = best.channel
              log.debug(s"${id(ch)} Requesting extension sigs, last ${knownSigs.length} are ${formatSignatures(knownSigs)}")
              Task(ch.writeAndFlush(GetSignatures(knownSigs))).runAsync
              state.copy(loaderState = LoaderState.ExpectingSignatures(ch, knownSigs, blacklistOnTimeout(ch, s"Timeout loading extension")))
            case None =>
              log.trace(s"Holding on requesting next sigs, $state")
              state
          }
      }
    }

    def onChannelClosed(state: State, ch: Channel): State = {
      state.loaderState match {
        case wp: LoaderState.WithPeer if wp.channel == ch =>
          log.debug(s"Current channel ${id(ch)} has been closed while $state")
          wp.timeout.cancel()
          requestNext(state.copy(loaderState = LoaderState.Idle))
        case _ =>
          log.trace(s"Unrelated channel ${id(ch)} has been closed while $state")
          state
      }
    }

    def onNewSyncWith(state: State, sw: SyncWith): State = {
      log.trace(s"New SyncWith: $sw, currentState = $state")
      sw match {
        case Some(_) if state.loaderState == LoaderState.Idle =>
          requestNext(state)
        case _ => state
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
              requestNext(state.copy(loaderState = LoaderState.Idle))
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
          log.trace(s"${id(ch)} Received unexpected signatures, ignoring at $state")
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
                val s = expected.size
                if (s == 1) "one=" + requested.last.trim
                else "total=" + s.toString
              }")))
          }
        case _ =>
          simpleBlocks.onNext((ch, block))
          state

      }
    }

    def extensionLoadingFinished(state: State, extension: ExtensionBlocks, ch: Channel): State = {
      state.applierState match {
        case Idle =>
          val newState = state.copy(applierState = Applying(extension))
          applyExtension(extension, ch)
          newState
        case Applying(applying) =>
          log.trace(s"Caching recieved $extension until $applying is executed")
          state.copy(applierState = Buffered(ch, extension, applying))
        case Buffered(_, _, _) =>
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
        case Idle =>
          log.warn(s"Applying $extension finished with $applicationResult, but applierState is Idle")
          state
        case Applying(_) =>
          log.trace(s"Applying $extension finished with $applicationResult, no cached")
          requestNext(state.copy(applierState = ApplierState.Idle))
        case Buffered(nextChannel, nextExtension, applying) => applicationResult match {
          case Left(_) =>
            log.debug(s"Falied to apply $extension, discarding cached as well")
            requestNext(state.copy(applierState = ApplierState.Idle))
          case Right(_) =>
            log.trace(s"Successfully applied $extension, staring to apply cached")
            val newState = requestNext(state.copy(applierState = Applying(nextExtension)))
            applyExtension(nextExtension, nextChannel)
            newState
        }
      }
    }

    Observable.merge(
      syncWith.map(c => onNewSyncWith(s, c)),
      signatures.map { case ((ch, sigs)) => onNewSignatures(s, ch, sigs) },
      blocks.map { case ((ch, block)) => onBlock(s, ch, block) },
      channelClosed.map(ch => onChannelClosed(s, ch)))
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
    override def toString: String = if (blocks.isEmpty) "ExtensionBlocks()" else s"ExtensionBlocks(size =${blocks.size}," +
      s" [${blocks.head.uniqueId.trim} -- ${blocks.last.uniqueId.trim}])"
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