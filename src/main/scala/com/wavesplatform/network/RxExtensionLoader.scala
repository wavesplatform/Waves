package com.wavesplatform.network

import com.wavesplatform.network.RxExtensionLoader.ApplierState.Buffer
import com.wavesplatform.network.RxExtensionLoader.LoaderState.WithPeer
import com.wavesplatform.network.RxScoreObserver.{ChannelClosedAndSyncWith, SyncWith}
import io.netty.channel._
import monix.eval.{Coeval, Task}
import monix.execution.schedulers.SchedulerService
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.GenericError
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

    def scheduleBlacklist(ch: Channel, reason: String): Task[Unit] = Task {
      peerDatabase.blacklistAndClose(ch, reason)
    }.delayExecution(syncTimeOut)


    def syncNext(state: State, syncWith: SyncWith = lastSyncWith().flatten): State =
      syncWith match {
        case None =>
          log.trace("Last bestChannel is None, state is up to date")
          state.withIdleLoader
        case Some(best) =>
          state.loaderState match {
            case wp: WithPeer =>
              log.trace(s"${id(wp.channel)} Already syncing, no need to sync next, $state")
              state
            case LoaderState.Idle =>
              val maybeKnownSigs = state.applierState match {
                case ApplierState.Idle => Some((history.lastBlockIds(maxRollback), false))
                case ApplierState.Applying(None, ext) => Some((ext.blocks.map(_.uniqueId), true))
                case _ => None
              }
              maybeKnownSigs match {
                case Some((knownSigs, optimistic)) =>
                  val ch = best.channel
                  log.debug(s"${id(ch)} Requesting extension signatures ${if (optimistic) "optimistically" else ""}, last ${knownSigs.length} are ${formatSignatures(knownSigs)}")
                  val blacklisting = scheduleBlacklist(ch, s"Timeout loading extension").runAsync
                  Task(ch.writeAndFlush(GetSignatures(knownSigs))).runAsync
                  state.withLoaderState(LoaderState.ExpectingSignatures(ch, knownSigs, blacklisting))
                case None =>
                  log.trace(s"Holding on requesting next sigs, $state")
                  state
              }
          }
      }

    def onNewSyncWithChannelClosed(state: State, cc: ChannelClosedAndSyncWith): State = {
      cc match {
        case ChannelClosedAndSyncWith(_, None) =>
          state.loaderState match {
            case _: LoaderState.WithPeer => state.withIdleLoader
            case _ => state
          }
        case ChannelClosedAndSyncWith(None, Some(bestChannel)) =>
          log.trace(s"New SyncWith: $bestChannel, currentState = $state")
          syncNext(state, Some(bestChannel))
        case ChannelClosedAndSyncWith(Some(closedChannel), Some(bestChannel)) =>
          state.loaderState match {
            case wp: LoaderState.WithPeer if closedChannel != wp.channel => state
            case _ => syncNext(state.withIdleLoader, Some(bestChannel))
          }
      }
    }

    def onNewSignatures(state: State, ch: Channel, sigs: Signatures): State = {
      state.loaderState match {
        case LoaderState.ExpectingSignatures(c, known, _) if c == ch =>
          val (_, unknown) = sigs.signatures.span(id => known.contains(id))
          sigs.signatures.find(invalidBlocks.contains) match {
            case Some(invalidBlock) =>
              peerDatabase.blacklistAndClose(ch, s"Signatures contain invalid block(s): $invalidBlock")
              syncNext(state.withIdleLoader)
            case None =>
              if (unknown.isEmpty) {
                log.trace(s"${id(ch)} Received empty extension signatures list, sync with node complete")
                state.withIdleLoader
              } else {
                log.trace(s"${id(ch)} Requesting all required blocks(size=${unknown.size})")
                val blacklistingAsync = scheduleBlacklist(ch, "Timeout loading first requested block").runAsync
                Task(unknown.foreach(s => ch.writeAndFlush(GetBlock(s)))).runAsync
                state.withLoaderState(LoaderState.ExpectingBlocks(ch, unknown, unknown.toSet, Set.empty, blacklistingAsync))
              }
          }
        case _ =>
          log.trace(s"${id(ch)} Received unexpected signatures ${formatSignatures(sigs.signatures)}, ignoring at $state")
          state
      }
    }

    def onBlock(state: State, ch: Channel, block: Block): State = {
      state.loaderState match {
        case LoaderState.ExpectingBlocks(c, requested, expected, recieved, _) if c == ch && expected.contains(block.uniqueId) =>
          if (expected == Set(block.uniqueId)) {
            val blockById = (recieved + block).map(b => b.uniqueId -> b).toMap
            val ext = ExtensionBlocks(requested.map(blockById))
            log.debug(s"${id(ch)} $ext successfully received")
            extensionLoadingFinished(state.withIdleLoader, ext, ch)
          } else {
            val blacklistAsync = scheduleBlacklist(ch, s"Timeout loading one of requested blocks, non-received: ${
              val totalleft = expected.size - 1
              if (totalleft == 1) "one=" + requested.last.trim
              else "total=" + totalleft.toString
            }").runAsync
            state.withLoaderState(LoaderState.ExpectingBlocks(c, requested, expected - block.uniqueId, recieved + block, blacklistAsync))
          }
        case _ =>
          simpleBlocks.onNext((ch, block))
          state

      }
    }

    def extensionLoadingFinished(state: State, extension: ExtensionBlocks, ch: Channel): State = {
      state.applierState match {
        case ApplierState.Idle =>
          applyExtension(extension, ch)
          syncNext(state.copy(applierState = ApplierState.Applying(None, extension)))
        case ApplierState.Applying(None, applying) =>
          log.trace(s"Caching received $extension until $applying is executed")
          state.copy(applierState = ApplierState.Applying(Some(Buffer(ch, extension)), applying))
        case _ =>
          log.warn(s"Overflow, discarding $extension")
          state
      }
    }

    def applyExtension(extensionBlocks: ExtensionBlocks, ch: Channel): CancelableFuture[Unit] = {
      extensionApplier(ch, extensionBlocks)
        .onErrorHandle(err => {
          log.error("Error applying extension", err)
          Left(GenericError(err))
        })
        .flatMap { ar =>
          Task {
            s = onExtensionApplied(s, extensionBlocks, ch, ar)
          }.executeOn(scheduler)
        }.logErr.runAsync(scheduler)
    }

    def onExtensionApplied(state: State, extension: ExtensionBlocks, ch: Channel, applicationResult: Either[ValidationError, Option[BlockchainScore]]): State = {
      log.trace(s"Applying $extension finished with $applicationResult")
      state.applierState match {
        case ApplierState.Idle =>
          log.warn(s"Applied $extension but ApplierState is Idle")
          state
        case ApplierState.Applying(maybeBuffer, applying) =>
          if (applying != extension) log.warn(s"Applied $extension doesn't match expected $applying")
          maybeBuffer match {
            case None => syncNext(state.copy(applierState = ApplierState.Idle))
            case Some(Buffer(nextChannel, nextExtension)) => applicationResult match {
              case Left(_) =>
                log.debug(s"Failed to apply $extension, discarding cached as well")
                syncNext(state.copy(applierState = ApplierState.Idle))
              case Right(_) =>
                log.trace(s"Successfully applied $extension, staring to apply cached")
                applyExtension(nextExtension, nextChannel)
                syncNext(state.copy(applierState = ApplierState.Applying(None, nextExtension)))
            }
          }
      }
    } tap (ns => log.trace(s"State after application: $ns"))

    Observable
      .merge(
        signatures.observeOn(scheduler).map { case ((ch, sigs)) => s = onNewSignatures(s, ch, sigs) },
        blocks.observeOn(scheduler).map { case ((ch, block)) => s = onBlock(s, ch, block) },
        syncWithChannelClosed.observeOn(scheduler).map { ch => s = onNewSyncWithChannelClosed(s, ch) }
      )
      .map { _ => log.trace(s"Current state: $s") }
      .logErr
      .subscribe()(scheduler)

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

  case class State(loaderState: LoaderState, applierState: ApplierState) {
    def withLoaderState(newLoaderState: LoaderState): State = {
      loaderState match {
        case wp: WithPeer => wp.timeout.cancel()
        case _ =>
      }
      State(newLoaderState, applierState)
    }

    def withIdleLoader: State = withLoaderState(LoaderState.Idle)
  }

  sealed trait ApplierState

  object ApplierState {

    case object Idle extends ApplierState

    case class Buffer(ch: Channel, ext: ExtensionBlocks) {
      override def toString: String = s"Buffer($ext from ${id(ch)})"
    }

    case class Applying(buf: Option[Buffer], applying: ExtensionBlocks) extends ApplierState

  }

}