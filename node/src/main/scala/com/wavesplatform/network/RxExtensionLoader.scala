package com.wavesplatform.network

import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.RxExtensionLoader.ApplierState.Buffer
import com.wavesplatform.network.RxExtensionLoader.LoaderState.WithPeer
import com.wavesplatform.network.RxScoreObserver.{ChannelClosedAndSyncWith, SyncWith}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel._
import monix.eval.{Coeval, Task}
import monix.execution.CancelableFuture
import monix.execution.schedulers.SchedulerService
import monix.reactive.subjects.{ConcurrentSubject, Subject}
import monix.reactive.{Observable, Observer}

import scala.concurrent.duration._

object RxExtensionLoader extends ScorexLogging {

  type ApplyExtensionResult = Either[ValidationError, Option[BigInt]]

  def apply(
      syncTimeOut: FiniteDuration,
      lastBlockIds: Coeval[Seq[ByteStr]],
      peerDatabase: PeerDatabase,
      invalidBlocks: InvalidBlockStorage,
      blocks: Observable[(Channel, Block)],
      signatures: Observable[(Channel, Signatures)],
      syncWithChannelClosed: Observable[ChannelClosedAndSyncWith],
      scheduler: SchedulerService,
      timeoutSubject: Subject[Channel, Channel]
  )(
      extensionApplier: (Channel, ExtensionBlocks) => Task[ApplyExtensionResult]
  ): (Observable[(Channel, Block)], Coeval[State], RxExtensionLoaderShutdownHook) = {

    implicit val schdlr: SchedulerService = scheduler

    val extensions: ConcurrentSubject[(Channel, ExtensionBlocks), (Channel, ExtensionBlocks)] = ConcurrentSubject.publish[(Channel, ExtensionBlocks)]
    val simpleBlocks: ConcurrentSubject[(Channel, Block), (Channel, Block)]                   = ConcurrentSubject.publish[(Channel, Block)]
    @volatile var stateValue: State                                                           = State(LoaderState.Idle, ApplierState.Idle)
    val lastSyncWith: Coeval[Option[SyncWith]]                                                = lastObserved(syncWithChannelClosed.map(_.syncWith))

    def scheduleBlacklist(ch: Channel, reason: String): Task[Unit] =
      Task {
        timeoutSubject.onNext(ch)
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
                case ApplierState.Idle                => Some((lastBlockIds(), false))
                case ApplierState.Applying(None, ext) => Some((ext.blocks.map(_.uniqueId).reverse, true))
                case _                                => None
              }
              maybeKnownSigs match {
                case Some((knownSigs, optimistic)) =>
                  val ch = best.channel
                  log.debug(
                    s"${id(ch)} Requesting signatures${if (optimistic) " optimistically" else ""}, last ${knownSigs.length} are ${formatSignatures(knownSigs)}"
                  )

                  val blacklisting = scheduleBlacklist(ch, s"Timeout loading extension").runAsyncLogErr
                  ch.writeAndFlush(GetSignatures(knownSigs)).addListener { f: ChannelFuture =>
                    if (!f.isSuccess) log.trace(s"Error requesting signatures: $ch", f.cause())
                  }

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
            case _                       => state
          }
        case ChannelClosedAndSyncWith(None, Some(bestChannel)) =>
          log.trace(s"New SyncWith: $bestChannel, currentState = $state")
          syncNext(state, Some(bestChannel))
        case ChannelClosedAndSyncWith(Some(closedChannel), Some(bestChannel)) =>
          state.loaderState match {
            case wp: LoaderState.WithPeer if closedChannel != wp.channel => state
            case _ =>
              log.trace(s"Switching to next best channel: state=$state, cc=$cc, bestChannel=$bestChannel")
              syncNext(state.withIdleLoader, Some(bestChannel))
          }
      }
    }

    def onNewSignatures(state: State, ch: Channel, sigs: Signatures): State = {
      state.loaderState match {
        case LoaderState.ExpectingSignatures(c, known, _) if c == ch =>
          val (_, unknown) = sigs.signatures.span(id => known.contains(id))

          val firstInvalid = sigs.signatures.view.flatMap { sig =>
            invalidBlocks.find(sig).map(sig -> _)
          }.headOption

          firstInvalid match {
            case Some((invalidBlock, reason)) =>
              peerDatabase.blacklistAndClose(ch, s"Signatures contain invalid block(s): $invalidBlock, $reason")
              syncNext(state.withIdleLoader)
            case None =>
              if (unknown.isEmpty) {
                log.trace(s"${id(ch)} Received empty extension signatures list, sync with node complete")
                state.withIdleLoader
              } else {
                log.trace(s"${id(ch)} Requesting ${unknown.size} blocks")
                val blacklistingAsync = scheduleBlacklist(ch, "Timeout loading first requested block").runAsyncLogErr
                unknown.foreach(s => ch.write(GetBlock(s)))
                ch.flush()
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
            val ext       = ExtensionBlocks(requested.map(blockById))
            log.debug(s"${id(ch)} $ext successfully received")
            extensionLoadingFinished(state.withIdleLoader, ext, ch)
          } else {
            val blacklistAsync = scheduleBlacklist(
              ch,
              s"Timeout loading one of requested blocks, non-received: ${
                val totalleft = expected.size - 1
                if (totalleft == 1) "one=" + requested.last.trim
                else "total=" + totalleft.toString
              }"
            ).runAsyncLogErr
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
          extensions.onNext(ch -> extension)
          syncNext(state.copy(applierState = ApplierState.Applying(None, extension)))
        case s @ ApplierState.Applying(None, applying) =>
          log.trace(s"An optimistic extension was received: $extension, but applying $applying now")
          state.copy(applierState = s.copy(buf = Some(Buffer(ch, extension))))
        case _ =>
          log.warn(s"Overflow, discarding $extension")
          state
      }
    }

    def onExtensionApplied(state: State, extension: ExtensionBlocks, ch: Channel, applicationResult: ApplyExtensionResult): State = {
      log.trace(s"Applying $extension finished with $applicationResult")
      state.applierState match {
        case ApplierState.Idle =>
          log.warn(s"Applied $extension but ApplierState is Idle")
          state
        case ApplierState.Applying(maybeBuffer, applying) =>
          if (applying != extension) log.warn(s"Applied $extension doesn't match expected $applying")
          maybeBuffer match {
            case None => state.copy(applierState = ApplierState.Idle)
            case Some(Buffer(nextChannel, nextExtension)) =>
              applicationResult match {
                case Left(_) =>
                  log.debug(s"Failed to apply $extension, discarding cached as well")
                  syncNext(state.copy(applierState = ApplierState.Idle))
                case Right(_) =>
                  log.trace(s"Successfully applied $extension, starting to apply an optimistically loaded one: $nextExtension")
                  extensions.onNext(nextChannel -> nextExtension)
                  syncNext(state.copy(applierState = ApplierState.Applying(None, nextExtension)))
              }
          }
      }
    }

    def appliedExtensions: Observable[(Channel, ExtensionBlocks, ApplyExtensionResult)] = {
      def apply(x: (Channel, ExtensionBlocks)): Task[ApplyExtensionResult] = Function.tupled(extensionApplier)(x)

      extensions.mapEval { x =>
        apply(x)
          .asyncBoundary(scheduler)
          .onErrorHandle { err =>
            log.error("Error during extension applying", err)
            Left(GenericError(err))
          }
          .map((x._1, x._2, _))
      }
    }

    Observable(
      signatures.observeOn(scheduler).map { case ((ch, sigs)) => stateValue = onNewSignatures(stateValue, ch, sigs) },
      blocks.observeOn(scheduler).map { case ((ch, block))    => stateValue = onBlock(stateValue, ch, block) },
      syncWithChannelClosed.observeOn(scheduler).map { ch =>
        stateValue = onNewSyncWithChannelClosed(stateValue, ch)
      },
      appliedExtensions.map { case ((ch, extensionBlocks, ar)) => stateValue = onExtensionApplied(stateValue, extensionBlocks, ch, ar) }
    ).merge
      .map { _ =>
        log.trace(s"Current state: $stateValue")
      }
      .logErr
      .subscribe()

    (simpleBlocks, Coeval.eval(stateValue), RxExtensionLoaderShutdownHook(extensions, simpleBlocks))
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

    case class ExpectingBlocks(
        channel: Channel,
        allBlocks: Seq[BlockId],
        expected: Set[BlockId],
        received: Set[Block],
        timeout: CancelableFuture[Unit]
    ) extends WithPeer {
      override def toString: String =
        s"ExpectingBlocks(channel=${id(channel)}, totalBlocks=${allBlocks.size}, " +
          s"received=${received.size}, expected=${if (expected.size == 1) expected.head.trim else expected.size})"
    }

  }

  case class RxExtensionLoaderShutdownHook(extensionChannel: Observer[(Channel, ExtensionBlocks)], simpleBlocksChannel: Observer[(Channel, Block)]) {
    def shutdown(): Unit = {
      extensionChannel.onComplete()
      simpleBlocksChannel.onComplete()
    }
  }

  case class ExtensionBlocks(blocks: Seq[Block]) {
    override def toString: String = s"ExtensionBlocks(${formatSignatures(blocks.map(_.uniqueId))}"
  }

  case class State(loaderState: LoaderState, applierState: ApplierState) {
    def withLoaderState(newLoaderState: LoaderState): State = {
      loaderState match {
        case wp: WithPeer => wp.timeout.cancel()
        case _            =>
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
