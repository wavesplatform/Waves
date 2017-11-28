package com.wavesplatform.network

import com.wavesplatform.network.RxExtensionLoader.ApplierState.{Applying, ApplyingAndNext, Idle}
import com.wavesplatform.network.RxScoreObserver.{BestChannel, SyncWith}
import io.netty.channel._
import monix.eval.{Coeval, Task}
import monix.execution.{CancelableFuture, Scheduler}
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.transaction.History.BlockchainScore
import scorex.transaction.{NgHistory, ValidationError}
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

object RxExtensionLoader extends ScorexLogging {

  def apply(maxRollback: Int, syncTimeOut: FiniteDuration,
            history: NgHistory,
            peerDatabase: PeerDatabase,
            invalidBlocks: InvalidBlockStorage,
            bestChannel: Observable[SyncWith],
            blocks: Observable[(Channel, Block)],
            signatures: Observable[(Channel, Signatures)],
            channelClosed: Observable[Channel]
           )(extensionApplier: (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BlockchainScore]]]): Observable[(Channel, Block)] = {

    implicit val scheduler: SchedulerService = Scheduler.singleThread("rx-block-loader")

    val simpleBlocks = ConcurrentSubject.publish[(Channel, Block)]
    var state: State = State(LoaderState.Idle, ApplierState.Idle)
    val lastBestChannel: Coeval[Option[SyncWith]] = lastSeen(bestChannel)

    def loaderBecome(s: LoaderState): Unit = {
      state = state.copy(extensionLoaderState = s)
      log.trace(s"changing loader state to $state")
    }

    def applierBecome(s: ApplierState): Unit = {
      state = state.copy(extensionApplierState = s)
      log.trace(s"changing applier state to $state")
    }

    def blacklistOnTimeout(ch: Channel, reason: String): CancelableFuture[Unit] = Task {
      peerDatabase.blacklistAndClose(ch, reason)
    }.delayExecution(syncTimeOut).runAsync(scheduler)

    def tryRequestNextExt(): Unit = {
      lastBestChannel().get match {
        case None =>
          log.trace("Last bestChannel is None, state is up to date")
          loaderBecome(LoaderState.Idle)
        case Some(best) =>
          val lastBlockIds = state.extensionApplierState match {
            case Idle => Some(history.lastBlockIds(maxRollback))
            case Applying(ext) => Some(history.lastBlockIds(maxRollback - ext.blocks.size) ++ ext.blocks.map(_.uniqueId))
            case ApplyingAndNext(_, _, _) => None
          }
          lastBlockIds match {
            case Some(knownSigs) =>
              val ch = best.channel
              ch.writeAndFlush(GetSignatures(knownSigs))
              log.debug(s"${id(ch)} Requesting extension sigs, last ${knownSigs.length} are ${formatSignatures(knownSigs)}")
              loaderBecome(LoaderState.ExpectingSignatures(ch, knownSigs, blacklistOnTimeout(ch, s"Timeout loading extension")))
            case None =>
              loaderBecome(LoaderState.Idle)
          }
      }
    }

    def processExtensionRecieved(ch: Channel, ext: ExtensionBlocks): Task[Unit] = Task {
      state.extensionApplierState match {
        case Idle =>
          applierBecome(Applying(ext))
          extensionApplier(ch, ext).flatMap {
            case Left(err) => Task {
              log.warn(s"Failed to apply $ext because $err, discarding next extension if exists")
              applierBecome(Idle)
            }
            case Right(_) => Task {
              log.trace(s"Extension $ext successfully applied")
              state.extensionApplierState match {
                case Applying(app) if app == ext =>
                  applierBecome(Idle)
                  tryRequestNextExt()
                case ApplyingAndNext(app, next, nextChannel) if app == ext =>
                  applierBecome(Applying(next))
                  extensionApplier(nextChannel, next)
                  tryRequestNextExt()
              }
            }
          }
        case Applying(extension) =>
          Task(applierBecome(ApplyingAndNext(extension, next = ext, nextChannel = ch)))
      }
    }.flatten


    channelClosed.mapTask(ch => Task {
      state.extensionLoaderState match {
        case wp: LoaderState.WithPeer if wp.channel == ch =>
          wp.timeout.cancel()
          tryRequestNextExt()
        case _ =>
      }
    }).logErr.subscribe()

    bestChannel.mapTask(c => Task {
      log.trace(s"New bestChannel: $c, currentState = $state")
      c match {
        case Some(BestChannel(ch, _)) if state.extensionLoaderState == LoaderState.Idle => tryRequestNextExt()
        case _ =>
      }
    }).logErr.subscribe()

    signatures.mapTask { case ((ch, sigs)) => Task {
      state.extensionLoaderState match {
        case LoaderState.ExpectingSignatures(c, known, timeout) if c == ch =>
          timeout.cancel()
          val (_, unknown) = sigs.signatures.span(id => known.contains(id))
          sigs.signatures.find(invalidBlocks.contains) match {
            case Some(invalidBlock) =>
              peerDatabase.blacklistAndClose(ch, s"Signatures contain invalid block(s): $invalidBlock")
              loaderBecome(LoaderState.Idle)
            case None =>
              if (unknown.isEmpty) {
                log.trace(s"${id(ch)} Received empty extension signatures list, sync with node complete")
                loaderBecome(LoaderState.Idle)
              } else {
                unknown.foreach(s => ch.writeAndFlush(GetBlock(s)))
                loaderBecome(LoaderState.ExpectingBlocks(ch, unknown, unknown.toSet, Set.empty, blacklistOnTimeout(ch, "Timeout loading first requested block")))
              }
          }
        case _ => log.trace(s"${id(ch)} Received unexpected signatures, ignoring")
      }
    }
    }.logErr.subscribe()

    blocks.mapTask { case ((ch, block)) => Task {
      state.extensionLoaderState match {
        case LoaderState.ExpectingBlocks(c, requested, expected, recieved, timeout) if c == ch && expected.contains(block.uniqueId) =>
          timeout.cancel()
          if (expected == Set(block.uniqueId)) {
            log.debug(s"${id(ch)} Extension(blocks=${expected.size}) successfully received")
            val blockById = (recieved + block).map(b => b.uniqueId -> b).toMap
            val ext = requested.map(blockById)
            loaderBecome(LoaderState.Idle)
            tryRequestNextExt()
            processExtensionRecieved(ch, ExtensionBlocks(ext)).runAsync(scheduler)

          } else {
            loaderBecome(LoaderState.ExpectingBlocks(c, requested, expected - block.uniqueId, recieved + block, blacklistOnTimeout(ch,
              s"Timeout loading one of requested blocks; prev state=${state.extensionLoaderState}")))
          }
        case _ => simpleBlocks.onNext((ch, block))
      }
    }
    }.logErr.subscribe()

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
        s"received=${received.size}, expected=${expected.size})"
    }

  }

  case class ExtensionBlocks(blocks: Seq[Block]) {
    override def toString: String = if (blocks.isEmpty) "ExtensionBlocks()" else s"ExtensionBlocks(size =${blocks.size}," +
      s" [${blocks.head.uniqueId.trim}..${blocks.last.uniqueId.trim}])"
  }

  sealed trait ApplierState

  object ApplierState {

    case object Idle extends ApplierState

    case class Applying(extension: ExtensionBlocks) extends ApplierState

    case class ApplyingAndNext(extension: ExtensionBlocks, next: ExtensionBlocks, nextChannel: Channel) extends ApplierState

  }

  case class State(extensionLoaderState: LoaderState, extensionApplierState: ApplierState)

}