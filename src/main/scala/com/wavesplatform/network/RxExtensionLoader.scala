package com.wavesplatform.network

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

import scala.concurrent.duration._

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
    var state: State = State(LoaderState.Idle)
    val lastBestChannel: Coeval[Option[SyncWith]] = lastSeen(bestChannel)

    def loaderBecome(s: LoaderState): Unit = {
      state = state.copy(loaderState = s)
      log.trace(s"changing loader state to $state")
    }

    def blacklistOnTimeout(ch: Channel, reason: String): CancelableFuture[Unit] = Task {
      peerDatabase.blacklistAndClose(ch, reason)
    }.delayExecution(syncTimeOut).runAsync(scheduler)

    def requestNextExt(): Unit = {
      lastBestChannel().get match {
        case None =>
          log.trace("Last bestChannel is None, state is up to date")
          loaderBecome(LoaderState.Idle)
        case Some(best) =>
          val knownSigs = history.lastBlockIds(maxRollback)
          val ch = best.channel
          ch.writeAndFlush(GetSignatures(knownSigs))
          log.debug(s"${id(ch)} Requesting extension sigs, last ${knownSigs.length} are ${formatSignatures(knownSigs)}")
          loaderBecome(LoaderState.ExpectingSignatures(ch, knownSigs, blacklistOnTimeout(ch, s"Timeout loading extension")))
      }
    }

    channelClosed.mapTask(ch => Task {
      state.loaderState match {
        case wp: LoaderState.WithPeer if wp.channel == ch =>
          wp.timeout.cancel()
          requestNextExt()
        case _ =>
      }
    }).logErr.subscribe()

    bestChannel.mapTask(c => Task {
      log.trace(s"New bestChannel: $c, currentState = $state")
      c match {
        case Some(BestChannel(ch, _)) if state.loaderState == LoaderState.Idle => requestNextExt()
        case _ =>
      }
    }).logErr.subscribe()

    signatures.mapTask { case ((ch, sigs)) => Task {
      state.loaderState match {
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
      state.loaderState match {
        case LoaderState.ExpectingBlocks(c, requested, expected, recieved, timeout) if c == ch && expected.contains(block.uniqueId) =>
          timeout.cancel()
          if (expected == Set(block.uniqueId)) {
            log.debug(s"${id(ch)} Extension(blocks=${requested.size}) successfully received")
            val blockById = (recieved + block).map(b => b.uniqueId -> b).toMap
            val ext = requested.map(blockById)
            loaderBecome(LoaderState.Idle)
            val extension = ExtensionBlocks(ext)
            extensionApplier(ch, extension) map {
              case Left(err) =>
                peerDatabase.blacklistAndClose(ch, s"${id(ch)} Error applying $extension: $err")
              case Right(_) =>
                requestNextExt()
            }
          } else Task.now {
            loaderBecome(LoaderState.ExpectingBlocks(c, requested, expected - block.uniqueId, recieved + block, blacklistOnTimeout(ch,
              s"Timeout loading one of requested blocks; prev state=${state.loaderState}")))
          }
        case _ => Task.now {
          simpleBlocks.onNext((ch, block))
        }
      }
    }.flatten
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

  case class State(loaderState: LoaderState)

}