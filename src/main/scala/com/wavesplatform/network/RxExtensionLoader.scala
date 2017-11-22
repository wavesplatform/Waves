package com.wavesplatform.network

import com.wavesplatform.network.RxScoreObserver.{BestChannel, SyncWith}
import com.wavesplatform.settings.SynchronizationSettings
import io.netty.channel._
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

object RxExtensionLoader extends ScorexLogging {

  sealed trait ExtensionLoaderState

  sealed trait WithPeer extends ExtensionLoaderState {
    def channel: Channel

    def timeout: CancelableFuture[Unit]
  }

  case object Idle extends ExtensionLoaderState

  case class ExpectingSignatures(channel: Channel, known: Seq[BlockId], timeout: CancelableFuture[Unit]) extends WithPeer

  case class ExpectingBlocks(channel: Channel, allBlocks: Seq[BlockId],
                             expected: Set[BlockId],
                             recieved: Set[Block],
                             timeout: CancelableFuture[Unit]) extends WithPeer


  def apply(ss: SynchronizationSettings,
            history: NgHistory,
            peerDatabase: PeerDatabase,
            bestChannel: Observable[SyncWith],
            blocks: Observable[(Channel, Block)],
            signatures: Observable[(Channel, Signatures)],
            channelClosed: Observable[Channel]): (Observable[(Channel, ExtensionBlocks)], Observable[(Channel, Block)]) = {
    val scheduler: SchedulerService = Scheduler.singleThread("rx-block-loader")

    val extensionBlocks = PublishSubject[(Channel, ExtensionBlocks)]()
    val simpleBlocks = PublishSubject[(Channel, Block)]()
    var innerState: ExtensionLoaderState = Idle


    def blacklistOnTimeout(ch: Channel, reason: String): CancelableFuture[Unit] = Task {
      peerDatabase.blacklistAndClose(ch, reason)
    }.delayExecution(ss.synchronizationTimeout).runAsync(scheduler)


    def requestExtension(ch: Channel, knownSigs: Seq[BlockId]): Unit = {
      ch.writeAndFlush(Signatures(knownSigs))
      innerState = ExpectingSignatures(ch, knownSigs, blacklistOnTimeout(ch, "Timeout expired while loading extension"))
    }

    channelClosed.executeOn(scheduler).mapTask { ch =>
      innerState match {
        case wp: WithPeer if wp.channel == ch => Task {
          wp.timeout.cancel()
          bestChannel.lastOptionL.map {
            _.flatten match {
              case None => innerState = Idle
              case Some(bestChannel: BestChannel) => requestExtension(bestChannel.channel, history.lastBlockIds(ss.maxRollback))
            }
          }
        }.flatten
        case _ => Task.unit
      }
    }


    bestChannel.executeOn(scheduler).mapTask {
      case Some(BestChannel(ch, _)) => innerState match {
        case Idle => Task(requestExtension(ch, history.lastBlockIds(ss.maxRollback)))
        case _ => Task.unit
      }
      case None => Task.unit
    }.subscribe()(scheduler)

    signatures.executeOn(scheduler).mapTask { case ((ch, sigs)) => innerState match {
      case ExpectingSignatures(c, known, timeout) if c == ch => Task {
        timeout.cancel()
        val (_, unknown) = sigs.signatures.span(id => known.contains(id))
        if (unknown.isEmpty)
          innerState = Idle
        else {
          unknown.foreach(s => ch.write(GetBlock(s)))
          innerState = ExpectingBlocks(ch, unknown, unknown.toSet, Set.empty, blacklistOnTimeout(ch, "Timeout loading blocks"))
        }
      }
      case _ => Task.unit
    }
    }.subscribe()(scheduler)

    blocks.executeOn(scheduler).mapTask { case ((ch, block)) => Task {
      innerState match {
        case ExpectingBlocks(c, requested, expected, recieved, timeout) if c == ch && expected.contains(block.uniqueId) =>
          timeout.cancel()
          if (expected == Set(block.uniqueId)) {
            val blockById = (recieved + block).map(b => b.uniqueId -> b).toMap
            val ext = requested.map(blockById)
            extensionBlocks.onNext((ch, ExtensionBlocks(ext)))
            bestChannel.lastOptionL map { // optimistic loader
              case None => innerState = Idle
              case Some(None) => requestExtension(ch, history.lastBlockIds(ss.maxRollback))
              case Some(Some(bestChannel: BestChannel)) => requestExtension(bestChannel.channel, history.lastBlockIds(ss.maxRollback))
            }
          } else {
            innerState = ExpectingBlocks(c, requested, expected - block.uniqueId, recieved + block, blacklistOnTimeout(ch, "Timeout loading blocks"))
          }
        case _ => simpleBlocks.onNext((ch, block))
      }
    }
    }.subscribe()(scheduler)
    (extensionBlocks, simpleBlocks)
  }
}