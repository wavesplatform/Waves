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

  case class ExpectingSignatures(channel: Channel, known: Seq[BlockId], timeout: CancelableFuture[Unit]) extends WithPeer {
    override def toString: String = s"ExpectingSignatures(channel=$channel)"
  }

  case class ExpectingBlocks(channel: Channel, allBlocks: Seq[BlockId],
                             expected: Set[BlockId],
                             received: Set[Block],
                             timeout: CancelableFuture[Unit]) extends WithPeer {
    override def toString: String = s"ExpectingBlocks(channel=$channel, totalBlocks=${allBlocks.size}, received=${received.size}, expected=${expected.size}"
  }

  case class ExtensionBlocks(extension: Seq[Block])

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


    def requestExtension(ch: Channel, knownSigs: Seq[BlockId], reason: String): Unit = {
      ch.writeAndFlush(GetSignatures(knownSigs))
      log.debug(s"${id(ch)} Requesting extension sigs because $reason, last ${knownSigs.length} are ${formatSignatures(knownSigs)}")
      innerState = ExpectingSignatures(ch, knownSigs, blacklistOnTimeout(ch, s"Timeout loading extension(request reason = '$reason'"))
    }

    channelClosed.executeOn(scheduler).mapTask { ch =>
      innerState match {
        case wp: WithPeer if wp.channel == ch => Task {
          wp.timeout.cancel()
          bestChannel.lastOptionL.map {
            _.flatten match {
              case None => innerState = Idle
              case Some(bestChannel: BestChannel) => requestExtension(bestChannel.channel, history.lastBlockIds(ss.maxRollback), s"current channel has been closed while state=$wp")
            }
          }
        }.flatten
        case _ => Task.unit
      }
    }


    bestChannel.executeOn(scheduler).mapTask {
      case Some(BestChannel(ch, _)) => innerState match {
        case Idle => Task(requestExtension(ch, history.lastBlockIds(ss.maxRollback), "Idle and channel with better score detected"))
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
          unknown.foreach(s => ch.writeAndFlush(GetBlock(s)))
          innerState = ExpectingBlocks(ch, unknown, unknown.toSet, Set.empty, blacklistOnTimeout(ch, "Timeout loading first requested block"))
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
            log.debug(s"${id(ch)} Extension successfully received, blocks=${ext.size}")
            val newStateTask = bestChannel.lastOptionL map {
              case None => innerState = Idle
              case Some(maybeBestChannel) =>
                val optimisticLastBlocks = history.lastBlockIds(ss.maxRollback - ext.size) ++ ext.map(_.uniqueId)
                maybeBestChannel match {
                  case None => requestExtension(ch, optimisticLastBlocks, "Optimistic loader, same channel")
                  case Some(bestChannel: BestChannel) => requestExtension(bestChannel.channel, optimisticLastBlocks, "Optimistic loader, better channel")
                }
            }
            extensionBlocks.onNext((ch, ExtensionBlocks(ext)))
            newStateTask
          } else Task {
            innerState = ExpectingBlocks(c, requested, expected - block.uniqueId, recieved + block, blacklistOnTimeout(ch, "Timeout loading one of reqested blocks"))
          }
        case _ => Task {
          simpleBlocks.onNext((ch, block))
        }
      }
    }.flatten
    }.subscribe()(scheduler)
    (extensionBlocks, simpleBlocks)
  }
}