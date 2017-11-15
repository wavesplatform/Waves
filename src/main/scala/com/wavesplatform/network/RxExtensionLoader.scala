package com.wavesplatform.network

import com.wavesplatform.network.RxScoreObserver.SyncWith
import com.wavesplatform.settings.SynchronizationSettings
import io.netty.channel._
import monix.eval.Task
import monix.execution.Scheduler
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
    def c: Channel
  }

  case object Still extends ExtensionLoaderState

  case class ExpectingSignatures(c: Channel, known: Seq[BlockId]) extends WithPeer

  case class ExpectingBlocks(c: Channel, allBlocks: Seq[BlockId],
                             expected: Set[BlockId],
                             recieved: Set[Block]) extends WithPeer


  def apply(ss: SynchronizationSettings,
            history: NgHistory,
            bestChannel: Observable[SyncWith],
            blocks: Observable[(Channel, Block)],
            signatures: Observable[(Channel, Signatures)],
            channelClosed: Observable[Channel]): (Observable[ExtensionBlocks], Observable[Block]) = {

    /* To put it in other words, in Monix it's usually the consumer and
     * not the producer that specifies the scheduler and this operator
     * allows for a different behavior. */
    val scheduler: SchedulerService = Scheduler.singleThread("rx-block-loader")

    val extensionBlocks = PublishSubject[ExtensionBlocks]()
    val simpleBlocks = PublishSubject[Block]()

    var innerState: ExtensionLoaderState = Still

    channelClosed.executeOn(scheduler).mapTask { ch =>
      innerState match {
        case wp: WithPeer if wp.c == ch => Task {
          innerState = Still
        }
        case _ => Task.unit
      }

    }

    bestChannel.executeOn(scheduler).mapTask { case SyncWith(ch, _) => innerState match {
      case Still => Task {
        val knownSigs = history.lastBlockIds(ss.maxRollback)
        ch.writeAndFlush(LoadBlockchainExtension(knownSigs))
        innerState = ExpectingSignatures(ch, knownSigs)
      }
      case _ => Task.unit
    }
    }

    signatures.executeOn(scheduler).mapTask { case ((ch, sigs)) => innerState match {
      case ExpectingSignatures(c, known) if c == ch => Task {
        val (_, unknown) = sigs.signatures.span(id => known.contains(id))
        if (unknown.isEmpty)
          innerState = Still
        else {
          unknown.foreach(s => ch.write(GetBlock(s)))
          innerState = ExpectingBlocks(ch, unknown, unknown.toSet, Set.empty)
        }
      }
      case _ => Task.unit
    }
    }

    blocks.executeOn(scheduler).mapTask { case ((ch, block)) => Task {
      innerState match {
        case ExpectingBlocks(c, requested, expected, recieved) if c == ch && expected.contains(block.uniqueId) => Task {
          if (expected == Set(block.uniqueId)) {
            val blockById = (recieved + block).map(b => b.uniqueId -> b).toMap
            val ext = requested.map(blockById)
            extensionBlocks.onNext(ExtensionBlocks(ext))
          }
        }
        case _ => simpleBlocks.onNext(block)
      }
    }
    }

    (extensionBlocks, simpleBlocks)
  }


}