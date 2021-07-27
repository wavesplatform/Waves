package com.wavesplatform.events
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.repo.LiquidState
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.utils.ScorexLogging
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

class Handler(id: String, liquidState: Seq[BlockchainUpdated], subject: Observer[BlockchainUpdated])(implicit s: Scheduler)
    extends ScorexLogging
    with BlockchainUpdateTriggers {

  private[this] val queue = ArrayBuffer.from(liquidState)
  @volatile
  private[this] var cancelled = false

  sendUpdate()

  override def onProcessBlock(
      block: Block,
      diff: BlockDiffer.DetailedDiff,
      minerReward: Option[Long],
      blockchainBeforeWithMinerReward: Blockchain
  ): Unit = handleAndSendUpdate(Handler.processBlock(queue, block, diff, blockchainBeforeWithMinerReward, fail))

  override def onProcessMicroBlock(
      microBlock: MicroBlock,
      diff: BlockDiffer.DetailedDiff,
      blockchainBeforeWithMinerReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): Unit = handleAndSendUpdate {
    queue.append(MicroBlockAppended.from(microBlock, diff, blockchainBeforeWithMinerReward, totalBlockId, totalTransactionsRoot))
  }

  override def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit =
    handleAndSendUpdate(Handler.rollback(queue, blockchainBefore, toBlockId, toHeight, fail))

  override def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit =
    handleAndSendUpdate(Handler.rollbackMicroBlock(queue, blockchainBefore, toBlockId, fail))

  def cancel(): Unit = {
    log.trace("Cancelling subscription")
    cancelled = true
  }

  private def handleAndSendUpdate(f: => Unit): Unit = {
    queue.synchronized(f)
    sendUpdate()
  }

  private def fail(message: String): Unit = {
    val ex = new IllegalStateException(message)
    log.error(s"[$id] Error in handler", ex)
    subject.onError(ex)
  }

  private def sendUpdate(): Unit =
    if (queue.nonEmpty && !cancelled)
      s.execute(
        () =>
          queue.synchronized {
            val v = queue.remove(0)
            log.trace(s"[$id] Sending $v to subscriber")
            subject.onNext(v).onComplete {
              case Success(Ack.Continue) =>
                log.trace(s"[$id] Sent $v to subscriber, attempting to send one more")
              case Success(Ack.Stop) =>
                log.debug(s"[$id] Subscriber stopped")
              case Failure(exception) =>
                log.error(s"[$id] Error sending update", exception)
            }
          }
      )
    else log.trace(s"Queue is empty")
}

object Handler {
  def processBlock(
      queue: ArrayBuffer[BlockchainUpdated],
      block: Block,
      diff: BlockDiffer.DetailedDiff,
      blockchainBeforeWithMinerReward: Blockchain,
      fail: String => Unit
  ): Unit = {
    queue.lastOption match {
      case Some(mba: MicroBlockAppended) =>
        if (mba.id != block.header.reference)
          fail(s"Block reference ${block.header.reference} does not match last microblock ${mba.id}")
        else {
          val updatesToFlatten = queue.zipWithIndex.reverseIterator
            .takeWhile {
              case (m: MicroBlockAppended, _) => mba.height == m.height
              case (b: BlockAppended, _)      => mba.height == b.height
              case _                          => false
            }
            .toSeq
            .reverse

          for ((_, idx) <- updatesToFlatten.headOption) queue.remove(idx, updatesToFlatten.size)

          val blocksCollector = Seq.newBuilder[(BlockAppended, Int)]
          val microsCollector = Seq.newBuilder[(MicroBlockAppended, Int)]

          updatesToFlatten.foreach {
            case (b: BlockAppended, i)      => blocksCollector += (b -> i)
            case (m: MicroBlockAppended, i) => microsCollector += (m -> i)
          }

          val block = blocksCollector.result()
          if (block.size != 1) fail(s"Expecting single block, but got ${block.map(_._1.id).mkString("[", ",", "]")}")

          val micros = microsCollector.result()
          if (block.head._2 >= micros.head._2) fail("Block update is not the first one in liquid chain")

          queue.append(LiquidState.solidify(block.head._1, micros.map(_._1)))
        }
      case _ => // previous block had no micro blocks
    }
    queue.append(BlockAppended.from(block, diff, blockchainBeforeWithMinerReward))
  }

  def rollback(queue: ArrayBuffer[BlockchainUpdated], blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int, fail: String => Unit): Unit =
    ???

  def rollbackMicroBlock(queue: ArrayBuffer[BlockchainUpdated], blockchainBefore: Blockchain, toBlockId: ByteStr, fail: String => Unit): Unit =
    queue.lastOption match {
      case None => fail("Could not rollback a microblock from empty liquid state")
      case Some(bu) =>
        queue.zipWithIndex.reverseIterator
          .takeWhile(_._1.height == bu.height)
          .find(_._1.id == toBlockId) match {
          case Some((_, idx)) =>
            val drop        = queue.slice(idx, queue.length).collect { case mb: MicroBlockAppended => mb }.toSeq
            val removedTxs  = drop.flatMap(_.microBlock.transactionData).map(_.id()).reverse
            val stateUpdate = MicroBlockAppended.revertMicroBlocks(drop)
            val result      = RollbackResult.micro(removedTxs, stateUpdate)
            val refAssets   = StateUpdate.referencedAssets(blockchainBefore, Seq(result.stateUpdate))
            queue.takeRightInPlace(idx + 1)
            queue.append(MicroBlockRollbackCompleted(toBlockId, blockchainBefore.height, result, refAssets))
          case None => fail(s"Could not find rollback target $toBlockId in liquid state")
        }
    }
}
