package com.wavesplatform.events

import com.google.common.primitives.Ints
import com.wavesplatform.api.grpc._
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, openDB}
import com.wavesplatform.events.api.grpc.protobuf.BlockchainUpdatesApiGrpc.BlockchainUpdatesApi
import com.wavesplatform.events.api.grpc.protobuf._
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.repo.LiquidState
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.StreamObserver
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.subjects.ReplaySubject

import scala.concurrent.Future

class Repo(dbDirectory: String, blockchain: Blockchain)(implicit s: Scheduler)
    extends BlockchainUpdatesApi
    with BlockchainUpdateTriggers
    with ScorexLogging {
  private[this] val monitor     = new Object
  private[this] var liquidState = Option.empty[LiquidState]
  private[this] var handlers    = Set.empty[Handler]
  private[this] val db          = openDB(dbDirectory)

  def shutdown(): Unit = {
    db.close()
  }

  override def onProcessBlock(
      block: Block,
      diff: BlockDiffer.DetailedDiff,
      minerReward: Option[Long],
      blockchainBeforeWithMinerReward: Blockchain
  ): Unit = monitor.synchronized {
    require(
      liquidState.forall(_.totalBlockId == block.header.reference),
      s"Block reference ${block.header.reference} does not match last block id ${liquidState.get.totalBlockId}"
    )

    liquidState.foreach(ls => db.put(Ints.toByteArray(ls.keyBlock.height), ls.solidify().protobuf.toByteArray))

    liquidState = Some(LiquidState(BlockAppended.from(block, diff, blockchainBeforeWithMinerReward), Seq.empty))
    handlers.foreach(_.onProcessBlock(block, diff, minerReward, blockchainBeforeWithMinerReward))
  }

  override def onProcessMicroBlock(
      microBlock: MicroBlock,
      diff: BlockDiffer.DetailedDiff,
      blockchainBeforeWithMinerReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): Unit = monitor.synchronized {
    val ls = liquidState.getOrElse(throw new IllegalArgumentException("Can not append microblock to empty liquid block"))
    require(
      ls.totalBlockId == microBlock.reference,
      s"Microblock reference ${microBlock.reference} does not match last block id ${liquidState.get.totalBlockId}"
    )

    liquidState = Some(
      ls.copy(
        microBlocks = ls.microBlocks :+ MicroBlockAppended
          .from(microBlock, diff, blockchainBeforeWithMinerReward, totalBlockId, totalTransactionsRoot)
      )
    )

    handlers.foreach(_.onProcessMicroBlock(microBlock, diff, blockchainBeforeWithMinerReward, totalBlockId, totalTransactionsRoot))
  }

  private def rollbackData(i: Int): Unit =
    db.readWrite { rw =>
      log.debug(s"Rolling back to $i")
      var maxHeightToRemove = Option.empty[Int]
      val iter              = rw.iterator
      iter.seek(Ints.toByteArray(i))
      while (iter.hasNext) {
        maxHeightToRemove = Some(maxHeightToRemove.getOrElse(i) + 1)
        val e = iter.next()
        log.debug(s"Height: ${Ints.fromByteArray(e.getKey)}")
      }
      iter.close()

      maxHeightToRemove.foreach { mh =>
        ((i + 1) to mh).foreach { h =>
          log.debug(s"Deleting update at height $h")
          rw.delete(Ints.toByteArray(h))
        }
      }
    }

  override def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit = monitor.synchronized {
    require(liquidState.forall(_.keyBlock.height > toHeight), s"Cannot rollback to current height $toHeight")
    rollbackData(toHeight)
    liquidState = None
    handlers.foreach(_.onRollback(blockchainBefore, toBlockId, toHeight))
  }

  override def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit = ???

  override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse] = Future.failed(new NotImplementedError)

  override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] =
    Future.failed(new NotImplementedError)

  override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = monitor.synchronized {
    val subject = ReplaySubject[BlockchainUpdated]()
    val handler = new Handler("", liquidState.fold[Seq[BlockchainUpdated]](Seq.empty)(_.toSeq), subject)
    handlers += handler

    val removeHandler = Task(monitor.synchronized {
      log.info(s"Removing handler $handler")
      handlers -= handler
    })

    responseObserver.completeWith(
      (new Loader(
        db,
        liquidState.fold(blockchain.height)(_.keyBlock.height - 1),
        liquidState.fold(blockchain.lastBlockId.get)(_.keyBlock.block.header.reference)
      ).streamFrom(request.fromHeight) ++
        subject.takeWhile(u => request.toHeight == 0 || u.height <= request.toHeight).map(_.protobuf))
        .map(bu => SubscribeEvent(Some(bu)))
        .doOnComplete(removeHandler)
        .doOnError(t => Task(log.error("Subscriber error", t)).flatMap(_ => removeHandler))
        .doOnEarlyStop(removeHandler)
        .doOnSubscriptionCancel(removeHandler)
    )
  }
}
