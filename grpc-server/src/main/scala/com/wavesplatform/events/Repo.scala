package com.wavesplatform.events

import cats.syntax.semigroup.*
import com.google.common.primitives.Ints
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.grpc.*
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.DBExt
import com.wavesplatform.events.Repo.keyForHeight
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.events.api.grpc.protobuf.BlockchainUpdatesApiGrpc.BlockchainUpdatesApi
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.events.protobuf.serde.*
import com.wavesplatform.events.repo.LiquidState
import com.wavesplatform.state.{Blockchain, StateSnapshot}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.StreamObserver
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.PublishToOneSubject
import org.rocksdb.RocksDB

import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Future
import scala.util.Using
import scala.util.control.Exception

class Repo(db: RocksDB, blocksApi: CommonBlocksApi)(implicit s: Scheduler)
    extends BlockchainUpdatesApi
    with BlockchainUpdateTriggers
    with ScorexLogging {
  private[this] val monitor     = new Object
  private[this] var liquidState = Option.empty[LiquidState]
  private[this] val handlers    = ConcurrentHashMap.newKeySet[Handler]()

  def newHandler(id: String, maybeLiquidState: Option[LiquidState], subject: PublishToOneSubject[BlockchainUpdated], maxQueueSize: Int): Handler =
    new Handler(id, maybeLiquidState, subject, maxQueueSize)

  def shutdownHandlers(): Unit = monitor.synchronized {
    handlers.forEach(_.shutdown())
  }

  def shutdown(): Unit = {
    shutdownHandlers()
  }

  def height: Int =
    liquidState.fold(db.readOnly { ro =>
      var lastHeight = 0
      Using(ro.newIterator) { iter =>
        Exception.ignoring(classOf[UnsupportedOperationException])(iter.seekToLast())
        while (iter.isValid) {
          lastHeight = Ints.fromByteArray(iter.key())
          iter.next()
        }
      }
      lastHeight
    })(_.keyBlock.height)

  override def onProcessBlock(
      block: Block,
      snapshot: StateSnapshot,
      reward: Option[Long],
      hitSource: ByteStr,
      blockchainBeforeWithReward: Blockchain
  ): Unit = monitor.synchronized {
    require(
      liquidState.forall(_.totalBlockId == block.header.reference),
      s"Block reference ${block.header.reference} does not match last block id ${liquidState.get.totalBlockId}"
    )

    liquidState.foreach(ls =>
      db.put(keyForHeight(ls.keyBlock.height), ls.solidify().protobuf.update(_.append.block.optionalBlock := None).toByteArray)
    )

    val ba = BlockAppended.from(block, snapshot, blockchainBeforeWithReward, reward, hitSource)
    liquidState = Some(LiquidState(ba, Seq.empty))
    handlers.forEach(_.handleUpdate(ba))
  }

  override def onProcessMicroBlock(
      microBlock: MicroBlock,
      snapshot: StateSnapshot,
      blockchainBeforeWithReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): Unit = monitor.synchronized {
    val ls = liquidState.getOrElse(throw new IllegalArgumentException("Can not append microblock to empty liquid block"))
    require(
      ls.totalBlockId == microBlock.reference,
      s"Microblock reference ${microBlock.reference} does not match last block id ${liquidState.get.totalBlockId}"
    )

    val mba = MicroBlockAppended.from(microBlock, snapshot, blockchainBeforeWithReward, totalBlockId, totalTransactionsRoot)
    liquidState = Some(ls.copy(microBlocks = ls.microBlocks :+ mba))

    handlers.forEach(_.handleUpdate(mba))
  }

  def rollbackData(toHeight: Int): Seq[BlockAppended] =
    db.readWrite { rw =>
      log.debug(s"Rolling back to $toHeight")
      var buf: List[BlockAppended] = Nil
      Using(rw.newIterator) { iter =>
        iter.seek(keyForHeight(toHeight + 1))
        while (iter.isValid) {
          val height      = Ints.fromByteArray(iter.key())
          val stateUpdate = Loader.parseUpdate(iter.value(), blocksApi, height).vanillaAppend
          buf = stateUpdate :: buf
          iter.next()
        }
      }

      (1 to buf.size).foreach { offset =>
        val height = toHeight + offset
        log.debug(s"Deleting update at $height")
        rw.delete(keyForHeight(height))
      }

      buf
    }

  private def revertMicroBlock(mba: MicroBlockAppended, blockchainBefore: Blockchain) =
    MicroBlockRollbackCompleted(
      mba.id,
      mba.height,
      RollbackResult.micro(
        {
          log.debug(s"Original order: ${mba.microBlock.transactionData.map(_.id())}")
          mba.microBlock.transactionData.map(_.id()).reverse
        },
        mba.reverseStateUpdate
      ),
      StateUpdate.referencedAssets(blockchainBefore, mba.transactionStateUpdates)
    )

  private def revertBlock(ba: BlockAppended, blockchainBefore: Blockchain) =
    RollbackCompleted(
      ba.block.header.reference,
      ba.height - 1,
      RollbackResult(
        Seq(ba.block),
        ba.block.transactionData.map(_.id()).reverse,
        ba.reverseStateUpdate,
        blockchainBefore.activatedFeatures.collect { case (id, activationHeight) if activationHeight == ba.height => id.toInt }.toSeq
      ),
      StateUpdate.referencedAssets(blockchainBefore, ba.transactionStateUpdates)
    )

  override def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit = monitor.synchronized {
    require(liquidState.forall(_.keyBlock.height > toHeight), s"Cannot rollback to current height $toHeight")
    val (microRollbacks, blockRollbacks) = liquidState match {
      case Some(ls) =>
        require(toHeight < ls.keyBlock.height, s"Cannot roll back to height $toHeight which is not lower than current height ${ls.keyBlock.height}")
        if (toHeight == ls.keyBlock.height - 1) {
          val liquidBlockRef = ls.keyBlock.block.header.reference
          require(liquidBlockRef == toBlockId, s"Liquid block reference $liquidBlockRef at $toHeight does not match rollback target $toBlockId")
          ls.microBlocks.reverse.map(revertMicroBlock(_, blockchainBefore)) -> Seq(revertBlock(ls.keyBlock, blockchainBefore))
        } else {
          ls.microBlocks.reverse.map(revertMicroBlock(_, blockchainBefore)) ->
            (ls.keyBlock +: rollbackData(toHeight)).map(revertBlock(_, blockchainBefore))
        }
      case None => Seq.empty -> rollbackData(toHeight).map(revertBlock(_, blockchainBefore))
    }

    liquidState = None

    handlers.forEach(_.rollbackBlock(microRollbacks, blockRollbacks))
  }

  override def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit = monitor.synchronized {
    log.trace(s"Rolling back liquid microblock to $toBlockId")
    liquidState match {
      case Some(ls) =>
        val discardedMicroBlocks = if (ls.keyBlock.id == toBlockId) {
          val toDiscard = ls.microBlocks
          liquidState = Some(LiquidState(ls.keyBlock, Seq.empty))
          toDiscard
        } else {
          ls.microBlocks.zipWithIndex.reverse
            .collectFirst {
              case (mba, idx) if mba.id == toBlockId => idx
            }
            .fold(throw new IllegalArgumentException(s"Cannot rollback microblock to $toBlockId")) { idx =>
              val (toKeep, toDiscard) = ls.microBlocks.splitAt(idx + 1)
              require(toDiscard.nonEmpty, s"Cannot rollback to $toBlockId which is already the last block")
              log.trace(s"Rolling back to $toBlockId, discarding ${toDiscard.size} microblocks")
              liquidState = Some(LiquidState(ls.keyBlock, toKeep))
              toDiscard
            }
        }

        handlers.forEach(
          _.rollbackMicroBlock(
            MicroBlockRollbackCompleted(
              toBlockId,
              ls.keyBlock.height,
              RollbackResult.micro(
                discardedMicroBlocks.flatMap(_.microBlock.transactionData.map(_.id())).reverse,
                discardedMicroBlocks.reverse.map(_.reverseStateUpdate).reduceLeft(_ |+| _)
              ),
              StateUpdate.referencedAssets(blockchainBefore, discardedMicroBlocks.flatMap(_.transactionStateUpdates))
            )
          )
        )
      case None =>
        throw new IllegalArgumentException(s"Cannot rollback to microblock $toBlockId, liquid state is empty")

    }
  }

  def getBlockUpdate(height: Int): GetBlockUpdateResponse = liquidState match {
    case Some(ls) if ls.keyBlock.height == height => GetBlockUpdateResponse(Some(ls.solidify().protobuf))
    case Some(ls) if ls.keyBlock.height < height  => throw new IllegalArgumentException()
    case _ =>
      db.withResource { res =>
        GetBlockUpdateResponse(Some(Loader.loadUpdate(res, blocksApi, height)))
      }
  }

  override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse] = Future(getBlockUpdate(request.height))

  override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] =
    stream(request.fromHeight, request.toHeight, Integer.toString(request.##, 16)).toListL.runToFuture
      .map(updates => GetBlockUpdatesRangeResponse(updates))

  private def stream(fromHeight: Int, toHeight: Int, streamId: String): Observable[PBBlockchainUpdated] = {
    require(fromHeight <= blocksApi.currentHeight, "Requested start height exceeds current blockchain height")
    require(fromHeight > 0, "fromHeight must be > 0")
    require(toHeight == 0 || toHeight >= fromHeight, "fromHeight must not exceed toHeight")
    monitor.synchronized {
      val subject = PublishToOneSubject[BlockchainUpdated]()
      val handler = newHandler(streamId, liquidState, subject, 250)
      handlers.add(handler)

      val removeHandler = Task {
        log.info(s"[$streamId] Removing handler")
        handlers.remove(handler)
      }.void

      (new Loader(
        db,
        blocksApi,
        liquidState.map(ls => (ls.keyBlock.height - 1) -> ls.keyBlock.block.header.reference),
        streamId
      ).loadUpdates(fromHeight) ++
        subject.map(_.protobuf))
        .takeWhile(u => toHeight == 0 || u.height <= toHeight)
        .doOnComplete(removeHandler)
        .doOnError(t => Task(log.error(s"[$streamId] Subscriber error", t)).flatMap(_ => removeHandler))
        .doOnEarlyStop(removeHandler)
        .doOnSubscriptionCancel(removeHandler)
    }
  }

  override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = {
    responseObserver.interceptErrors(
      responseObserver.completeWith(
        stream(request.fromHeight, request.toHeight, responseObserver.id).map(bu => SubscribeEvent(Some(bu)))
      )
    )
  }
}

object Repo {
  def keyForHeight(height: Int): Array[Byte] = ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putInt(height).array()
}
