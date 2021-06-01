package com.wavesplatform.events.repo

import java.nio.{ByteBuffer, ByteOrder}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

import cats.kernel.Monoid
import com.wavesplatform.Shutdownable
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.openDB
import com.wavesplatform.events._
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}
import monix.eval.Task
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

class UpdatesRepoImpl(directory: String, blocks: CommonBlocksApi)(implicit val scheduler: Scheduler)
    extends UpdatesRepo.Read
    with UpdatesRepo.Write
    with UpdatesRepo.Stream
    with ScorexLogging
    with Shutdownable
    with OptimisticLockable {
  import UpdatesRepoImpl._

  private[this] val db = openDB(directory)

  @volatile
  private[this] var liquidState: Option[LiquidState] = None

  log.info(s"BlockchainUpdates extension opened db at $directory")

  override def shutdown(): Unit = db.close()

  @volatile
  private[this] var lastRealTimeUpdates = Seq.empty[BlockchainUpdated]
  private[this] val realTimeUpdates     = ConcurrentSubject.publish[BlockchainUpdated]

  private[this] def sendRealTimeUpdate(upd: BlockchainUpdated): Unit = {
    val currentUpdates = this.lastRealTimeUpdates
    val currentHeight  = height.toOption
    this.lastRealTimeUpdates = currentUpdates.dropWhile(u => currentHeight.exists(_ - 1 > u.height)) :+ upd
    realTimeUpdates.onNext(upd) match {
      case Ack.Continue => // OK
      case Ack.Stop     => throw new IllegalStateException("Real time updates subject is stopped")
    }
  }

  // UpdatesRepo.Read impl
  override def height: Try[Int] = Try {
    liquidState.map(_.keyBlock.height).getOrElse {
      val iter = db.iterator()

      def parseHeight(blockBytes: Array[Byte]) = {
        val lastUpdate = PBBlockchainUpdated.parseFrom(blockBytes).vanilla.get
        lastUpdate.height
      }

      try {
        try iter.seekToLast()
        catch {
          case _: UnsupportedOperationException =>
          // Skip to support test implementation
        }

        iter.asScala
          .map(_.getValue)
          .to(LazyList)
          .lastOption
          .fold(0)(parseHeight)
      } finally iter.close()
    }
  }

  private[this] def readBlock(height: Int) = {
    val (meta, txs) = blocks.blockAtHeight(height).get
    Block(meta.header, meta.signature, txs.map(_._1))
  }

  override def updateForHeight(height: Int): Try[BlockAppended] =
    Try(liquidState match {
      case Some(ls) if ls.keyBlock.height == height =>
        log.debug(s"BlockchainUpdates extension requested liquid block at height $height")
        ls.solidify()
      case Some(ls) if ls.keyBlock.height < height =>
        throw new NoSuchElementException(s"BlockchainUpdates extension requested non-existing block at height $height, current ${ls.keyBlock.height}")
      case _ if height <= 0 =>
        throw new IllegalArgumentException("BlockchainUpdates asked for an update at a non-positive height")
      case _ =>
        val bytes = db.get(key(height))
        if (bytes == null || bytes.isEmpty) {
          throw new IllegalStateException(s"No data for blockchain update in database at height $height")
        } else {
          val pbParseResult = PBBlockchainUpdated.parseFrom(bytes)
          val vanillaUpdate = pbParseResult.vanilla.get.asInstanceOf[BlockAppended]
          vanillaUpdate.copy(block = readBlock(vanillaUpdate.height))
        }
    })

  override def updatesRange(from: Int, to: Int): Observable[BlockAppended] = {
    stream(from)
      .collect { case u: BlockAppended => u }
      .takeWhileInclusive(_.height < to)
  }

  // UpdatesRepo.Write impl
  override def appendBlock(blockAppended: BlockAppended): Try[Unit] = writeLock {
    Try {
      liquidState.foreach { ls =>
        val solidBlock = ls.solidify()
        if (solidBlock.height > 1) require(db.get(key(solidBlock.height - 1)) != null, s"Previous block missing: ${solidBlock.height - 1}")
        db.put(
          key(solidBlock.height),
          solidBlock.protobuf.update(_.append.block.optionalBlock := None).toByteArray
        )
        require(db.get(key(solidBlock.height)) != null, s"Update isn't persisted: ${solidBlock.height}")
      }
      liquidState = Some(LiquidState(blockAppended, Seq.empty))
      sendRealTimeUpdate(blockAppended)
    }
  }

  override def appendMicroBlock(microBlockAppended: MicroBlockAppended): Try[Unit] = Try {
    liquidState match {
      case Some(LiquidState(keyBlock, microBlocks)) =>
        liquidState = Some(LiquidState(keyBlock, microBlocks :+ microBlockAppended))
        sendRealTimeUpdate(microBlockAppended)
      case None => throw new IllegalStateException("BlockchainUpdates attempted to insert a microblock without a keyblock")
    }
  }

  override def rollback(blockchain: Blockchain, toId: ByteStr, toHeight: Int, sendEvent: Boolean): Try[Unit] =
    for {
      h <- this.height
      result <- if (toHeight > h) {
        Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to a height higher than current"))
      } else if (toHeight == h) {
        Success(Monoid.empty[RollbackResult])
      } else if (toHeight == h - 1 && liquidState.isDefined) {
        Success(doFullLiquidRollback())
      } else {
        doStateRollback(toHeight)
      }
      refAssets = StateUpdate.referencedAssets(blockchain, Seq(result.stateUpdate))
      _         = if (sendEvent) sendRealTimeUpdate(RollbackCompleted(toId, toHeight, result, refAssets))
    } yield ()

  private[this] def doFullLiquidRollback(): RollbackResult = {
    val microBlocks = liquidState.toSeq.flatMap(_.microBlocks)
    val removedTxs  = microBlocks.flatMap(_.microBlock.transactionData).map(_.id())
    val stateUpdate = MicroBlockAppended.revertMicroBlocks(microBlocks)
    liquidState = None
    RollbackResult.micro(removedTxs, stateUpdate)
  }

  private[this] def doStateRollback(toHeight: Int): Try[RollbackResult] =
    Try(writeLock {
      log.info(s"Rolling back blockchain updates to $toHeight")

      val iter  = db.iterator()
      val batch = db.createWriteBatch()
      try {
        var changes = Seq.empty[RollbackResult]

        iter.seek(key(toHeight))

        iter.asScala
          .map(e => (e.getKey, protobuf.BlockchainUpdated.parseFrom(e.getValue).vanilla.get.asInstanceOf[BlockAppended]))
          .filter(_._2.height > toHeight)
          .foreach {
            case (key, ba) =>
              val reverted = {
                val block        = Try(readBlock(ba.height)).toOption
                val transactions = block.fold(Seq.empty[ByteStr])(_.transactionData.map(_.id()))
                RollbackResult(block.toSeq, transactions.reverse, ba.reverseStateUpdate)
              }
              log.trace(s"Rolling back block update at ${ba.height}: ${ba.id}")
              changes = reverted +: changes
              batch.delete(key)
          }

        db.write(batch)
        liquidState = None

        require(height.get == toHeight, s"Rollback doesn't succeed: ${height.get} != $toHeight")
        Monoid.combineAll(changes)
      } finally {
        iter.close()
        batch.close()
      }
    })

  override def rollbackMicroBlock(blockchain: Blockchain, toId: ByteStr): Try[Unit] =
    for {
      height <- this.height
      result <- liquidState match {
        case Some(ls) =>
          if (toId == ls.keyBlock.id) {
            liquidState = Some(ls.copy(microBlocks = Seq.empty))
            val removedTxs  = ls.microBlocks.flatMap(_.microBlock.transactionData).map(_.id())
            val stateUpdate = MicroBlockAppended.revertMicroBlocks(ls.microBlocks)
            Success(RollbackResult.micro(removedTxs, stateUpdate))
          } else {
            @tailrec
            def dropUntilId(
                microBlocks: Seq[MicroBlockAppended],
                id: ByteStr,
                dropped: Seq[MicroBlockAppended] = Nil
            ): (Seq[MicroBlockAppended], Seq[MicroBlockAppended]) = microBlocks match {
              case Nil                                   => (Nil, dropped)
              case rest @ (_ :+ block) if block.id == id => (rest, dropped)
              case rest :+ block                         => dropUntilId(rest, id, block +: dropped)
            }
            val (keep, drop) = dropUntilId(ls.microBlocks, toId)
            if (keep.isEmpty) {
              Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback a non-existing microblock"))
            } else {
              liquidState = Some(ls.copy(microBlocks = keep))
              val removedTxs  = drop.flatMap(_.microBlock.transactionData).map(_.id()).reverse
              val stateUpdate = MicroBlockAppended.revertMicroBlocks(drop)
              Success(RollbackResult.micro(removedTxs, stateUpdate))
            }
          }
        case None => Failure(new IllegalStateException("BlockchainUpdates attempted to rollback microblock without liquid state present"))
      }
      refAssets = StateUpdate.referencedAssets(blockchain, Seq(result.stateUpdate))
      _         = sendRealTimeUpdate(MicroBlockRollbackCompleted(toId, height, result, refAssets))
    } yield ()

  // UpdatesRepo.Stream impl
  override def stream(fromHeight: Int): Observable[BlockchainUpdated] = {
    def readBatchTask(from: Int): Task[(Seq[BlockchainUpdated], Option[Int])] = {
      blocks
        .blocksRange(from, from + LevelDBReadBatchSize)
        .map {
          case (meta, txs) =>
            for {
              bytes <- readLockCond(Option(db.get(key(meta.height))))(_.isEmpty)
              update = PBBlockchainUpdated.parseFrom(bytes)
              ba     = update.vanilla.get.asInstanceOf[BlockAppended]
            } yield ba.copy(block = Block(meta.header, meta.signature, txs.map(_._1)))
        }
        .takeWhile(_.isDefined)
        .flatMap(Observable.fromIterable(_))
        .toListL
        .map { data =>
          def isLastBatch(data: Seq[_]): Boolean = data.length < LevelDBReadBatchSize

          if (isLastBatch(data)) (data, None) else {
            val nextTickFrom = data.lastOption.map(_.height + 1)
            (data, nextTickFrom)
          }
        }
    }

    Observable.fromTry(height).flatMap { h =>
      if (h < fromHeight) {
        Observable.raiseError(new IllegalArgumentException("Requested start height exceeds current blockchain height"))
      } else {
        def readBatchStream(from: Int, prevLastPersistent: Option[BlockchainUpdated] = None): Observable[BlockchainUpdated] = Observable.fromTask(readBatchTask(from)).flatMap {
          case (data, next) =>
            Observable.fromIterable(data) ++ (next match {
              case Some(next) =>
                readBatchStream(next, data.lastOption.ensuring(_.nonEmpty))

              case None =>
                val lastPersistentUpdate = data.lastOption.orElse(prevLastPersistent)
                Observable
                  .fromIterable(lastRealTimeUpdates)
                  .++(realTimeUpdates)
                  .dropWhile(u => !lastPersistentUpdate.forall(u.references))
            })
        }
        readBatchStream(fromHeight)
      }
    }
  }
}

object UpdatesRepoImpl {
  private val LevelDBReadBatchSize = 100

  private def key(height: Int): Array[Byte] =
    ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putInt(height).array()
}
