package com.wavesplatform.events.repo

import java.nio.{ByteBuffer, ByteOrder}

import cats.kernel.Monoid
import com.wavesplatform.Shutdownable
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.openDB
import com.wavesplatform.events._
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}
import monix.eval.Task
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

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

  log.info(s"BlockchainUpdates extension opened db at ${directory}")

  override def shutdown(): Unit = db.close()

  private[this] val realTimeUpdates = ConcurrentSubject.replayLimited[BlockchainUpdated](100)

  private[this] def sendRealTimeUpdate(ba: BlockchainUpdated): Try[Unit] = {
    realTimeUpdates.onNext(ba) match {
      case Ack.Continue =>
        Success(())
      case Ack.Stop =>
        Failure(new IllegalStateException("realTimeUpdates stream sent Ack.Stop"))
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

      try Try(iter.seekToLast()).fold(
        _ =>
          iter.asScala
            .foldLeft(Option.empty[Array[Byte]]) { case (_, e) => Some(e.getValue) }
            .fold(0)(parseHeight),
        _ => if (iter.hasNext) parseHeight(iter.next.getValue) else 0
      )
      finally iter.close()
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

  override def updatesRange(from: Int, to: Int): Observable[BlockAppended] = height match {
    case Success(h) =>
      stream(from)
        .collect { case u: BlockAppended => u }
        .takeWhile(_.height <= h)

    case Failure(exception) =>
      Observable.raiseError(exception)
  }

  // UpdatesRepo.Write impl
  override def appendBlock(blockAppended: BlockAppended): Try[Unit] = writeLock {
    Try {
      liquidState.foreach { ls =>
        val solidBlock = ls.solidify()
        log.info(s"BlockchainUpdates persisting: ${solidBlock.height}")
        db.put(
          key(solidBlock.height),
          solidBlock.protobuf.update(_.append.block.optionalBlock := None).toByteArray
        )
      }
      liquidState = Some(LiquidState(blockAppended, Seq.empty))
      sendRealTimeUpdate(blockAppended)
    }
  }

  override def appendMicroBlock(microBlockAppended: MicroBlockAppended): Try[Unit] = {
    liquidState match {
      case Some(LiquidState(keyBlock, microBlocks)) =>
        liquidState = Some(LiquidState(keyBlock, microBlocks :+ microBlockAppended))
        sendRealTimeUpdate(microBlockAppended)
      case None =>
        Failure(new IllegalStateException("BlockchainUpdates attempted to insert a microblock without a keyblock"))
    }
  }

  override def rollback(toId: ByteStr, toHeight: Int): Try[Unit] =
    for {
      h <- this.height
      result <- if (toHeight > h) {
        Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to a height higher than current"))
      } else if (toHeight <= 0) {
        Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to a non-positive height"))
      } else if (toHeight == h) {
        Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to current height"))
      } else if (toHeight == h - 1) {
        Success(doFullLiquidRollback())
      } else {
        doStateRollback(toHeight)
      }
      _ <- sendRealTimeUpdate(RollbackCompleted(toId, toHeight, result))
    } yield ()

  private[this] def doFullLiquidRollback(): RollbackResult = writeLock {
    val microBlocks = liquidState.toSeq.flatMap(_.microBlocks)
    val removedTxs  = microBlocks.flatMap(_.microBlock.transactionData).map(_.id())
    val stateUpdate = MicroBlockAppended.revertMicroBlocks(microBlocks)
    liquidState = None
    RollbackResult.micro(removedTxs, stateUpdate)
  }

  private[this] def doStateRollback(toHeight: Int): Try[RollbackResult] =
    Try(writeLock {
      val iter  = db.iterator()
      val batch = db.createWriteBatch()
      try {
        var changes = Seq.empty[RollbackResult]
        iter.seek(key(toHeight))
        iter.next()
        while (iter.hasNext) {
          val key = iter.next.getKey
          val update = protobuf.BlockchainUpdated.parseFrom(db.get(key)).vanilla.collect {
            case ba: BlockAppended =>
              val block        = readBlock(ba.height)
              val transactions = block.transactionData.map(_.id())
              RollbackResult(Seq(block), transactions.reverse, ba.reverseStateUpdate)
          }
          changes = update.get +: changes
          batch.delete(key)
        }
        db.write(batch)
        Monoid.combineAll(changes)
      } finally {
        iter.close()
        batch.close()
      }
    })

  override def rollbackMicroBlock(toId: ByteStr): Try[Unit] =
    for {
      height <- this.height
      stateUpdate <- writeLock(liquidState match {
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
      })
      _ <- sendRealTimeUpdate(MicroBlockRollbackCompleted(toId, height, stateUpdate))
    } yield ()

  // UpdatesRepo.Stream impl
  override def stream(fromHeight: Int): Observable[BlockchainUpdated] = {
    def readBatchTask(from: Int): Task[(Seq[BlockchainUpdated], Option[Int])] = {
      blocks
        .blocksRange(from, from + LevelDBReadBatchSize)
        .map {
          case (meta, txs) =>
            Try(PBBlockchainUpdated.parseFrom(db.get(key(meta.height)))).flatMap(_.vanilla) match {
              case Success(ba: BlockAppended) => Some(ba.copy(block = Block(meta.header, meta.signature, txs.map(_._1))))
              case _ => None
            }
        }
        .takeWhile(_.isDefined)
        .flatMap(Observable.fromIterable(_))
        .toListL
        .map { data =>
          def isLastBatch(data: Seq[_]): Boolean = data.length < LevelDBReadBatchSize

          if (isLastBatch(data)) {
            val liquidUpdates = liquidState match {
              case None => Seq.empty
              case Some(LiquidState(keyBlock, microBlocks)) =>
                val lastBlock = data.lastOption
                require(lastBlock.forall(keyBlock.references))
                Seq(keyBlock) ++ microBlocks
            }
            (data ++ liquidUpdates, None)
          } else {
            val nextTickFrom = data.lastOption.map(_.height + 1)
            (data, nextTickFrom)
          }
        }
    }

    Observable.fromTry(height).flatMap { h =>
      if (h < fromHeight) {
        Observable.raiseError(new IllegalArgumentException("Requested start height exceeds current blockchain height"))
      } else {
        def readBatchStream(from: Int): Observable[BlockchainUpdated] = Observable.fromTask(readBatchTask(from)).flatMap {
          case (data, next) =>
            Observable.fromIterable(data) ++ (next match {
              case Some(next) =>
                readBatchStream(next)

              case None =>
                val lastPersistentUpdate = data.lastOption
                realTimeUpdates.dropWhile(u => !lastPersistentUpdate.forall(u.references))
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
