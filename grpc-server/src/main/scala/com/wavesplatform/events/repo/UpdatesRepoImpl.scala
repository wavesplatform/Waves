package com.wavesplatform.events.repo

import java.nio.{ByteBuffer, ByteOrder}

import com.wavesplatform.Shutdownable
import com.wavesplatform.database.openDB
import com.wavesplatform.events._
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

class UpdatesRepoImpl(directory: String)(implicit val scheduler: Scheduler)
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

  @volatile
  private[this] var lastRealTimeUpdates = Seq.empty[BlockchainUpdated]
  private[this] val realTimeUpdates     = ConcurrentSubject.publish[BlockchainUpdated]

  private[this] def sendRealTimeUpdate(upd: BlockchainUpdated): Unit = {
    val currentUpdates = this.lastRealTimeUpdates
    val currentHeight  = height.toOption
    this.lastRealTimeUpdates = currentUpdates.dropWhile(u => currentHeight.exists(_ - 5 > u.toHeight)) :+ upd
    realTimeUpdates.onNext(upd) match {
      case Ack.Continue => // OKs
      case Ack.Stop     => throw new IllegalStateException("Real time updates subject is stopped")
    }
  }

  // UpdatesRepo.Read impl
  override def height: Try[Int] = Try {
    liquidState.map(_.keyBlock.toHeight).getOrElse {
      val iter = db.iterator()

      def parseHeight(blockBytes: Array[Byte]) = {
        val lastUpdate = PBBlockchainUpdated.parseFrom(blockBytes).vanilla.get
        lastUpdate.toHeight
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

  override def updateForHeight(height: Int): Try[Option[BlockAppended]] =
    liquidState match {
      case Some(ls) if ls.keyBlock.toHeight == height =>
        log.debug(s"BlockchainUpdates extension requested liquid block at height $height")
        Success(Some(ls.solidify()))
      case Some(ls) if ls.keyBlock.toHeight < height =>
        log.warn(s"BlockchainUpdates extension requested non-existing block at height $height, current ${ls.keyBlock.toHeight}")
        Success(None)
      case _ if height <= 0 =>
        Failure(new IllegalArgumentException("BlockchainUpdates asked for an update at a non-positive height"))
      case _ =>
        val bytes = db.get(key(height))
        if (bytes == null || bytes.isEmpty) {
          Success(None)
        } else {
          for {
            pbParseResult <- Try(PBBlockchainUpdated.parseFrom(bytes))
            vanillaUpdate <- pbParseResult.vanilla
            blockAppended <- Try(vanillaUpdate.asInstanceOf[BlockAppended])
          } yield Some(blockAppended)
        }
    }

  override def updatesRange(from: Int, to: Int): Observable[BlockAppended] = height match {
    case Success(h) =>
      stream(from)
        .collect { case u: BlockAppended => u }
        .takeWhile(_.toHeight <= h)

    case Failure(exception) =>
      Observable.raiseError(exception)
  }

  // UpdatesRepo.Write impl
  override def appendBlock(blockAppended: BlockAppended): Try[Unit] = writeLock {
    Try {
      liquidState.foreach { ls =>
        val solidBlock = ls.solidify()
        db.put(
          key(solidBlock.toHeight),
          solidBlock.protobuf.toByteArray
        )
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

  override def rollback(rollback: RollbackCompleted): Try[Unit] =
    height
      .flatMap { h =>
        if (rollback.toHeight > h) {
          Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to a height higher than current"))
        } else if (rollback.toHeight <= 0) {
          Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to a non-positive height"))
        } else if (rollback.toHeight == h) {
          Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback to current height"))
        } else if (rollback.toHeight == h - 1) {
          liquidState = None
          Success(())
        } else
          writeLock {
            val iter  = db.iterator()
            val batch = db.createWriteBatch()
            try {
              iter.seek(key(rollback.toHeight))
              iter.next
              while (iter.hasNext) {
                batch.delete(iter.next.getKey)
              }
              db.write(batch)
              liquidState = None
              Success(())
            } catch {
              case t: Throwable => Failure(t)
            } finally {
              iter.close()
              batch.close()
            }
          }
      }
      .map(_ => sendRealTimeUpdate(rollback))

  override def rollbackMicroBlock(microBlockRollback: MicroBlockRollbackCompleted): Try[Unit] =
    height
      .flatMap { h =>
        if (microBlockRollback.toHeight != h) {
          Failure(new IllegalArgumentException("BlockchainUpdates microblock rollback height was not equal to current height"))
        } else {
          liquidState match {
            case Some(ls) =>
              if (microBlockRollback.toId == ls.keyBlock.toId) {
                liquidState = Some(ls.copy(microBlocks = Seq.empty))
                Success(())
              } else {
                val remainingMicroBlocks = ls.microBlocks.reverse.dropWhile(_.toId != microBlockRollback.toId).reverse
                if (remainingMicroBlocks.isEmpty) {
                  Failure(new IllegalArgumentException("BlockchainUpdates attempted to rollback a non-existing microblock"))
                } else {
                  liquidState = Some(ls.copy(microBlocks = remainingMicroBlocks))
                  Success(())
                }
              }
            case None => Failure(new IllegalStateException("BlockchainUpdates attempted to rollback microblock without liquid state present"))
          }
        }
      }
      .map(_ => sendRealTimeUpdate(microBlockRollback))

  // UpdatesRepo.Stream impl
  override def stream(fromHeight: Int): Observable[BlockchainUpdated] = {

    /**
      * reads from level db by synchronous batches each using one iterator
      * each batch gets a read lock
      * @param from batch start height
      * @return Task to be consumed by Observable.unfoldEval
      */
    def readBatch(from: Int): (Seq[BlockchainUpdated], Option[Int]) =
      readLockCond {
        def isLastBatch(data: Seq[_]): Boolean = data.length < LevelDBReadBatchSize

        val data = {
          val iterator = db.iterator()
          try {
            iterator.seek(key(from))
            iterator.asScala
              .take(LevelDBReadBatchSize)
              .map(e => PBBlockchainUpdated.parseFrom(e.getValue).vanilla.get)
              .toVector
          } finally iterator.close()
        }

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
          val nextTickFrom = data.lastOption.map(_.toHeight + 1)
          (data, nextTickFrom)
        }
      }(_._2.isEmpty)

    Observable.fromTry(height).flatMap { h =>
      if (h < fromHeight) {
        Observable.raiseError(new IllegalArgumentException("Requested start height exceeds current blockchain height"))
      } else {
        def readBatchStream(from: Int): Observable[BlockchainUpdated] = Observable.defer {
          val (data, next) = concurrent.blocking(readBatch(from))
          Observable.fromIterable(data) ++ (next match {
            case Some(next) =>
              readBatchStream(next)

            case None =>
              val lastPersistentUpdate = data.lastOption
              log.info(s"Last persistent: $lastPersistentUpdate")
              Observable
                .fromIterable(lastRealTimeUpdates)
                .++(realTimeUpdates)
                .dropWhile { u =>
                  val result = !lastPersistentUpdate.forall(u.references)
                  if (result) log.info(s"Dropping: $u")
                  result
                }
                .map { u =>
                  log.info(s"Sending real time: $u")
                  u
                }
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
