package com.wavesplatform.events.repo

import java.nio.ByteBuffer

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.{BlockAppended, MicroBlockAppended}
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

class UpdatesRepoImpl(db: DB) extends UpdatesRepo with ScorexLogging {
  import UpdatesRepoImpl.blockKey

  // no need to persist liquid state. Keeping it mutable in a class
  private[this] var liquidState: Option[LiquidState] = None

  override def getLiquidState(): Option[LiquidState] = liquidState

  override def appendMicroBlock(microBlockAppended: MicroBlockAppended): Try[Unit] =
    liquidState match {
      case Some(LiquidState(keyBlock, microBlocks)) =>
        liquidState = Some(LiquidState(keyBlock, microBlocks :+ microBlockAppended))
        Success()
      case None =>
        Failure(new IllegalStateException("Attempting to insert a microblock without a keyblock"))
    }

  override def dropLiquidState(afterId: Option[ByteStr]): Unit =
    (afterId, liquidState) match {
      case (Some(id), Some(LiquidState(keyBlock, microBlocks))) =>
        if (keyBlock.toId == id) {
          // rollback to key block
          liquidState = Some(LiquidState(keyBlock, Seq.empty))
        } else {
          // otherwise, rollback to a microblock
          val index = microBlocks.indexWhere(_.toId == id)
          if (index != -1) {
            liquidState = Some(LiquidState(keyBlock, microBlocks.dropRight(microBlocks.length - index - 1)))
          }
        }
      case (None, _) => liquidState = None
      case _         => ()
    }

  override def removeAfter(height: Int): Try[Unit] = ???

  override def appendBlock(blockAppended: BlockAppended): Try[Unit] = Try {
    liquidState.foreach { ls =>
      val solidBlock = ls.solidify()
      val key        = blockKey(solidBlock.toHeight)
      db.put(key, solidBlock.protobuf.toByteArray)
    }
    liquidState = Some(LiquidState(blockAppended, Seq.empty))
  }

  override def getForHeight(height: Int): Try[Option[BlockAppended]] = {
    liquidState match {
      case Some(ls) if ls.keyBlock.toHeight == height =>
        log.debug(s"BlockchainUpdates extension requested liquid block at height $height")
        Success(Some(ls.solidify()))
      case Some(ls) if ls.keyBlock.toHeight < height =>
        log.debug(s"BlockchainUpdates extension requested non-existing block at height $height, current ${ls.keyBlock.toHeight}")
        Success(None)
      case _ =>
        val bytes = db.get(blockKey(height))
        if (bytes.isEmpty) {
          Success(None)
        } else {
          Try {
            log.debug(s"BlockchainUpdates extension parsing bytes from leveldb for height $height")
            PBBlockchainUpdated
              .parseFrom(bytes)
              .vanilla
              .toOption
              .collect { case ba: BlockAppended => ba }
          }
        }
    }
  }

  override def getRange(from: Int, to: Int): Try[Seq[BlockAppended]] = Try {
    val iterator = db.iterator()
    iterator.seek(blockKey(from))

    var results = Seq.newBuilder[BlockAppended]
    results.sizeHint(to - from + 1)

    @tailrec
    def go(remaining: Int): Unit = {
      if (remaining > 0) {
        val blockBytes = iterator.next().getValue
        if (blockBytes.nonEmpty) {
          PBBlockchainUpdated.parseFrom(blockBytes).vanilla.get match {
            case b: BlockAppended => results += b
            case _                => ()
          }
          if (iterator.hasNext) go(remaining - 1)
        }
      }
    }

    go(to - from + 1)

    iterator.close()
    results.result()
  }
}

object UpdatesRepoImpl {
  private def blockKey(height: Int): Array[Byte] = ByteBuffer.allocate(8).putInt(height).array()
}
