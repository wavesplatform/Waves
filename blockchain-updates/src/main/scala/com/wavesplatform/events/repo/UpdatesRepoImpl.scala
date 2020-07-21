package com.wavesplatform.events.repo

import java.nio.ByteBuffer

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated, PBEvents}
import com.wavesplatform.events.{BlockAppended, BlockchainUpdated, MicroBlockAppended}
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

class UpdatesRepoImpl(db: DB) extends UpdatesRepo with ScorexLogging {
  import UpdatesRepoImpl.blockKey

  // no need to persist liquid state. Keeping it mutable in a class
  private[this] var liquidState: Option[LiquidState] = None

  override def appendMicroBlock(microBlockAppended: MicroBlockAppended): Unit =
    liquidState match {
      case Some(LiquidState(keyBlock, microBlocks)) =>
        liquidState = Some(LiquidState(keyBlock, microBlocks :+ microBlockAppended))
      case None =>
        throw new IllegalStateException("Attempting to insert a microblock without a keyblock")
    }

  override def getLiquidState(): Option[LiquidState] = liquidState

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

  override def removeAfter(height: Int): Unit = ???

  override def appendBlock(blockAppended: BlockAppended): Unit = {
    liquidState.foreach { ls =>
      val solidBlock = ls.solidify()
      val key        = blockKey(solidBlock.toHeight)
      db.put(key, PBEvents.protobuf(solidBlock).toByteArray)
    }
    liquidState = Some(LiquidState(blockAppended, Seq.empty))
  }

  override def getForHeight(height: Int): Option[BlockAppended] = {
    liquidState match {
      case Some(ls) if ls.keyBlock.toHeight == height =>
        log.debug(s"BlockchainUpdates extension requested liquid block at height $height")
        Some(ls.solidify())
      case Some(ls) if ls.keyBlock.toHeight < height =>
        log.debug(s"BlockchainUpdates extension requested non-existing block at height $height, current ${ls.keyBlock.toHeight}")
        None
      case _ =>
        val bytes = db.get(blockKey(height))
        if (bytes.isEmpty) {
          None
        } else {
          try {
            log.debug(s"BlockchainUpdates extension parsing bytes from leveldb for height $height")
            val pb = PBBlockchainUpdated.parseFrom(bytes)
            PBEvents.vanilla(pb).toOption.collect { case ba: BlockAppended => ba }
          } catch {
            case e: Throwable =>
              log.error("Failed to parse leveldb block update", e)
              None
          }
        }
    }
  }

//  def streamFrom(height: Int): Observable[BlockchainUpdated]
}

object UpdatesRepoImpl {
  private def blockKey(height: Int): Array[Byte] = ByteBuffer.allocate(8).putInt(height).array()
}
