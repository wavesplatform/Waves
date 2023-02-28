package com.wavesplatform.riderunner.storage

import cats.data.NonEmptyList
import cats.syntax.option.*
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.riderunner.storage.BlockHeadersStorage.BlockInfo
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite
import com.wavesplatform.riderunner.storage.persistent.BlockPersistentCache
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}

import scala.util.chaining.scalaUtilChainingOps

class BlockHeadersStorage private (
    blockchainApi: BlockchainApi,
    persistentCache: BlockPersistentCache
) extends OptimisticLockable
    with ScorexLogging {
  private var liquidBlocks: NonEmptyList[BlockInfo] = _

  private def init()(implicit ctx: ReadWrite): Unit = if (liquidBlocks == null) {
    liquidBlocks = NonEmptyList.one {
      val height = persistentCache.getLastHeight.getOrElse(blockchainApi.getCurrentBlockchainHeight() - 1)
      val x      = getInternal(height).getOrElse(throw new RuntimeException(s"Can't find a block at $height"))
      BlockInfo(height, x.id(), x)
    }
  }

  def latest: BlockInfo = readLockCond(liquidBlocks.head)(_ => false)

  def getLocal(atHeight: Int)(implicit ctx: ReadWrite): Option[SignedBlockHeader] = persistentCache.get(atHeight)

  def get(atHeight: Int)(implicit ctx: ReadWrite): Option[SignedBlockHeader] =
    readLockCond(if (liquidBlocks.head.height == atHeight) liquidBlocks.head.header.some else none)(_ => false)
      .orElse(getInternal(atHeight))

  private def getInternal(atHeight: Int)(implicit ctx: ReadWrite): Option[SignedBlockHeader] =
    persistentCache.get(atHeight).orElse {
      blockchainApi.getBlockHeader(atHeight).tap {
        case Some(r) => persistentCache.set(atHeight, r)
        case None =>
          log.error(s"Can't find block on height=$atHeight (lastHeight=${persistentCache.getLastHeight}), please contact with developers")
          None
      }
    }

  def update(event: BlockchainUpdated)(implicit ctx: ReadWrite): Unit = writeLock {
    event.update match {
      case Update.Empty => // Ignore
      case Update.Append(append) =>
        append.body match {
          case Body.Empty => // Ignore
          case Body.Block(block) =>
            val newFullBlock = BlockInfo(
              event.height,
              event.id.toByteStr,
              SignedBlockHeader(PBBlocks.vanilla(block.getBlock.getHeader), block.getBlock.signature.toByteStr)
            )
            log.debug(s"Update at ${newFullBlock.height} with ${newFullBlock.id.toString.take(5)}")
            persistentCache.set(newFullBlock.height, newFullBlock.header)
            liquidBlocks = NonEmptyList.one(newFullBlock)

          case Body.MicroBlock(microBlock) =>
            val last = liquidBlocks.head
            // See NgState.forgeBlock and Block.Create
            val newLiquidBlock = BlockInfo(
              event.height,
              // Same as newLiquidBlock.header.id() and event.id.toByteStr
              microBlock.getMicroBlock.totalBlockId.toByteStr,
              SignedBlockHeader(
                last.header.header.copy(transactionsRoot = microBlock.updatedTransactionsRoot.toByteStr),
                signature = microBlock.getMicroBlock.signature.toByteStr
              )
            )
            persistentCache.set(newLiquidBlock.height, newLiquidBlock.header)
            liquidBlocks = newLiquidBlock :: liquidBlocks
        }

      case _: Update.Rollback =>
        val toBlockId = event.id.toByteStr
        val toHeight  = event.height
        val dropped   = liquidBlocks.toList.dropWhile(x => x.id != toBlockId && x.height >= toHeight)
        dropped match {
          case Nil =>
            log.debug(s"Remove from ${toHeight + 1}")
            persistentCache.removeFrom(toHeight + 1)
            liquidBlocks = NonEmptyList.one {
              val header = getInternal(toHeight).getOrElse(throw new RuntimeException(s"Can't get block at $toHeight"))
              BlockInfo(toHeight, header.id(), header)
            }

          case last :: rest =>
            log.debug(s"Replace at ${last.height} with ${last.id.toString.take(5)}")
            persistentCache.set(last.height, last.header)
            liquidBlocks = NonEmptyList(last, rest)
        }
    }
  }

  def removeFrom(height: Int)(implicit ctx: ReadWrite): Unit = persistentCache.removeFrom(height)
}

object BlockHeadersStorage {
  def apply(
      blockchainApi: BlockchainApi,
      persistentCache: BlockPersistentCache
  )(implicit ctx: ReadWrite): BlockHeadersStorage = {
    new BlockHeadersStorage(
      blockchainApi = blockchainApi,
      persistentCache = persistentCache
    ).tap(_.init())
  }

  case class BlockInfo(height: Int, id: ByteStr, header: SignedBlockHeader)
}
