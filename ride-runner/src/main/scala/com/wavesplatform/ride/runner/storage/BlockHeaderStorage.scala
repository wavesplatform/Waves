package com.wavesplatform.ride.runner.storage

import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.ride.runner.storage.BlockHeaderStorage.BlockInfo
import com.wavesplatform.ride.runner.storage.persistent.BlockPersistentCache
import com.wavesplatform.ride.runner.storage.persistent.PersistentStorageContext.ReadWrite
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}

import scala.util.chaining.scalaUtilChainingOps

class BlockHeaderStorage private (
    blockchainApi: BlockchainApi,
    persistentCache: BlockPersistentCache,
    var liquidBlocks: List[BlockInfo]
) extends OptimisticLockable
    with ScorexLogging {

  /** @return
    *   None if there is no stored blocks
    */
  def latestHeight: Option[Int] = readLockCond(liquidBlocks.headOption)(_ => false).map(_.height)

  def getLocal(atHeight: Int)(implicit ctx: ReadWrite): Option[SignedBlockHeader] =
    getFromLiquidBlockOr(atHeight, persistentCache.get(atHeight))

  def get(atHeight: Int)(implicit ctx: ReadWrite): Option[SignedBlockHeader] =
    getFromLiquidBlockOr(atHeight, getInternal(atHeight))

  private def getFromLiquidBlockOr(atHeight: Int, or: => Option[SignedBlockHeader]): Option[SignedBlockHeader] =
    readLockCond(liquidBlocks.headOption.collect { case x if x.height == atHeight => x.header })(_ => false)
      .orElse(or)

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
            liquidBlocks = newFullBlock :: Nil

          case Body.MicroBlock(microBlock) =>
            val last = liquidBlocks.headOption.getOrElse(throw new IllegalStateException("I haven't gotten a key block, but received a micro block"))
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
        val dropped   = liquidBlocks.dropWhile(x => x.id != toBlockId && x.height >= toHeight)
        dropped match {
          case Nil =>
            log.debug(s"Remove from ${toHeight + 1}")
            persistentCache.removeFrom(toHeight + 1)
            val header = getInternal(toHeight).getOrElse(throw new RuntimeException(s"Can't get block at $toHeight"))
            liquidBlocks = BlockInfo(toHeight, header.id(), header) :: Nil

          case last :: rest =>
            log.debug(s"Replace at ${last.height} with ${last.id.toString.take(5)}")
            persistentCache.set(last.height, last.header)
            liquidBlocks = last :: rest
        }
    }
  }

  def removeFrom(height: Int)(implicit ctx: ReadWrite): Unit = persistentCache.removeFrom(height)
}

object BlockHeaderStorage {
  def apply(
      blockchainApi: BlockchainApi,
      persistentCache: BlockPersistentCache
  )(implicit ctx: ReadWrite): BlockHeaderStorage = new BlockHeaderStorage(
    blockchainApi = blockchainApi,
    persistentCache = persistentCache,
    liquidBlocks = for {
      h <- persistentCache.getLastHeight.toList
      b <- persistentCache.get(h)
    } yield BlockInfo(h, b.id(), b)
  )

  case class BlockInfo(height: Int, id: ByteStr, header: SignedBlockHeader)
}
