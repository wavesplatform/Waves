package com.wavesplatform.ride.runner.storage

import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.ride.runner.db.ReadWrite
import com.wavesplatform.ride.runner.storage.BlockHeaderStorage.BlockInfo
import com.wavesplatform.ride.runner.storage.persistent.BlockPersistentCache
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}

import scala.util.chaining.scalaUtilChainingOps

class BlockHeaderStorage private (
    blockchainApi: BlockchainApi,
    persistentCache: BlockPersistentCache,
    var liquidBlocks: List[BlockInfo]
) extends OptimisticLockable
    with ScorexLogging {

//  def addDependent(atHeight: Int, tag: TagT): Unit = tags.compute(key, (_, origTags) => Option(origTags).getOrElse(Set.empty) + tag)
//
  /** @return
    *   None if there is no stored blocks
    */
  def latestHeight: Option[Int] = readLockCond(liquidBlocks.headOption)(_ => false).map(_.height)

  def getLocal(atHeight: Int)(implicit ctx: ReadWrite): Option[SignedBlockHeaderWithVrf] =
    getFromLiquidBlockOr(atHeight, persistentCache.get(atHeight))

  def get(atHeight: Int)(implicit ctx: ReadWrite): Option[SignedBlockHeaderWithVrf] =
    getFromLiquidBlockOr(atHeight, getInternal(atHeight))

  private def getFromLiquidBlockOr(atHeight: Int, or: => Option[SignedBlockHeaderWithVrf]): Option[SignedBlockHeaderWithVrf] =
    readLockCond(liquidBlocks.headOption.collect { case x if x.height == atHeight => x.header })(_ => false)
      .orElse(or)

  private def getInternal(atHeight: Int)(implicit ctx: ReadWrite): Option[SignedBlockHeaderWithVrf] =
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
              SignedBlockHeaderWithVrf(
                SignedBlockHeader(PBBlocks.vanilla(block.getBlock.getHeader), block.getBlock.signature.toByteStr),
                block.vrf.toByteStr
              )
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
              SignedBlockHeaderWithVrf(
                SignedBlockHeader(
                  last.header.header.header.copy(transactionsRoot = microBlock.updatedTransactionsRoot.toByteStr),
                  signature = microBlock.getMicroBlock.signature.toByteStr
                ),
                last.header.vrf
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
            liquidBlocks = BlockInfo(toHeight, header.header.id(), header) :: Nil

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
    } yield BlockInfo(h, b.header.id(), b)
  )

  case class BlockInfo(height: Int, id: ByteStr, header: SignedBlockHeaderWithVrf)
}
