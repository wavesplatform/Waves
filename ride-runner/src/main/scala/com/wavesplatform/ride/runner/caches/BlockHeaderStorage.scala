package com.wavesplatform.ride.runner.caches

import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.ride.runner.caches.BlockHeaderStorage.BlockInfo
import com.wavesplatform.ride.runner.caches.disk.BlockPersistentCache
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}

import scala.util.chaining.scalaUtilChainingOps

/** @param liquidBlocks
  *   head contains the latest
  */
class BlockHeaderStorage(
    blockchainApi: BlockchainApi,
    persistentCache: BlockPersistentCache
) extends OptimisticLockable
    with ScorexLogging {
  private var liquidBlocks: List[BlockInfo] = Nil

  def load()(implicit ctx: ReadOnly): Unit = writeLock {
    liquidBlocks = for {
      h <- persistentCache.getLastHeight.toList
      b <- persistentCache.get(h)
    } yield BlockInfo(h, b.header.id(), b)
  }

//  def addDependent(atHeight: Int, tag: TagT): Unit = tags.compute(key, (_, origTags) => Option(origTags).getOrElse(Set.empty) + tag)
//
  /** @return
    *   None if there is no stored blocks
    */
  def latestHeight: Option[Height] = readLockCond(liquidBlocks.headOption)(_ => false).map(_.height)

  def getLocal(atHeight: Height)(implicit ctx: ReadWrite): Option[SignedBlockHeaderWithVrf] =
    getFromLiquidBlockOr(atHeight, persistentCache.get(atHeight))

  def getOrFetch(atHeight: Height)(implicit ctx: ReadWrite): Option[SignedBlockHeaderWithVrf] =
    getFromLiquidBlockOr(atHeight, getFromPersistentOrFetch(atHeight))

  private def getFromLiquidBlockOr(atHeight: Height, or: => Option[SignedBlockHeaderWithVrf]): Option[SignedBlockHeaderWithVrf] =
    readLockCond(liquidBlocks.headOption.collect { case x if x.height == atHeight => x.header })(_ => false).orElse(or)

  private def getFromPersistentOrFetch(atHeight: Height)(implicit ctx: ReadWrite): Option[SignedBlockHeaderWithVrf] =
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
            val stateUpdate = append.getStateUpdate
            val newFullBlock = BlockInfo(
              Height(event.height),
              event.id.toByteStr,
              SignedBlockHeaderWithVrf(
                SignedBlockHeader(PBBlocks.vanilla(block.getBlock.getHeader), block.getBlock.signature.toByteStr),
                block.vrf.toByteStr,
                blockReward = stateUpdate.balances.map(x => x.getAmountAfter.amount - x.amountBefore).sum
              )
            )
            log.debug(s"Update at ${newFullBlock.height} with ${newFullBlock.id.toString.take(5)}")
            persistentCache.set(newFullBlock.height, newFullBlock.header)
            persistentCache.setLastHeight(newFullBlock.height)
            liquidBlocks = newFullBlock :: Nil

          case Body.MicroBlock(microBlock) =>
            val last = liquidBlocks.headOption.getOrElse(throw new IllegalStateException("I haven't gotten a key block, but received a micro block"))
            // See NgState.forgeBlock and Block.Create
            val newLiquidBlock = BlockInfo(
              Height(event.height),
              // Same as newLiquidBlock.header.id() and event.id.toByteStr
              microBlock.getMicroBlock.totalBlockId.toByteStr,
              SignedBlockHeaderWithVrf(
                SignedBlockHeader(
                  last.header.header.header.copy(transactionsRoot = microBlock.updatedTransactionsRoot.toByteStr),
                  signature = microBlock.getMicroBlock.signature.toByteStr
                ),
                last.header.vrf,
                last.header.blockReward
              )
            )
            persistentCache.set(newLiquidBlock.height, newLiquidBlock.header)
            liquidBlocks = newLiquidBlock :: liquidBlocks
        }

      case _: Update.Rollback =>
        val toBlockId = event.id.toByteStr
        val toHeight  = Height(event.height)
        val preserved = liquidBlocks.dropWhile(x => x.height >= toHeight && x.id != toBlockId)
        liquidBlocks = preserved match {
          case Nil =>
            log.debug(s"Remove from ${toHeight + 1}")
            persistentCache.removeFrom(Height(toHeight + 1))
            persistentCache.setLastHeight(toHeight)
            val header = getFromPersistentOrFetch(toHeight).getOrElse(throw new RuntimeException(s"Can't get block at $toHeight"))
            BlockInfo(toHeight, header.header.id(), header) :: Nil

          case last :: _ =>
            log.debug(s"Replace at ${last.height} with ${last.id}")
            persistentCache.set(last.height, last.header)
            persistentCache.setLastHeight(last.height)
            preserved
        }
    }
  }

  def removeFrom(height: Height)(implicit ctx: ReadWrite): Unit = writeLock {
    persistentCache.removeFrom(height)
    persistentCache.setLastHeight(Height(height - 1))
    liquidBlocks = liquidBlocks.dropWhile(_.height >= height)
  }
}

object BlockHeaderStorage {
  case class BlockInfo(height: Height, id: ByteStr, header: SignedBlockHeaderWithVrf)
}
