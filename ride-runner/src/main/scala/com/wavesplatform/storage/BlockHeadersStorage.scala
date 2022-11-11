package com.wavesplatform.storage

import cats.data.NonEmptyList
import cats.syntax.option.*
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.storage.BlockHeadersStorage.BlockInfo
import com.wavesplatform.storage.persistent.BlockPersistentCache
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

class BlockHeadersStorage(blockchainApi: BlockchainGrpcApi, persistentCache: BlockPersistentCache) extends ScorexLogging {
  @volatile private var liquidBlocks: NonEmptyList[BlockInfo] = NonEmptyList.one {
    val height = persistentCache.getLastHeight.getOrElse(blockchainApi.getCurrentBlockchainHeight() - 1)
    val x      = getInternal(height).getOrElse(throw new RuntimeException(s"Can't find a block at $height"))
    BlockInfo(height, x.id(), x)
  }

  def last: BlockInfo = liquidBlocks.last

  def get(height: Int): Option[SignedBlockHeader] = {
    if (liquidBlocks.head.height == height) liquidBlocks.head.header.some
    else getInternal(height)
  }

  private def getInternal(height: Int): Option[SignedBlockHeader] =
    persistentCache.get(height).orElse {
      blockchainApi.getBlockHeader(height).tap {
        case Some(r) => persistentCache.set(height, r)
        case None =>
          log.error(s"Can't find block on height=$height (lastHeight=${persistentCache.getLastHeight}), please contact with developers")
          None
      }
    }

  def update(event: BlockchainUpdated): Unit = {
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
            log.debug(s"Update at ${newFullBlock.height} with ${newFullBlock.id.toString.take(5)})")
            persistentCache.set(newFullBlock.height, newFullBlock.header)
            liquidBlocks = NonEmptyList.one(newFullBlock)

          case Body.MicroBlock(microBlock) =>
            val last = liquidBlocks.head
            // See NgState.forgeBlock and Block.Create
            val newLiquidBlock = BlockInfo(
              event.height,
              microBlock.getMicroBlock.totalBlockId.toByteStr,
              SignedBlockHeader(
                last.header.header.copy(transactionsRoot = microBlock.updatedTransactionsRoot.toByteStr),
                signature = microBlock.getMicroBlock.signature.toByteStr
              )
            )
            persistentCache.set(newLiquidBlock.height, newLiquidBlock.header)
            liquidBlocks = newLiquidBlock :: liquidBlocks
            log.debug(
              s"==> Compare ids: signedBlockHeader.id=${newLiquidBlock.header
                .id()} vs event.id=${event.id.toByteStr} vs microBlock.getMicroBlock.totalBlockId=${microBlock.getMicroBlock.totalBlockId}"
            )
        }

      case _: Update.Rollback =>
        val toBlockId = event.id.toByteStr
        val toHeight  = event.height
        val dropped   = liquidBlocks.toList.dropWhile(x => x.id != toBlockId && x.height >= toHeight)
        dropped match {
          case Nil =>
            log.debug(s"Remove from ${toHeight + 1}")
            persistentCache.remove(toHeight + 1)
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
}

object BlockHeadersStorage {
  case class BlockInfo(height: Int, id: ByteStr, header: SignedBlockHeader)
}
