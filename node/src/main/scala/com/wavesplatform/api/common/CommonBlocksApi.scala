package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import monix.reactive.Observable

trait CommonBlocksApi {
  def childBlock(blockId: BlockId): Option[(Block, Int)]

  def blockDelay(blockId: BlockId, blockNum: Int): Option[Long]

  def currentHeight: Int

  def block(blockId: BlockId): Option[Block]

  def blockAtHeight(height: Int): Option[Block]

  def blocksRange(fromHeight: Int, toHeight: Int): Observable[(Block, Int)]

  def blocksRange(fromHeight: Int, toHeight: Int, generatorAddress: Address): Observable[(Block, Int)]

  def meta(id: ByteStr): Option[BlockMeta]

  def metaAtHeight(height: Int): Option[BlockMeta]

  def metaRange(fromHeight: Int, toHeight: Int): Observable[BlockMeta]

}

object CommonBlocksApi {
  def apply(blockchain: Blockchain, blockAt: Int => Option[Block]): CommonBlocksApi = new CommonBlocksApi {
    private def fixHeight(h: Int)                  = if (h <= 0) blockchain.height + h else h
    private def heightOf(id: ByteStr): Option[Int] = blockchain.heightOf(id)

    private def blockHeaderAt(height: Int): Option[BlockMeta] =
      blockchain.blockHeaderAndSize(height).map {
        case (header, size, txCount, uniqueId) => BlockMeta(header, size, txCount, uniqueId, height)
      }

    def blocksRange(fromHeight: Int, toHeight: Int): Observable[(Block, Int)] =
      metaRange(fromHeight, toHeight).flatMap { m =>
        Observable.fromIterable(blockAt(m.height).map(_ -> m.height))
      }

    def blocksRange(fromHeight: Int, toHeight: Int, generatorAddress: Address): Observable[(Block, Int)] =
      metaRange(fromHeight, toHeight)
        .collect { case m if m.header.generator.toAddress == generatorAddress => m.height }
        .flatMap(height => Observable.fromIterable(blockAt(height).map(_ -> height)))

    def childBlock(blockId: BlockId): Option[(Block, Int)] =
      for {
        height <- heightOf(blockId)
        block  <- blockAt(height + 1)
      } yield (block, height + 1)

    def blockDelay(blockId: BlockId, blockNum: Int): Option[Long] =
      heightOf(blockId)
        .map { maxHeight =>
          val minHeight  = maxHeight - blockNum.max(1)
          val allHeaders = (minHeight to maxHeight).flatMap(blockHeaderAt)
          val totalPeriod = allHeaders
            .sliding(2)
            .map { pair =>
              pair(1).header.timestamp - pair(0).header.timestamp
            }
            .sum
          totalPeriod / allHeaders.size
        }

    def currentHeight: Int = blockchain.height

    def blockAtHeight(height: Int): Option[Block] = blockAt(height)

    def metaAtHeight(height: Int): Option[BlockMeta] = blockHeaderAt(height)

    def meta(id: ByteStr): Option[BlockMeta] = heightOf(id).flatMap(blockHeaderAt)

    def metaRange(fromHeight: Int, toHeight: Int): Observable[BlockMeta] =
      Observable.fromIterable((fixHeight(fromHeight) to fixHeight(toHeight)).flatMap(blockHeaderAt))

    def block(blockId: BlockId): Option[Block] = heightOf(blockId).flatMap(blockAt)
  }
}
