package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Transaction
import monix.reactive.Observable

trait CommonBlocksApi {
  def blockDelay(blockId: BlockId, blockNum: Int): Option[Long]

  def currentHeight: Int

  def block(blockId: BlockId): Option[(BlockMeta, Seq[(Transaction, Boolean)])]

  def blockAtHeight(height: Int): Option[(BlockMeta, Seq[(Transaction, Boolean)])]

  def blocksRange(fromHeight: Int, toHeight: Int): Observable[(BlockMeta, Seq[(Transaction, Boolean)])]

  def blocksRange(fromHeight: Int, toHeight: Int, generatorAddress: Address): Observable[(BlockMeta, Seq[(Transaction, Boolean)])]

  def meta(id: ByteStr): Option[BlockMeta]

  def metaAtHeight(height: Int): Option[BlockMeta]

  def metaRange(fromHeight: Int, toHeight: Int): Observable[BlockMeta]
}

object CommonBlocksApi {
  def apply(
      blockchain: Blockchain,
      metaAt: Int => Option[BlockMeta],
      blockInfoAt: Int => Option[(BlockMeta, Seq[(Transaction, Boolean)])]
  ): CommonBlocksApi = new CommonBlocksApi {
    private def fixHeight(h: Int) = if (h <= 0) blockchain.height + h else h

    def blocksRange(fromHeight: Int, toHeight: Int): Observable[(BlockMeta, Seq[(Transaction, Boolean)])] =
      Observable
        .fromIterable(fixHeight(fromHeight) to fixHeight(toHeight))
        .map(blockInfoAt)
        .takeWhile(_.isDefined)
        .flatMap(Observable.fromIterable(_))

    def blocksRange(fromHeight: Int, toHeight: Int, generatorAddress: Address): Observable[(BlockMeta, Seq[(Transaction, Boolean)])] =
      Observable.fromIterable(
        (fixHeight(fromHeight) to fixHeight(toHeight))
          .flatMap(h => metaAt(h))
          .collect { case m if m.header.generator.toAddress == generatorAddress => m.height }
          .flatMap(h => blockInfoAt(h))
      )

    def blockDelay(blockId: BlockId, blockNum: Int): Option[Long] =
      blockchain
        .heightOf(blockId)
        .map { maxHeight =>
          val minHeight  = maxHeight - blockNum.max(1)
          val allHeaders = (minHeight to maxHeight).flatMap(h => metaAt(h))
          val totalPeriod = allHeaders
            .sliding(2)
            .map { pair =>
              pair(1).header.timestamp - pair(0).header.timestamp
            }
            .sum
          totalPeriod / (allHeaders.size - 1).max(1)
        }

    def currentHeight: Int = blockchain.height

    def blockAtHeight(height: Int): Option[(BlockMeta, Seq[(Transaction, Boolean)])] = blockInfoAt(height)

    def metaAtHeight(height: Int): Option[BlockMeta] = metaAt(height)

    def meta(id: ByteStr): Option[BlockMeta] = blockchain.heightOf(id).flatMap(metaAt)

    def metaRange(fromHeight: Int, toHeight: Int): Observable[BlockMeta] =
      Observable.fromIterable((fixHeight(fromHeight) to fixHeight(toHeight)).flatMap(h => metaAt(h)))

    def block(blockId: BlockId): Option[(BlockMeta, Seq[(Transaction, Boolean)])] = blockchain.heightOf(blockId).flatMap(h => blockInfoAt(h))
  }
}
