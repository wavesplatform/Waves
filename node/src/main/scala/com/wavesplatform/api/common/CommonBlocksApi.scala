package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Blockchain, Height, TxMeta}
import com.wavesplatform.transaction.Transaction
import monix.reactive.Observable

trait CommonBlocksApi {
  def blockDelay(blockId: BlockId, blockNum: Int): Option[Long]

  def currentHeight: Height

  def currentFinalizedHeight: Height

  def finalizedHeightAt(at: Height): Option[Height]

  def block(blockId: BlockId): Option[(BlockMeta, Seq[(TxMeta, Transaction)])]

  def blockAtHeight(height: Height): Option[(BlockMeta, Seq[(TxMeta, Transaction)])]

  def blocksRange(fromHeight: Height, toHeight: Height): Observable[(BlockMeta, Seq[(TxMeta, Transaction)])]

  def blocksRange(fromHeight: Height, toHeight: Height, generatorAddress: Address): Observable[(BlockMeta, Seq[(TxMeta, Transaction)])]

  def meta(id: ByteStr): Option[BlockMeta]

  def metaAtHeight(height: Height): Option[BlockMeta]

  def metaRange(fromHeight: Height, toHeight: Height): Observable[BlockMeta]
}

object CommonBlocksApi {
  def apply(
      maxSyncRollbackLength: Int,
      blockchain: Blockchain,
      metaAt: Height => Option[BlockMeta],
      blockInfoAt: Height => Option[(BlockMeta, Seq[(TxMeta, Transaction)])]
  ): CommonBlocksApi = new CommonBlocksApi {
    private def fixHeight(h: Height) = if (h <= Height(0)) h + blockchain.height else h

    def blocksRange(fromHeight: Height, toHeight: Height): Observable[(BlockMeta, Seq[(TxMeta, Transaction)])] =
      Observable
        .fromIterable(fixHeight(fromHeight) to fixHeight(toHeight))
        .map(Height.apply)
        .map(blockInfoAt)
        .takeWhile(_.isDefined)
        .flatMap(Observable.fromIterable)

    def blocksRange(fromHeight: Height, toHeight: Height, generatorAddress: Address): Observable[(BlockMeta, Seq[(TxMeta, Transaction)])] =
      for {
        height <- Observable.fromIterable(fixHeight(fromHeight) to fixHeight(toHeight)).map(Height.apply)
        meta   <- Observable.fromIterable(metaAt(height)) if meta.header.generator.toAddress == generatorAddress
        block  <- Observable.fromIterable(blockInfoAt(Height(meta.height)))
      } yield block

    def blockDelay(blockId: BlockId, blockNum: Int): Option[Long] =
      blockchain
        .heightOf(blockId)
        .map { maxHeight =>
          val minHeight  = maxHeight - blockNum.max(1)
          val allHeaders = (minHeight to maxHeight).flatMap(h => metaAt(Height(h)))
          val totalPeriod = allHeaders
            .sliding(2)
            .map { pair =>
              pair(1).header.timestamp - pair(0).header.timestamp
            }
            .sum
          totalPeriod / (allHeaders.size - 1).max(1)
        }

    def currentHeight: Height = Height(blockchain.height)

    def currentFinalizedHeight: Height = blockchain.finalizedHeightOrFallback(maxSyncRollbackLength)

    def finalizedHeightAt(at: Height): Option[Height] = Option.when(at < currentHeight) {
      Blockchain.finalizedHeightOrFallback(
        at = at,
        latestFinalized = blockchain.finalizedHeightAt(at),
        maxRollbackLength = maxSyncRollbackLength
      )
    }

    def blockAtHeight(height: Height): Option[(BlockMeta, Seq[(TxMeta, Transaction)])] = blockInfoAt(height)

    def metaAtHeight(height: Height): Option[BlockMeta] = metaAt(height)

    def meta(id: ByteStr): Option[BlockMeta] = blockchain.heightOf(id).flatMap(h => metaAt(Height(h)))

    def metaRange(fromHeight: Height, toHeight: Height): Observable[BlockMeta] =
      for {
        height <- Observable.fromIterable(fixHeight(fromHeight) to fixHeight(toHeight))
        meta   <- Observable.fromIterable(metaAt(Height(height)))
      } yield meta

    def block(blockId: BlockId): Option[(BlockMeta, Seq[(TxMeta, Transaction)])] = blockchain.heightOf(blockId).flatMap(h => blockInfoAt(Height(h)))
  }
}
