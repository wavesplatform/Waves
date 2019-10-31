package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import monix.eval.Task
import monix.reactive.Observable

private[api] class CommonBlocksApi(blockchain: Blockchain, blockAt: Int => Task[Block]) {
  private def fixHeight(h: Int) = if (h <= 0) blockchain.height + h else h
  private def heightOf(id: ByteStr): Task[Int] =
    Task(blockchain.heightOf(id).getOrElse(throw new NoSuchElementException(s"Could not find block $id")))

  def blocksRange(fromHeight: Int, toHeight: Int): Observable[(Block, Int)] =
    blockHeadersRange(fromHeight, toHeight)
      .mapEval { case (_, _, _, _, height) => blockAt(height).map(_ -> height) }

  def blocksRange(fromHeight: Int, toHeight: Int, generatorAddress: Address): Observable[(Block, Int)] =
    blockHeadersRange(fromHeight, toHeight)
      .collect { case (header, _, _, _, height) if header.generator.toAddress == generatorAddress => height }
      .mapEval(height => blockAt(height).map(_ -> height))

  def childBlock(blockId: BlockId): Task[(Block, Int)] =
    for {
      height <- heightOf(blockId)
      block  <- blockAt(height + 1)
    } yield (block, height + 1)

  def calcBlocksDelay(blockId: BlockId, blockNum: Int): Task[Option[Long]] = Task.raiseError(new NotImplementedError)

  def blockHeight(blockId: BlockId): Option[Int] = blockchain.heightOf(blockId)

  def currentHeight(): Int = blockchain.height

  def blockAtHeight(height: Int): Task[Block] = blockAt(height)

  def blockHeaderAtHeight(height: Int): Task[Option[(BlockHeader, Int, Int, ByteStr)]] = Task(blockchain.blockHeaderAndSize(height))

  def blockHeadersRange(fromHeight: Int, toHeight: Int): Observable[(BlockHeader, Int, Int, ByteStr, Int)] = {
    Observable
      .fromIterable(fixHeight(fromHeight) to fixHeight(toHeight))
      .map(height => (height, blockchain.blockHeaderAndSize(height)))
      .collect { case (height, Some((header, size, transactionCount, signature))) => (header, size, transactionCount, signature, height) }
  }

  def lastBlock(): Option[Block] = blockchain.lastBlock

  def lastBlockHeaderAndSize(): Option[(Block, Int)] = blockchain.lastBlock.map(block => (block, block.bytes().length))

  def blockBySignature(blockId: BlockId): Task[Block] =
    for {
      height <- heightOf(blockId)
      block  <- blockAt(height)
    } yield block
}
