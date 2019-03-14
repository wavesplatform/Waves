package com.wavesplatform.api.common
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.{ApiError, BlockDoesNotExist, CustomValidationError}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.block.VanillaBlock
import com.wavesplatform.state.Blockchain
import monix.reactive.Observable

private[api] class CommonBlocksApi(blockchain: Blockchain) {
  def blocksByAddress(address: Address, fromHeight: Int, toHeight: Int): Observable[(VanillaBlock, Int)] = {
    Observable
      .fromIterable(fromHeight to toHeight)
      .map(height => (blockchain.blockAt(height), height))
      .collect { case (Some(block), height) if block.signerData.generator.toAddress == address => (block, height) }
  }

  def childBlock(blockId: BlockId): Option[VanillaBlock] = {
    val childBlock = for {
      height     <- blockchain.heightOf(blockId)
      childBlock <- blockchain.blockAt(height + 1)
    } yield childBlock

    childBlock
  }

  def calcBlocksDelay(blockId: BlockId, blockNum: Int): Either[ApiError, Long] = {
    getBlockById(blockId).flatMap { block =>
      blockchain
        .parent(block, blockNum)
        .map(parent => (block.timestamp - parent.timestamp) / blockNum)
        .toRight(CustomValidationError(s"Cannot go $blockNum blocks back"))
    }
  }

  def blockHeight(blockId: BlockId): Option[Int] = {
    blockchain.heightOf(blockId)
  }

  def currentHeight(): Int = {
    blockchain.height
  }

  def blockAtHeight(height: Int): Option[VanillaBlock] = {
    blockchain.blockAt(height)
  }

  def blockHeaderAtHeight(height: Int): Option[(BlockHeader, Int)] = {
    blockchain.blockHeaderAndSize(height)
  }

  def blocksRange(fromHeight: Int, toHeight: Int): Observable[(Int, Block)] = {
    Observable
      .fromIterable(fromHeight to toHeight)
      .map(height => (height, blockchain.blockAt(height)))
      .collect { case (height, Some(block)) => (height, block) }
  }

  def blockHeadersRange(fromHeight: Int, toHeight: Int): Observable[(Int, BlockHeader, Int)] = {
    Observable
      .fromIterable(fromHeight to toHeight)
      .map(height => (height, blockchain.blockHeaderAndSize(height)))
      .collect { case (height, Some((header, size))) => (height, header, size) }
  }

  def lastBlock(): Option[VanillaBlock] = {
    blockchain.lastBlock
  }

  def lastBlockHeader(): Option[(VanillaBlock, Int)] = {
    blockchain.lastBlockHeaderAndSize
  }

  def firstBlock(): VanillaBlock = {
    blockchain.genesis
  }

  def blockBySignature(blockId: BlockId): Either[ApiError, VanillaBlock] = {
    getBlockById(blockId)
  }

  private[this] def getBlockById(signature: ByteStr): Either[ApiError, VanillaBlock] = {
    blockchain
      .blockById(signature)
      .toRight(BlockDoesNotExist)
  }
}
