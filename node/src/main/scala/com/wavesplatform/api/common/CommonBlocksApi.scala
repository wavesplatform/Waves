package com.wavesplatform.api.common

import com.wavesplatform.api.http.ApiError
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.block.VanillaBlock
import com.wavesplatform.state.Blockchain
import monix.reactive.Observable

private[api] class CommonBlocksApi(blockchain: Blockchain) {
  def blocksRange(fromHeight: Int, toHeight: Int): Observable[(VanillaBlock, Int)] = {
    def fixHeight(h: Int) = if (h <= 0) blockchain.height + h else h

    Observable
      .fromIterable(fixHeight(fromHeight) to fixHeight(toHeight))
      .map(height => (blockchain.blockAt(height), height))
      .collect { case (Some(block), height) => (block, height) }
  }

  def childBlock(blockId: BlockId): Option[(VanillaBlock, Int)] = {
    for {
      height     <- blockchain.heightOf(blockId)
      childBlock <- blockchain.blockAt(height + 1)
    } yield (childBlock, height + 1)
  }

  def calcBlocksDelay(blockId: BlockId, blockNum: Int): Either[ApiError, Long] = {
    getBlockById(blockId).flatMap { block =>
      blockchain
        .parentHeader(block, blockNum)
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

  def blockHeadersRange(fromHeight: Int, toHeight: Int): Observable[(BlockHeader, Int, Int)] = {
    Observable
      .fromIterable(fromHeight to toHeight)
      .map(height => (height, blockchain.blockHeaderAndSize(height)))
      .collect { case (height, Some((header, size))) => (header, size, height) }
  }

  def lastBlock(): Option[VanillaBlock] = {
    blockchain.lastBlock
  }

  def lastBlockHeaderAndSize(): Option[(VanillaBlock, Int)] = {
    blockchain.lastBlock.map(block => (block, block.bytes().length))
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
