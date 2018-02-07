package com.wavesplatform.network

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.HeightInfo
import scorex.block.Block.BlockId
import scorex.block.{Block, BlockHeader, MicroBlock}
import scorex.transaction.History.BlockchainScore
import scorex.transaction.{History, NgHistory}


class TestHistory extends NgHistory {
  def appendId(s: BlockId): Unit = {
    lastBlockIds = s +: lastBlockIds
  }

  private var lastBlockIds = Seq.empty[BlockId] // fresh head
  override def lastBlockIds(howMany: Int): Seq[ByteStr] = lastBlockIds.take(howMany)

  override def microBlock(id: ByteStr): Option[MicroBlock] = ???

  override def bestLastBlockInfo(maxTimestamp: Long): Option[History.BlockMinerInfo] = ???

  override def blockBytes(height: Int): Option[Array[Byte]] = ???

  override def heightOf(blockId: ByteStr): Option[Int] = ???

  override def lastBlockTimestamp(): Option[Long] = ???

  override def blockHeaderAndSizeAt(height: Int): Option[(BlockHeader, Int)] = ???

  override def lastBlockId(): Option[ByteStr] = ???

  override def blockAt(height: Int): Option[Block] = ???

  override def height(): Int = ???

  override def scoreOf(id: ByteStr): Option[BlockchainScore] = ???

  override def synchronizationToken: ReentrantReadWriteLock = ???

  override def debugInfo: HeightInfo = ???
}