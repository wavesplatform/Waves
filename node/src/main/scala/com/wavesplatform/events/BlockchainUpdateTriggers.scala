package com.wavesplatform.events

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Blockchain, StateSnapshot}

trait BlockchainUpdateTriggers {
  def onProcessBlock(
      block: Block,
      snapshot: StateSnapshot,
      reward: Option[Long],
      hitSource: ByteStr,
      blockchainBeforeWithReward: Blockchain
  ): Unit
  def onProcessMicroBlock(
      microBlock: MicroBlock,
      snapshot: StateSnapshot,
      blockchainBeforeWithReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): Unit
  def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit
  def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit
}

object BlockchainUpdateTriggers {
  def noop: BlockchainUpdateTriggers = new BlockchainUpdateTriggers {
    override def onProcessBlock(
        block: Block,
        snapshot: StateSnapshot,
        reward: Option[Long],
        hitSource: ByteStr,
        blockchainBeforeWithReward: Blockchain
    ): Unit = {}
    override def onProcessMicroBlock(
        microBlock: MicroBlock,
        snapshot: StateSnapshot,
        blockchainBeforeWithReward: Blockchain,
        totalBlockId: ByteStr,
        totalTransactionsRoot: ByteStr
    ): Unit = {}
    override def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit = {}
    override def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit      = {}
  }

  def combined(triggers: => Seq[BlockchainUpdateTriggers]): BlockchainUpdateTriggers = new BlockchainUpdateTriggers {
    override def onProcessBlock(
        block: Block,
        snapshot: StateSnapshot,
        reward: Option[Long],
        hitSource: ByteStr,
        blockchainBeforeWithReward: Blockchain
    ): Unit =
      triggers.foreach(_.onProcessBlock(block, snapshot, reward, hitSource, blockchainBeforeWithReward))

    override def onProcessMicroBlock(
        microBlock: MicroBlock,
        snapshot: StateSnapshot,
        blockchainBeforeWithReward: Blockchain,
        totalBlockId: ByteStr,
        totalTransactionsRoot: ByteStr
    ): Unit =
      triggers.foreach(_.onProcessMicroBlock(microBlock, snapshot, blockchainBeforeWithReward, totalBlockId, totalTransactionsRoot))

    override def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit =
      triggers.foreach(_.onRollback(blockchainBefore, toBlockId, toHeight))

    override def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit =
      triggers.foreach(_.onMicroBlockRollback(blockchainBefore, toBlockId))
  }
}
