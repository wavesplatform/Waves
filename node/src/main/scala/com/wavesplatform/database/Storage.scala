package com.wavesplatform.database

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{GeneratorBalances, StateSnapshot}
import com.wavesplatform.transaction.DiscardedBlocks

trait Storage {
  def append(
      snapshot: StateSnapshot,
      carryFee: Long,
      totalFee: Long,
      reward: Option[Long],
      hitSource: ByteStr,
      computedBlockStateHash: ByteStr,
      block: Block,
      generatorBalances: GeneratorBalances
  ): Unit
  def lastBlock: Option[Block]
  def rollbackTo(height: Int): Either[String, DiscardedBlocks]
  def safeRollbackHeight: Int
}
