package com.wavesplatform.database

import com.wavesplatform.block.{Block, BlockSnapshot}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Height, StateSnapshot}

trait Storage {
  def append(
      snapshot: StateSnapshot,
      carryFee: Long,
      totalFee: Long,
      reward: Option[Long],
      hitSource: ByteStr,
      computedBlockStateHash: ByteStr,
      block: Block
  ): Unit
  def lastBlock: Option[Block]
  def rollbackTo(height: Height): Either[String, Seq[(Block, ByteStr, Option[BlockSnapshot])]]
  def safeRollbackHeight: Height
}
