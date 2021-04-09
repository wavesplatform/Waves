package com.wavesplatform.database

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Diff

trait Storage {
  def append(diff: Diff, carryFee: Long, totalFee: Long, reward: Option[Long], hitSource: ByteStr, block: Block): Unit
  def lastBlock: Option[Block]
  def rollbackTo(height: Int): Either[String, Seq[(Block, ByteStr)]]
  def safeRollbackHeight: Int
}
