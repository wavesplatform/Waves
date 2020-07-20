package com.wavesplatform.events

import com.wavesplatform.common.state.ByteStr

trait UpdatesRepo {
  def appendMicroBlock(microBlockAppended: MicroBlockAppended): Unit

  def getLiquidState(): (BlockAppended, Seq[MicroBlockAppended])
  def dropLiquidState(afterId: Option[ByteStr] = None): Unit

  def appendBlock(blockAppended: BlockAppended): Unit

  def removeAfter(height: Int): Unit
}
