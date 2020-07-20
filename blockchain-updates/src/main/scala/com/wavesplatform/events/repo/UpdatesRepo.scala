package com.wavesplatform.events.repo

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.{BlockAppended, MicroBlockAppended}

trait UpdatesRepo {
  def appendMicroBlock(microBlockAppended: MicroBlockAppended): Unit

  def getLiquidState(): Option[(BlockAppended, Seq[MicroBlockAppended])]
  def dropLiquidState(afterId: Option[ByteStr] = None): Unit

  def appendBlock(blockAppended: BlockAppended): Unit

  def removeAfter(height: Int): Unit

  def getForHeight(height: Int): Option[BlockAppended]
}
