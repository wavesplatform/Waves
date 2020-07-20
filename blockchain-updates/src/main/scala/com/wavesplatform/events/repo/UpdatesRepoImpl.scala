package com.wavesplatform.events.repo

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.{BlockAppended, MicroBlockAppended}
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

class UpdatesRepoImpl(db: DB) extends UpdatesRepo with ScorexLogging {
  override def appendMicroBlock(microBlockAppended: MicroBlockAppended): Unit = ???

  override def getLiquidState(): Option[(BlockAppended, Seq[MicroBlockAppended])] = None

  override def dropLiquidState(afterId: Option[ByteStr]): Unit = {}

  override def removeAfter(height: Int): Unit = ???

  override def appendBlock(blockAppended: BlockAppended): Unit = ???

  override def getForHeight(height: Int): Option[BlockAppended] = ???
}
