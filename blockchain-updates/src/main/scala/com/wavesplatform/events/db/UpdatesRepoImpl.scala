package com.wavesplatform.events.db

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.{BlockAppended, MicroBlockAppended, UpdatesRepo}
import com.wavesplatform.utils.ScorexLogging

class UpdatesRepoImpl extends UpdatesRepo with ScorexLogging {
  private[this] var blocks = scala.collection.mutable.ListBuffer.empty[BlockAppended]

  override def appendMicroBlock(microBlockAppended: MicroBlockAppended): Unit = ???

  override def getLiquidState(): Option[(BlockAppended, Seq[MicroBlockAppended])] = None

  override def dropLiquidState(afterId: Option[ByteStr]): Unit = {}

  override def removeAfter(height: Int): Unit = ???

  override def appendBlock(blockAppended: BlockAppended): Unit = {
    log.info(s"Appended block ${blockAppended.toHeight}")
    blocks += blockAppended
    if (blocks.length > 100) {
      Thread.sleep(10000) // after first 1000 blocks throttle Appender
    }
  }

  override def getForHeight(height: Int): Option[BlockAppended] = {
    log.info(s"getForHeight $height")
    val b = blocks.find(_.toHeight == height)
    log.info(s"returning $b")
    b
  }
}
