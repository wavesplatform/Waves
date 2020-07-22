package com.wavesplatform.events.repo

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.{BlockAppended, MicroBlockAppended}

import scala.util.Try

trait UpdatesRepo {
  // infallible operations
  def getLiquidState(): Option[LiquidState]

  def dropLiquidState(afterId: Option[ByteStr] = None): Unit

  // fallible operations
  def appendMicroBlock(microBlockAppended: MicroBlockAppended): Try[Unit]

  def appendBlock(blockAppended: BlockAppended): Try[Unit]

  def removeAfter(height: Int): Try[Unit]

  def getForHeight(height: Int): Try[Option[BlockAppended]]

  // inclusive from both sides
  def getRange(from: Int, to: Int): Try[Seq[BlockAppended]]
}
