package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import monix.reactive.subjects.PublishSubject

final case class StateUpdate(portfolios: Map[Address, Portfolio])

trait StateUpdateEvent
final case class BlockAddEvent(b: Block, blockStateUpdate: StateUpdate, transactionsStateUpdates: Seq[StateUpdate]) extends StateUpdateEvent
final case class MicroBlockEvent(b: MicroBlock, microBlockStateUpdate: StateUpdate, transactionsStateUpdates: Seq[StateUpdate])
    extends StateUpdateEvent
final case class RollbackEvent(eventId: ByteStr) extends StateUpdateEvent

class StateUpdateProcessor(events: PublishSubject[StateUpdateEvent]) {
  def onProcessBlock(b: Block, diff: DetailedDiff, bc: Blockchain): Unit = ???

  def onProcessMicroblock(mb: MicroBlock, diff: DetailedDiff, bc: Blockchain): Unit = ???

  def onRollback(blockId: ByteStr): Unit = ???
}
