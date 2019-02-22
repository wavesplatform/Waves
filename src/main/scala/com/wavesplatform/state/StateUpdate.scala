package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff

import monix.reactive.subjects.PublishSubject

final case class StateUpdate(portfolios: Map[Address, Portfolio])

trait Event
final case class Block(b: Block, blockStateUpdate: StateUpdate, transactionsStateUpdate: Seq[StateUpdate])                extends Event
final case class MicroBlock(b: MicroBlock, microBlockStateUpdate: StateUpdate, transactionsStateUpdate: Seq[StateUpdate]) extends Event
final case class Rollback(eventId: ByteStr)                                                                               extends Event

class StateUpdateProcessor(events: PublishSubject[Event]) {
  def onProcessBlock(b: Block, diff: DetailedDiff, bc: Blockchain): Unit = ???

  def onProcessMicroblock(mb: MicroBlock, diff: DetailedDiff, bc: Blockchain): Unit = ???

  def onRollback(blockId: ByteStr): Unit = ???
}
