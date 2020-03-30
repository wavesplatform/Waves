package com.wavesplatform.state

import cats.kernel.Monoid
import cats.syntax.semigroup._
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr

import scala.annotation.tailrec

sealed abstract class NgState(
    val totalDiff: Diff,
    val fee: Long,
    val carry: Long,
    val timestamp: Long
) {
  def totalBlock: Block
  def keyBlock: NgState.KeyBlock

  def append(microBlock: MicroBlock, diff: Diff, microBlockFee: Long, microBlockCarry: Long, timestamp: Long): NgState =
    new NgState.LiquidBlock(this, microBlock, totalDiff |+| diff, this.carry + microBlockCarry, this.fee + microBlockFee, timestamp)
}

object NgState {
  @tailrec private def findMatchingNGS(startFrom: NgState, maxTimestamp: Long): NgState = startFrom match {
    case ngs if ngs.timestamp <= maxTimestamp => ngs
    case kb: KeyBlock                         => kb
    case lb: LiquidBlock                      => findMatchingNGS(lb.parent, maxTimestamp)
  }

  @tailrec private def collectDiscardedMicroBlocks(
      totalBlockId: ByteStr,
      startWith: NgState,
      accumulator: List[MicroBlock]
  ): Option[(NgState, Seq[MicroBlock])] = startWith match {
    case ngs if ngs.totalBlock.id() == totalBlockId => Some((ngs, accumulator))
    case _: KeyBlock                                => None
    case lb: LiquidBlock                            => collectDiscardedMicroBlocks(totalBlockId, lb.parent, lb.microBlock :: accumulator)
  }

  @tailrec private def collectIds(startFrom: NgState, otherIds: Seq[ByteStr]): Seq[ByteStr] = startFrom match {
    case _: KeyBlock     => otherIds
    case lb: LiquidBlock => collectIds(lb.parent, lb.microBlock.totalResBlockSig +: otherIds)
  }

  implicit class NgStateExt(val ngs: NgState) extends AnyVal {
    def forId(str: ByteStr): Option[NgState] = forge(str).map(_._1)
    def contains(blockId: ByteStr): Boolean  = ngs.forId(blockId).isDefined
    def withExpiredLeases: Diff =
      ngs.totalDiff |+| Monoid.combineAll(ngs.keyBlock.leasesToCancel.collect {
        case (id, diff) if ngs.totalDiff.leaseState.getOrElse(id, true) => diff
      })
    def microBlockForId(id: ByteStr): Option[MicroBlock] = ngs.forId(id).collect {
      case lb: LiquidBlock => lb.microBlock
    }

    final def microBlockIds: Seq[ByteStr] = collectIds(ngs, Seq.empty)

    def bestLastBlockInfo(maxTimestamp: Long): BlockMinerInfo = {
      val matchingBlock = findMatchingNGS(ngs, maxTimestamp).totalBlock
      BlockMinerInfo(matchingBlock.header.baseTarget, matchingBlock.header.generationSignature, matchingBlock.header.timestamp, matchingBlock.id())
    }

    def forge(totalBlockId: ByteStr): Option[(NgState, Seq[MicroBlock])] =
      collectDiscardedMicroBlocks(totalBlockId, ngs, Nil)
  }

  case class KeyBlock(
      diff: Diff,
      block: Block,
      _fee: Long,
      _carry: Long,
      approvedFeatures: Set[Short],
      reward: Option[Long],
      hitSource: ByteStr,
      leasesToCancel: Map[ByteStr, Diff]
  ) extends NgState(diff, _fee, _carry, block.header.timestamp) {
    override val keyBlock: KeyBlock = this
    override val totalBlock: Block  = block
  }

  class LiquidBlock private[NgState] (val parent: NgState, val microBlock: MicroBlock, val diff: Diff, _fee: Long, _carry: Long, _timestamp: Long)
      extends NgState(diff, _fee, _carry, _timestamp) {
    override val keyBlock: KeyBlock     = parent.keyBlock
    override lazy val totalBlock: Block = parent.totalBlock.appendTransactions(microBlock.transactionData, microBlock.totalResBlockSig)
  }
}
