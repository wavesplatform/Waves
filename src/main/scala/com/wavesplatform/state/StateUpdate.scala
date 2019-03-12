package com.wavesplatform.state

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import monix.reactive.Observer

final case class StateUpdate(balances: Map[(Address, Option[AssetId]), Long], leases: Map[Address, LeaseBalance])

sealed trait StateUpdateEvent
final case class BlockEvent(b: Block, height: Int, blockStateUpdate: StateUpdate, transactionsStateUpdates: Seq[StateUpdate])
    extends StateUpdateEvent
final case class MicroBlockEvent(b: MicroBlock, height: Int, microBlockStateUpdate: StateUpdate, transactionsStateUpdates: Seq[StateUpdate])
    extends StateUpdateEvent
final case class RollbackEvent(eventId: ByteStr, height: Int) extends StateUpdateEvent

object StateUpdateProcessor {

  private def stateUpdateFromDiff(blockchain: Blockchain, diff: Diff): StateUpdate = {
    val balances = Map.newBuilder[(Address, Option[AssetId]), Long]
    val leases   = Map.newBuilder[Address, LeaseBalance]

    for ((address, portfolioDiff) <- diff.portfolios) {
      if (portfolioDiff.balance != 0) {
        val wavesBalance = portfolioDiff.balance + blockchain.balance(address, None)
        balances += (address, None) -> wavesBalance
      }

      if (portfolioDiff.lease != LeaseBalance.empty) {
        val updatedLeaseBalance = blockchain.leaseBalance(address).combine(portfolioDiff.lease)
        leases += address -> updatedLeaseBalance
      }

      if (portfolioDiff.assets.nonEmpty) {
        for { (k, v) <- portfolioDiff.assets if v != 0 } yield {
          val b = Some(k)
          balances += (address, b) -> (v + blockchain.balance(address, b))
        }
      }
    }

    StateUpdate(balances.result(), leases.result())
  }

  private def stateUpdatesFromDetailedDiff(blockchain: Blockchain, diff: DetailedDiff): (StateUpdate, Seq[StateUpdate]) = {
    val (blockDiff, txsDiffs) = diff
    val blockStateUpdate      = stateUpdateFromDiff(blockchain, blockDiff)

    val txsStateUpdates = txsDiffs.foldLeft((Seq.empty[StateUpdate], composite(blockchain, blockDiff)))((acc, txDiff) => {
      val (updates, bc) = acc
      val update        = stateUpdateFromDiff(bc, txDiff)
      (updates :+ update, composite(bc, txDiff))
    })

    (blockStateUpdate, txsStateUpdates._1)
  }

  def onProcessBlock(events: Option[Observer[StateUpdateEvent]], block: Block, diff: DetailedDiff, blockchain: Blockchain): Unit =
    events foreach { es =>
      val (blockStateUpdate, txsStateUpdates) = stateUpdatesFromDetailedDiff(blockchain, diff)
      es.onNext(BlockEvent(block, blockchain.height + 1, blockStateUpdate, txsStateUpdates))
    }

  def onProcessMicroBlock(events: Option[Observer[StateUpdateEvent]], microBlock: MicroBlock, diff: DetailedDiff, blockchain: Blockchain): Unit =
    events foreach { es =>
      val (microBlockStateUpdate, txsStateUpdates) = stateUpdatesFromDetailedDiff(blockchain, diff)
      es.onNext(MicroBlockEvent(microBlock, blockchain.height, microBlockStateUpdate, txsStateUpdates))
    }

  def onRollback(events: Option[Observer[StateUpdateEvent]], blockId: ByteStr, height: Int): Unit =
    events foreach (_.onNext(RollbackEvent(blockId, height)))
}
