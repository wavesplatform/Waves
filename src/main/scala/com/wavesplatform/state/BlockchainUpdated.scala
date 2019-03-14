package com.wavesplatform.state

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import monix.reactive.Observer

final case class StateUpdated(balances: Seq[(Address, Option[AssetId], Long)], leases: Seq[(Address, LeaseBalance)])

sealed trait BlockchainUpdated
final case class BlockAdded(block: Block, height: Int, blockStateUpdate: StateUpdated, transactionsStateUpdates: Seq[StateUpdated])
    extends BlockchainUpdated
final case class MicroBlockAdded(microBlock: MicroBlock,
                                 height: Int,
                                 microBlockStateUpdate: StateUpdated,
                                 transactionsStateUpdates: Seq[StateUpdated])
    extends BlockchainUpdated
final case class RollbackCompleted(to: ByteStr, height: Int) extends BlockchainUpdated

object BlockchainUpdateNotifier {

  private def stateUpdateFromDiff(blockchain: Blockchain, diff: Diff): StateUpdated = {
    val balances = Seq.newBuilder[(Address, Option[AssetId], Long)]
    val leases   = Seq.newBuilder[(Address, LeaseBalance)]

    for ((address, portfolioDiff) <- diff.portfolios) {
      if (portfolioDiff.balance != 0) {
        val wavesBalance = portfolioDiff.balance + blockchain.balance(address, None)
        balances += ((address, None, wavesBalance))
      }

      if (portfolioDiff.lease != LeaseBalance.empty) {
        val updatedLeaseBalance = blockchain.leaseBalance(address).combine(portfolioDiff.lease)
        leases += ((address, updatedLeaseBalance))
      }

      if (portfolioDiff.assets.nonEmpty) {
        for { (k, v) <- portfolioDiff.assets if v != 0 } yield {
          val b = Some(k)
          balances += ((address, b, v + blockchain.balance(address, b)))
        }
      }
    }

    StateUpdated(balances.result(), leases.result())
  }

  private def stateUpdatesFromDetailedDiff(blockchain: Blockchain, diff: DetailedDiff): (StateUpdated, Seq[StateUpdated]) = {
    val (blockDiff, txsDiffs) = diff
    val blockStateUpdate      = stateUpdateFromDiff(blockchain, blockDiff)

    val txsStateUpdates = txsDiffs.foldLeft((Seq.empty[StateUpdated], composite(blockchain, blockDiff)))((acc, txDiff) => {
      val (updates, bc) = acc
      val update        = stateUpdateFromDiff(bc, txDiff)
      (updates :+ update, composite(bc, txDiff))
    })

    (blockStateUpdate, txsStateUpdates._1)
  }

  def notifyProcessBlock(events: Option[Observer[BlockchainUpdated]], block: Block, diff: DetailedDiff, blockchain: Blockchain): Unit =
    events foreach { es =>
      val (blockStateUpdate, txsStateUpdates) = stateUpdatesFromDetailedDiff(blockchain, diff)
      es.onNext(BlockAdded(block, blockchain.height + 1, blockStateUpdate, txsStateUpdates))
    }

  def notifyProcessMicroBlock(events: Option[Observer[BlockchainUpdated]], microBlock: MicroBlock, diff: DetailedDiff, blockchain: Blockchain): Unit =
    events foreach { es =>
      val (microBlockStateUpdate, txsStateUpdates) = stateUpdatesFromDetailedDiff(blockchain, diff)
      es.onNext(MicroBlockAdded(microBlock, blockchain.height, microBlockStateUpdate, txsStateUpdates))
    }

  def notifyRollback(events: Option[Observer[BlockchainUpdated]], blockId: ByteStr, height: Int): Unit =
    events foreach (_.onNext(RollbackCompleted(blockId, height)))
}
