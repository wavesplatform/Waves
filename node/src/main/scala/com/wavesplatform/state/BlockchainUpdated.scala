package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.transaction.Asset
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import monix.reactive.Observer

final case class StateUpdate(balances: Seq[(Address, Asset, Long)], leases: Seq[(Address, LeaseBalance)], dataEntries: Seq[(Address, DataEntry[_])]) {
  def isEmpty: Boolean = balances.isEmpty && leases.isEmpty && dataEntries.isEmpty
}

sealed trait BlockchainUpdated
final case class BlockAppended(block: Block, height: Int, blockStateUpdate: StateUpdate, transactionStateUpdates: Seq[StateUpdate])
    extends BlockchainUpdated
final case class MicroBlockAppended(microBlock: MicroBlock,
                                    height: Int,
                                    microBlockStateUpdate: StateUpdate,
                                    transactionStateUpdates: Seq[StateUpdate])
    extends BlockchainUpdated
final case class RollbackCompleted(to: ByteStr, height: Int)           extends BlockchainUpdated
final case class MicroBlockRollbackCompleted(to: ByteStr, height: Int) extends BlockchainUpdated

object BlockchainUpdateNotifier {

  private def stateUpdateFromDiff(blockchain: Blockchain, diff: Diff): StateUpdate = {
    val balances = DiffToStateApplier.balances(blockchain, diff).toSeq.map { case ((address, asset), balance) => (address, asset, balance) }
    val leases   = DiffToStateApplier.leases(blockchain, diff).toSeq
    val dataEntries = diff.accountData.toSeq.flatMap {
      case (address, AccountDataInfo(data)) =>
        data.toSeq.map { case (_, entry) => (address, entry) }
    }
    StateUpdate(balances, leases, dataEntries)
  }

  private def stateUpdatesFromDetailedDiff(blockchain: Blockchain, diff: DetailedDiff): (StateUpdate, Seq[StateUpdate]) = {
    val DetailedDiff(parentDiff, txsDiffs) = diff
    val parentStateUpdate                  = stateUpdateFromDiff(blockchain, parentDiff)

    val (txsStateUpdates, _) = txsDiffs.foldLeft((Seq.empty[StateUpdate], composite(blockchain, parentDiff))) {
      case ((updates, bc), txDiff) => (updates :+ stateUpdateFromDiff(bc, txDiff), composite(bc, txDiff))
    }

    (parentStateUpdate, txsStateUpdates)
  }

  def notifyProcessBlock(events: Option[Observer[BlockchainUpdated]], block: Block, diff: DetailedDiff, blockchain: Blockchain): Unit =
    events foreach { es =>
      val (blockStateUpdate, txsStateUpdates) = stateUpdatesFromDetailedDiff(blockchain, diff)
      es.onNext(BlockAppended(block, blockchain.height + 1, blockStateUpdate, txsStateUpdates))
    }

  def notifyProcessMicroBlock(events: Option[Observer[BlockchainUpdated]], microBlock: MicroBlock, diff: DetailedDiff, blockchain: Blockchain): Unit =
    events foreach { es =>
      val (microBlockStateUpdate, txsStateUpdates) = stateUpdatesFromDetailedDiff(blockchain, diff)
      es.onNext(MicroBlockAppended(microBlock, blockchain.height + 1, microBlockStateUpdate, txsStateUpdates))
    }

  // here height + 1 is not required, because blockchain rollback resets height and ngState no longer affects it
  def notifyRollback(events: Option[Observer[BlockchainUpdated]], blockId: ByteStr, height: Int): Unit =
    events foreach (_.onNext(RollbackCompleted(blockId, height)))

  def notifyMicroBlockRollback(events: Option[Observer[BlockchainUpdated]], toSignature: ByteStr, height: Int): Unit =
    events foreach (_.onNext(MicroBlockRollbackCompleted(toSignature, height + 1)))
}
