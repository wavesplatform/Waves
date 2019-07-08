package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset
import monix.reactive.Observer

import scala.collection.mutable.ArrayBuffer

final case class StateUpdate(balances: Seq[(Address, Asset, Long)], leases: Seq[(Address, LeaseBalance)], dataEntries: Seq[(Address, DataEntry[_])]) {
  def isEmpty: Boolean = balances.isEmpty && leases.isEmpty && dataEntries.isEmpty
}

sealed trait BlockchainUpdated
final case class BlockAppended(block: Block,
                               height: Int,
                               blockStateUpdate: StateUpdate,
                               transactionStateUpdates: Seq[StateUpdate],
                               transactionIds: Seq[ByteStr])
    extends BlockchainUpdated
final case class MicroBlockAppended(microBlock: MicroBlock,
                                    height: Int,
                                    microBlockStateUpdate: StateUpdate,
                                    transactionStateUpdates: Seq[StateUpdate],
                                    transactionIds: Seq[ByteStr])
    extends BlockchainUpdated
final case class RollbackCompleted(to: ByteStr, height: Int)           extends BlockchainUpdated
final case class MicroBlockRollbackCompleted(to: ByteStr, height: Int) extends BlockchainUpdated

object BlockchainUpdateNotifier {

  private def stateUpdateFromDiff(blockchain: Blockchain, diff: Diff): StateUpdate = {
    val PortfolioUpdates(updatedBalances, updatedLeases) = DiffToStateApplier.portfolios(blockchain, diff)

    val balances = ArrayBuffer.empty[(Address, Asset, Long)]
    for ((address, assetMap) <- updatedBalances; (asset, balance) <- assetMap) balances += ((address, asset, balance))

    val dataEntries = diff.accountData.toSeq.flatMap {
      case (address, AccountDataInfo(data)) =>
        data.toSeq.map { case (_, entry) => (address, entry) }
    }
    StateUpdate(balances, updatedLeases.toSeq, dataEntries)
  }

  private def stateUpdatesFromDetailedDiff(blockchain: Blockchain, diff: DetailedDiff): (StateUpdate, Seq[StateUpdate], Seq[ByteStr]) = {
    val DetailedDiff(parentDiff, txsDiffs) = diff
    val parentStateUpdate                  = stateUpdateFromDiff(blockchain, parentDiff)

    val (txsStateUpdates, _) = txsDiffs.foldLeft((Seq.empty[StateUpdate], CompositeBlockchain(blockchain, Some(parentDiff)))) {
      case ((updates, bc), txDiff) => (updates :+ stateUpdateFromDiff(bc, txDiff), CompositeBlockchain(bc, Some(txDiff)))
    }

    val txIds = txsDiffs.flatMap(txDiff => txDiff.transactions.keys.toSeq)

    (parentStateUpdate, txsStateUpdates, txIds)
  }

  def notifyProcessBlock(events: Observer[BlockchainUpdated], block: Block, diff: DetailedDiff, blockchain: Blockchain): Unit = {
    val (blockStateUpdate, txsStateUpdates, txIds) = stateUpdatesFromDetailedDiff(blockchain, diff)
    events.onNext(BlockAppended(block, blockchain.height + 1, blockStateUpdate, txsStateUpdates, txIds))
  }

  def notifyProcessMicroBlock(events: Observer[BlockchainUpdated], microBlock: MicroBlock, diff: DetailedDiff, blockchain: Blockchain): Unit = {
    val (microBlockStateUpdate, txsStateUpdates, txIds) = stateUpdatesFromDetailedDiff(blockchain, diff)
    events.onNext(MicroBlockAppended(microBlock, blockchain.height + 1, microBlockStateUpdate, txsStateUpdates, txIds))
  }

  // here height + 1 is not required, because blockchain rollback resets height and ngState no longer affects it
  def notifyRollback(events: Observer[BlockchainUpdated], blockId: ByteStr, height: Int): Unit =
    events.onNext(RollbackCompleted(blockId, height))

  def notifyMicroBlockRollback(events: Observer[BlockchainUpdated], toSignature: ByteStr, height: Int): Unit =
    events.onNext(MicroBlockRollbackCompleted(toSignature, height + 1))
}
